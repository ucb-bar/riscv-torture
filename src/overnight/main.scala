package torture
package overnight

import java.net.InetAddress
import java.util.Properties._
import javax.mail._
import javax.mail.internet._
import scala.collection.JavaConversions._
import scala.sys.process._
import scalax.file.Path
import Path._
import scalax.file.PathSet
import scalax.file.FileSystem
import scopt.OptionParser
import java.io.File

case class Options(var timeToRun: Option[Int] = None,
  var emailAddress: Option[String] = None,
  var errorThreshold: Option[Int] = None,
  var cSimPath: Option[String] = None,
  var rtlSimPath: Option[String] = None,
  var permDir: Option[String] = None,
  var gitCommit: Option[String] = None,
  var confFileName: Option[String] = None)

object Overnight extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("overnight/run") {
      opt("C", "config", "<file>", "config file", {s: String => opts.confFileName = Some(s)})
      opt("p", "permdir", "<dir>", "dir to store failing tests", {s: String => opts.permDir = Some(s)})
      opt("c", "csim", "<file>", "C simulator", {s: String => opts.cSimPath = Some(s)})
      opt("r", "rtlsim", "<file>", "RTL simulator", {s: String => opts.rtlSimPath = Some(s)})
      opt("e", "email", "<address>", "email to report to", {s: String => opts.emailAddress = Some(s)})
      opt("g", "gitcommit", "<git commit>", "Git commit to check out", {s: String => opts.gitCommit = Some(s)})
      opt("t", "threshold", "<count>", "number of failures to trigger email", {i: String => opts.errorThreshold = Some(i.toInt)})
      opt("m", "minutes", "<int>", "number of minutes to run tests", {i: String => opts.timeToRun = Some(i.toInt)})
    }
    if (parser.parse(args)) {
      val confFileName = opts.confFileName.getOrElse("config")
      val permDir      = opts.permDir.getOrElse("output/failedtests")
      val thresh       = opts.errorThreshold.getOrElse(5)
      val minutes      = opts.timeToRun.getOrElse(1)
      val startTime = System.currentTimeMillis
      var endTime = startTime + minutes*60*1000
      var errCount = 0

      val (cSim, rtlSim) = gitCheckout(opts.cSimPath.getOrElse(""), opts.rtlSimPath.getOrElse(""), opts.gitCommit.getOrElse(""))
      while(System.currentTimeMillis < endTime) {
        val baseName = "test_" + System.currentTimeMillis
        val newAsmName = generator.Generator.generate(confFileName, baseName)
        val (failed, test) = testrun.TestRunner.testrun( Some(newAsmName), cSim, rtlSim, Some(true), Some(true), Some(confFileName)) 
        if(failed) {
          errCount += 1
          test foreach { t =>
            println(t)
            println(t.last)
            val permFiles:PathSet[Path] = Path(t.init:_*) * (t.last + "*")
            println(permFiles.mkString)
            permFiles.foreach( f => f.copyTo( permDir / f.name))
          }
        } 
        test foreach { t =>
          val targetFiles:PathSet[Path] = Path(t.init:_*) * (baseName+"*")
          targetFiles.foreach(_.delete())
        }
        if(errCount == thresh) {
          println("////////////////////////////////////////////////////////////////")
          println("//  Aborting test runs due to error threshold being exceeded  //")
          println("////////////////////////////////////////////////////////////////")
          endTime = 0
        }
      }
      val permPath: Path = permDir
      opts.emailAddress foreach { addr =>
        val properties = System.getProperties
        properties.put("mail.smtp.host", "localhost")
        val hostname = InetAddress.getLocalHost().getHostName()
        val session = Session.getDefaultInstance(properties)
        val message = new MimeMessage(session)
        message.setFrom(new InternetAddress("torture@"+hostname+".millennium.berkeley.edu"))
        message.setRecipients(Message.RecipientType.TO, addr)
        message.setText( "Run complete with " + errCount + " errors. Failing tests put in " +  permPath.toAbsolute.path )
        message.setSubject("Run complete on " + hostname)
        println("////////////////////////////////////////////////////////////////")
        println("//  Sending " + message + " to " + addr)
        println("////////////////////////////////////////////////////////////////")
        Transport.send(message)
      }
      println("////////////////////////////////////////////////////////////////")
      println("//  Testing complete with " + errCount + " errors.")
      println("//  Failing tests put in " +  permPath.toAbsolute.path)
      println("////////////////////////////////////////////////////////////////")
    }
  }

  def gitCheckout(cDir: String, rtlDir: String, commit: String): (Option[String], Option[String]) =
  {
    def compileSim(simname: String, simDir: String, bool: Boolean): Option[String] =
    {
      var simFile = ""
      if (simname=="c") simFile = simDir+"/emulator"
      if (simname=="r") simFile = simDir+"/simv" 
      if (simFile=="") return None
      val workDir = new File(simDir)
      val simPath: Path = simFile
      if (bool)
      {
        Process("make -j", workDir).!
        if (!simPath.exists) Process("make -j", workDir).!
        return Some(simFile)
      } else return None
    }
        
    val rBool = (rtlDir != "")
    val cBool = (cDir != "")
    var rocketDir = ""

    if (cBool)
    {
      rocketDir = cDir.substring(0,cDir.length-18) // String ends with /emulator/emulator
    }
    if (rBool)
    {
      rocketDir = rtlDir.substring(0,rtlDir.length-36) //String ends with /vlsi-generic/build/vcs-sim-rtl/simv
    }

    var cSim: Option[String] = None
    var rSim: Option[String] = None

    if (commit == "")
    {
      if (cBool) cSim = Some(cDir)
      if (rBool) rSim = Some(rtlDir)
      return (cSim, rSim)
    }
    if (rocketDir != "")
    {
      val tmpRocketDir = "../rocket_"+commit
      val tmpRocketPath: Path = tmpRocketDir
      val copycmd = "cp -r " + rocketDir + " " + tmpRocketDir
      if (!tmpRocketPath.exists)
      {
        println(copycmd)
        val copyexit = copycmd.!

        val cmd = "git checkout " + commit
        val workDir = new File(tmpRocketDir)
        println(cmd + " run in " + tmpRocketDir)
        val proc = Process(cmd, workDir)
        val output = proc.!!
        println(output)

        val cSimDir = tmpRocketDir+"/emulator"
        val cWorkDir = new File(cSimDir)
        println(Process("make clean", cWorkDir).!!)
        val rSimDir = tmpRocketDir + "/vlsi-generic/build/vcs-sim-rtl"
        val rWorkDir = new File(rSimDir)
        println(Process("make clean", rWorkDir).!!)

        cSim = compileSim("c", cSimDir, cBool)
        rSim = compileSim("r", rSimDir, rBool)
        (cSim, rSim)
      } else {
        val csimpath: Path = tmpRocketDir+"/emulator/emulator"
        val rsimpath: Path = tmpRocketDir+"/vlsi-generic/build/vcs-sim-rtl/simv"
        cSim = compileSim("c", tmpRocketDir+"/emulator", cBool)
        rSim = compileSim("r", tmpRocketDir+"/vlsi-generic/build/vcs-sim-rtl", rBool)
        (cSim, rSim)
      }
    } else {
      (cSim, rSim)
    }
  }
}

