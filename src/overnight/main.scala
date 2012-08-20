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
import java.util.Properties
import java.io.FileInputStream

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
      val config = new Properties()
      val configin = new FileInputStream(confFileName)
      config.load(configin)
      configin.close()

      val errors  = config.getProperty("torture.overnight.errors","1").toInt
      val runtime = config.getProperty("torture.overnight.minutes","1").toInt
      val outdir  = config.getProperty("torture.overnight.outdir","output/failedtests")
      val email   = config.getProperty("torture.overnight.email","your@email.address")

      val permDir      = opts.permDir.getOrElse(outdir)
      val thresh       = opts.errorThreshold.getOrElse(errors)
      val minutes      = opts.timeToRun.getOrElse(runtime)
      val address = opts.emailAddress.getOrElse(email)
      val startTime = System.currentTimeMillis
      var endTime = startTime + minutes*60*1000
      var errCount = 0

      val (cSim, rtlSim) = checkoutRocket(opts.cSimPath.getOrElse(""), opts.rtlSimPath.getOrElse(""), opts.gitCommit.getOrElse("none"))
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
            val statFile: Path = Path(t.init:_*) / (baseName+".stats")
            println(permFiles.mkString)
            println(statFile)
            permFiles.foreach( f => f.copyTo( permDir / f.name))
            statFile.copyTo(permDir / statFile.name, replaceExisting=true)
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
      if (address != "your@email.address")
      {
        Some(address) foreach { addr =>
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
  }

  def checkoutRocket(cPath: String, rPath: String, commit: String): (Option[String],Option[String]) =
  {
    var cSim: Option[String] = None
    var rSim: Option[String] = None
    val cmmt = commit.toUpperCase
    if (cPath != "") cSim = Some(cPath)
    if (rPath != "") rSim = Some(rPath)
    if (cmmt == "NONE") return (cSim, rSim)

    var rocketDir = ""
    if (cPath != "") rocketDir = cPath.substring(0,cPath.length-18)
    if (rPath != "") rocketDir = rPath.substring(0,rPath.length-36)
    val rocketPath: Path = rocketDir
    val destPath: Path = (rocketPath / Path("..") / Path("rocket_"+cmmt))
    val emPath: Path = destPath / Path("emulator")
    val vcsrelPath: Path = "vlsi-generic/build/vcs-sim-rtl"
    val vcsPath: Path = destPath / vcsrelPath
    
    if (!destPath.exists)
    {
      FileOperations.gitcheckout(rocketPath, destPath, cmmt)
      println("Doing make clean in " + emPath.toAbsolute.normalize.path)
      FileOperations.clean(emPath)
      println("Doing make clean in " + vcsPath.toAbsolute.normalize.path)
      FileOperations.clean(vcsPath)
    }

    if (cPath != "") FileOperations.compile(emPath, emPath / Path("emulator"))
    if (rPath != "") FileOperations.compile(vcsPath, vcsPath / Path("simv"))

    if (cPath != "") cSim = Some(emPath.toAbsolute.normalize.path + "/emulator")
    if (rPath != "") rSim = Some(vcsPath.toAbsolute.normalize.path + "/simv")
    (cSim, rSim)
  }
}
