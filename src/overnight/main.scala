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
import torture.fileop._


case class Options(var timeToRun: Int = Overnight.DefTime,
  var emailAddress: String = Overnight.DefEmail,
  var errorThreshold: Int = Overnight.DefThresh,
  var cSimPath: String = Overnight.DefCSim,
  var rtlSimPath: String = Overnight.DefRtlSim,
  var permDir: String = Overnight.DefPermDir,
  var gitCommit: String = Overnight.DefGitCommit,
  var confFileName: String = Overnight.DefConfig,
  var output: Boolean = false,
  var dumpCSim: Boolean = false,
  var dumpVSim: Boolean = false)

object Overnight extends App
{
  val DefTime = 1
  val DefEmail = "your@email.address"
  val DefThresh = 1
  val DefCSim = ""
  val DefRtlSim = ""
  val DefPermDir = "output/failedtests"
  val DefGitCommit = ""
  val DefConfig = "config"
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser[Options]("overnight/run") {
      opt[String]('C', "config") valueName("<file>") text("config file") action {(s: String, c) => c.copy(confFileName = s)}
      opt[String]('p', "permdir") valueName("<dir>") text("dir to store failing tests") action {(s: String, c) => c.copy(permDir = s)}
      opt[String]('c', "csim") valueName("<file>") text("C simulator") action {(s: String, c) => c.copy(cSimPath = s)}
      opt[String]('r', "rtlsim") valueName("<file>") text("RTL simulator") action {(s: String, c) => c.copy(rtlSimPath = s)}
      opt[String]('e', "email") valueName("<address>") text("email to report to") action {(s: String, c) => c.copy(emailAddress = s)}
      opt[String]('g', "gitcommit") valueName("<git commit>") text("Git commit to check out") action {(s: String, c) => c.copy(gitCommit = s)}
      opt[Int]('t', "threshold") valueName("<count>") text("number of failures to trigger email") action {(i: Int, c) => c.copy(errorThreshold = i)}
      opt[Int]('m', "minutes") valueName("<minutes>") text("number of minutes to run tests") action {(i: Int, c) => c.copy(timeToRun = i)}
      opt[Unit]("output") abbr("o") text("Write verbose output of simulators to file") action {(_, c) => c.copy(output = true)}
      opt[Unit]("dumpcsim") abbr("dc") text("Create a vcd from csim") action {(_, c) => c.copy(dumpCSim = true)}
      opt[Unit]("dumpvsim") abbr("dv") text("Create a vpd from vsim") action {(_, c) => c.copy(dumpVSim = true)}
    }
    parser.parse(args, Options()) match {
      case Some(opts) =>
        overnight(opts.confFileName, opts.permDir, opts.cSimPath, opts.rtlSimPath,
          opts.emailAddress, opts.gitCommit, opts.errorThreshold, opts.timeToRun, opts.output, opts.dumpCSim, opts.dumpVSim)
      case None =>
        System.exit(1) // error message printed by parser
    }
  }
    def overnight(configFileName: String,
                  outputDir: String,
                  cSimPath: String,
                  rtlSimPath: String,
                  emailAddress: String,
                  gitCommit: String,
                  errorThreshold: Int,
                  timeToRun: Int,
                  output: Boolean,
                  dumpCSim: Boolean,
                  dumpVSim: Boolean)
    {
      val config = new Properties()
      val configin = new FileInputStream(configFileName)
      config.load(configin)
      configin.close()

      val errors  = Option(config.getProperty("torture.overnight.errors")) map ( _.toInt )
      val thresh  = if(errorThreshold == DefThresh) errors.getOrElse(DefThresh) else errorThreshold
      val runtime = Option(config.getProperty("torture.overnight.minutes")) map ( _.toInt )
      val minutes = if(timeToRun == DefTime) runtime.getOrElse(DefTime) else timeToRun
      val outdir  = Option(config.getProperty("torture.overnight.outdir"))
      val permDir = if(outputDir == DefPermDir) outdir.getOrElse(DefPermDir) else outputDir
      val email   = Option(config.getProperty("torture.overnight.email"))
      val address = if(emailAddress == DefEmail) email.getOrElse(DefEmail) else emailAddress

      val startTime = System.currentTimeMillis
      var endTime = startTime + minutes*60*1000
      var errCount = 0

      val (cSim, rtlSim) = checkoutRocket(cSimPath, rtlSimPath, gitCommit)
      while(System.currentTimeMillis < endTime) {
        val baseName = "test_" + System.currentTimeMillis
        val newAsmName = generator.Generator.generate(configFileName, baseName)
        val (failed, test) = testrun.TestRunner.testrun( Some(newAsmName), cSim, rtlSim, true, output, dumpCSim, dumpVSim, configFileName) 
        if(failed) {
          errCount += 1
          test foreach { t =>
            println(t)
            println(t.last)
            val permFiles:PathSet[Path] = Path(t.init:_*) * (t.last + "*")
            val statFile: Path = Path(t.init:_*) / (baseName+".stats")
            println(permFiles.mkString)
            println(statFile)
            permFiles.foreach( f => f.copyTo( permDir / f.name, copyAttributes=false))
            statFile.copyTo(permDir / statFile.name, replaceExisting=true, copyAttributes=false)
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
      if (address != DefEmail)
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

  def checkoutRocket(cPath: String, rPath: String, commit: String): (Option[String],Option[String]) =
  {
    var cSim: Option[String] = None
    var rSim: Option[String] = None
    val cmmt = commit.toUpperCase
    if (cPath != DefCSim) cSim = Some(cPath)
    if (rPath != DefRtlSim) rSim = Some(rPath)
    if (cmmt == "NONE") return (cSim, rSim)

    var rocketDir = ""
    if (cPath != DefCSim) rocketDir = cPath.substring(0,cPath.length-18)
    if (rPath != DefRtlSim) rocketDir = rPath.substring(0,rPath.length-36)
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

    if (cPath != DefCSim) FileOperations.compile(emPath, emPath / Path("emulator"))
    if (rPath != DefRtlSim) FileOperations.compile(vcsPath, vcsPath / Path("simv"))

    if (cPath != DefCSim) cSim = Some(emPath.toAbsolute.normalize.path + "/emulator")
    if (rPath != DefRtlSim) rSim = Some(vcsPath.toAbsolute.normalize.path + "/simv")
    (cSim, rSim)
  }
}
