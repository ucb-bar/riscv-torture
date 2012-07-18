package torture
package schadenfreude

import scopt.OptionParser
import scala.sys.process._
import scalax.file.Path
import scalax.file.FileSystem
import java.io.File
import java.io.FileWriter

case class Options(var timeToRun: Option[Int] = None,
  var emailAddress: Option[String] = None,
  var errorThreshold: Option[Int] = None,
  var cSimPath: Option[String] = None,
  var rtlSimPath: Option[String] = None,
  var permDir: Option[String] = None,
  var tempDir: Option[String] = None,
  var confFileName: Option[String] = None,
  var instanceCnt: Option[Int] = None)

object Schadenfreude extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("schadenfreude/run") {
      opt("C", "config", "<file>", "config file", {s: String => opts.confFileName = Some(s)})
      opt("p", "permdir", "<dir>", "dir to store failing tests", {s: String => opts.permDir = Some(s)})
      opt("d", "tmpdir", "<dir>", "dir to create temporary instance dirs in", {s: String => opts.tempDir = Some(s)})
      opt("c", "csim", "<file>", "C simulator", {s: String => opts.cSimPath = Some(s)})
      opt("r", "rtlsim", "<file>", "RTL simulator", {s: String => opts.rtlSimPath = Some(s)})
      opt("e", "email", "<address>", "email to report to", {s: String => opts.emailAddress = Some(s)})
      opt("t", "threshold", "<count>", "number of failures to trigger email", {i: String => opts.errorThreshold = Some(i.toInt)})
      opt("m", "minutes", "<int>", "number of minutes to run tests", {i: String => opts.timeToRun = Some(i.toInt)})
      opt("n", "instcnt", "<int>", "number of instances to run", {i: String => opts.instanceCnt = Some(i.toInt)})
    }
    if (parser.parse(args))
    {
      val confFileName = opts.confFileName.getOrElse("config")
      val permDir      = opts.permDir.getOrElse("output/failedtests")
      val tmpDir       = opts.tempDir.getOrElse("..")
      val cPath        = opts.cSimPath.getOrElse("")
      val rPath        = opts.rtlSimPath.getOrElse("")
      val email        = opts.emailAddress.getOrElse("")
      val thresh       = opts.errorThreshold.getOrElse(5)
      val minutes      = opts.timeToRun.getOrElse(1)
      val instcnt      = opts.instanceCnt.getOrElse(1)
      runInstances(confFileName, permDir, tmpDir,  cPath, rPath, email, thresh, minutes, instcnt)
    }
  }

  def runInstances(conf: String, permdir: String, tmpDir: String, cPath: String, rPath: String, email: String, thresh: Int, minutes: Int, instcnt: Int)
  {
    copyTortureDir(Path("."), Path(tmpDir), instcnt)
    val processRA = new Array[Process](instcnt)
    println("\nRunning %d instances" format(instcnt))
    val logtime = System.currentTimeMillis
    for (i <- 0 until instcnt)
    {
      var cmdstring = ""
      if (cPath != "" && rPath != "")
        cmdstring += "make crnight"
      if (cPath != "" && rPath == "")
        cmdstring += "make cnight"
      if (cPath == "" && rPath != "")
        cmdstring += "make rnight"
      if (cPath == "" && rPath == "")
      {
        System.err.println("No simulators were specified")
        System.exit(1)
      }
      if (email != "") cmdstring += "e EMAIL=" + email
      cmdstring += " ERRORS=" + thresh + " MINUTES=" + minutes
      cmdstring += " DIR=" + permdir
      
      val logname = "output/schad" + i + "_" + logtime + ".log"
      val logfile = new File(logname)
      val plog = ProcessLogger(line => writeln(line, logname), line => writeln(line, logname))
      val workDir = new File(tmpDir + "/schad" + i)
      val cmd = Process(cmdstring, workDir)

      println(("Starting instance %d".format(i)) + " in directory " + workDir.getCanonicalPath())
      println("Instance log output will be placed in " + (new File(logname)).getCanonicalPath())
      println(cmdstring)
      processRA(i) = cmd.run(plog)
      println("Started running instance %d\n" format(i))
    }
  }

  def copyTortureDir(torturePath: Path, tmpPath: Path, instcnt: Int)
  {
    def canonicalPath(p: Path): String = (new File(p.toAbsolute.path)).getCanonicalPath()
    println("Copying torture directory to: " + canonicalPath(tmpPath))
    for (i <- 0 until instcnt)
    {
      val instancePath = tmpPath / Path("schad"+i)
      if (instancePath.isDirectory)
      {
        println(canonicalPath(instancePath) + " already exists. Copying only config file")
        (torturePath / Path("config")).copyTo((instancePath / Path("config")), replaceExisting=true)
        println("Copied config to " + canonicalPath(instancePath / Path("config")))
      } else {
        torturePath.copyTo(instancePath)
        println("Copied torture to " + canonicalPath(instancePath))
      }
    }
  }

  private def writeln(line: String, logfile: String)
  {
    val writer = new FileWriter(logfile, true)
    try {
      writer.write(line + "\n")
    } finally {
      writer.close()
    }
  }
}
