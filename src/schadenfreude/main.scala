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
  var confFileList: Option[List[String]] = Some(List()),
  var instanceCnt: Option[Int] = None)

object Schadenfreude extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("schadenfreude/run") {
      opt("C", "config", "<file>", "config file", {s: String => opts.confFileList = Some(opts.confFileList.get ++ List(s))})
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
      val confFileList = opts.confFileList.get
      val permDir      = opts.permDir.getOrElse("output/failedtests")
      val tmpDir       = opts.tempDir.getOrElse("..")
      val cPath        = opts.cSimPath.getOrElse("")
      val rPath        = opts.rtlSimPath.getOrElse("")
      val email        = opts.emailAddress.getOrElse("")
      val thresh       = opts.errorThreshold.getOrElse(5)
      val minutes      = opts.timeToRun.getOrElse(1)
      val instcnt      = opts.instanceCnt.getOrElse(1)
      runInstances(confFileList, permDir, tmpDir,  cPath, rPath, email, thresh, minutes, instcnt)
    }
  }

  def runInstances(cfgs: List[String], permdir: String, tmpDir: String, cPath: String, rPath: String, email: String, thresh: Int, minutes: Int, instcnt: Int)
  {
    copyTortureDir(Path("."), Path(tmpDir), instcnt, cfgs)
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

      assert(cmdstring != "", println("No simulators were specified"))

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

  def copyTortureDir(torturePath: Path, tmpPath: Path, instcnt: Int, cfgs: List[String])
  {
    def canonicalPath(p: Path): String = (new File(p.toAbsolute.path)).getCanonicalPath()
    val cfgpairs = mapConfigFiles(instcnt, cfgs)
    println("Copying torture directory to: " + canonicalPath(tmpPath))
    for (i <- 0 until instcnt)
    {
      val instancePath = tmpPath / Path("schad"+i)
      if (instancePath.isDirectory)
      {
        println(canonicalPath(instancePath) + " already exists. Not copying directory.")
      } else {
        torturePath.copyTo(instancePath)
        println("Copied torture to " + canonicalPath(instancePath))
      }
      if (cfgs.length > 1)
      {
        val cfgPath: Path = cfgpairs(i)
        (torturePath / cfgPath).copyTo((instancePath / Path("config")), replaceExisting=true)
        println("Using config file: " + cfgPath.name + " for instance " + i)
      }
    }
  }

  def mapConfigFiles(instcnt: Int, cfgs: List[String]): List[String] =
  {
    val cfgcnt = cfgs.length
    if (cfgcnt == 0) return List("config")
    assert (cfgcnt <= instcnt, println("Number of config files was greater than the number of instances specified"))
    var map: List[String] = List()
    for (i <- 0 until instcnt)
    {
      val cfgindx = i % cfgcnt
      map ++= List(cfgs(cfgindx))
    }
    map
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
