package torture
package schadenfreude

import scala.sys.process._
import scalax.file.Path
import scalax.file.FileSystem
import java.io.File
import java.io.FileWriter

object InstanceRunner
{
  def apply(insttype: String, instnum: Int): InstanceRunner = 
  {
    assert(List("local").contains(insttype), println("Invalid instance type specified."))
    val runner: InstanceRunner = insttype match {
      case "local" => new LocalRunner(instnum)
    }
    return runner
  }
}

abstract class InstanceRunner
{
  val instancenum: Int
  var fileLogger: ProcessLogger

  def copyTortureDir(tortureDir: String, instDir: String, config: String): Unit
  def createLogger(logtime: Long): Unit
  def run(cmdstr: String, workDir: String): Process
  def writeln(line: String, logfile: String): Unit =
  {
    val writer = new FileWriter(logfile, true)
    try {
      writer.write(line + "\n")
    } finally {
      writer.close()
    }
  }
}

class LocalRunner(val instancenum: Int) extends InstanceRunner
{
  var fileLogger = ProcessLogger(line => (), line => ())

  def createLogger(logtime: Long): Unit =
  {
    val logname = "output/schad" + instancenum + "_" + logtime + ".log"
    val plog = ProcessLogger(line => writeln(line, logname), line => writeln(line, logname))
    fileLogger = plog
    println("Instance log output will be placed in " + (new File(logname)).getCanonicalPath())
  }

  def copyTortureDir(tortureDir: String, instDir: String, config: String): Unit =
  {
    def canonicalPath(p: Path): String = (new File(p.toAbsolute.path)).getCanonicalPath()
    val torturePath: Path = tortureDir
    val instPath: Path = instDir
    val cfgPath: Path = config
    println("Copying torture directory to: " + canonicalPath(instPath))
    if (instPath.isDirectory)
    {
      println(canonicalPath(instPath) + "already exists. Not copying torture.")
    } else {
      torturePath.copyTo(instPath)
      println("Copied torture to " + canonicalPath(instPath))
    }
    cfgPath.copyTo((instPath / Path("config")), replaceExisting=true)
    println("Using config file: " + cfgPath.path)
    println(" Cleaning up " + canonicalPath(instPath / Path("output")) + " before running.")
    Process("make clean-all", new File(instDir + "/output")).!
  }

  def run(cmdstr: String, workDir: String): Process = 
  {
    val workDirFile = new File(workDir)
    val cmd = Process(cmdstr, workDirFile)
    println(("Starting instance %d".format(instancenum)) + " in directory " + workDirFile.getCanonicalPath())
    println(cmdstr)
    
    val proc = cmd.run(fileLogger)
    println("Started running instance %d\n" format(instancenum))
    proc
  }
}
