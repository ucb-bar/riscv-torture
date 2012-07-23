package torture
package schadenfreude

import scala.sys.process._
import scalax.file.Path
import scalax.file.FileSystem
import java.io.File
import java.io.FileWriter

object InstanceRunner
{
  def apply(insttype: String, instnum: Int, mgr: InstanceManager): InstanceRunner = 
  {
    assert(List("local","psi").contains(insttype), println("Invalid instance type specified."))
    val runner: InstanceRunner = insttype match {
      case "local" => new LocalRunner(instnum, mgr)
      case "psi" => new PSIRunner(instnum, mgr)
    }
    return runner
  }
}

abstract class InstanceRunner
{
  val instancenum: Int
  var fileLogger: ProcessLogger
  val mgr: InstanceManager

  def copyTortureDir(tortureDir: String, instDir: String, config: String): Unit
  def createLogger(logtime: Long): Unit //Maybe move ProcessLogger creation to be done at object instantiation
  def run(cmdstr: String, workDir: String): Process
  def canonicalPath(p: Path): String = (new File(p.toAbsolute.path)).getCanonicalPath()
  def writeln(line: String, logfile: String): Unit =
  {
    val writer = new FileWriter(logfile, true)
    try {
      writer.write(line + "\n")
    } finally {
      writer.close()
    }
  }
  def scp(localPath: Path, remotePath: Path, host: String): Unit = 
  {
    def scpFile(localPath: Path, remotePath: Path): Unit = 
    {
      val localStr = localPath.path
      val remoteStr = remotePath.path
      println("Copying file " + localPath.name + " to " + host + " remote directory " + remoteStr)
      val cmd = "scp " + localStr + " "+host+":"+remoteStr
      println(cmd)
      val exitCode = cmd.!
      assert(exitCode == 0, println("SCP failed to successfully copy file " + localPath.name))
      println("Successfully copied file to remote "+host+" directory.\n")
    }
    def compressDir(dir: String, tgz: String): Unit = 
    {
      println("Compressing directory to " + tgz)
      val tarcmd = "tar -czf " + tgz + " " + dir
      println(tarcmd)
      val out = tarcmd.!
      assert (out == 0, println("Failed to properly compress directory."))
      println("Successfully compressed directory to " + tgz + "\n")
    }
    def extractDir(remoteTgz: String, remoteDir: String): Unit = 
    {
      println("Extracting "+remoteTgz+" to "+host+" remote directory " + remoteDir)
      val extractcmd = "ssh "+host+" tar -xzf " + remoteTgz +" -C " + remoteDir
      println (extractcmd)
      val out = extractcmd.!
      assert (out == 0, println("Failed to extract remote file " + remoteTgz + " to directory " + remoteDir))
      println("Successfully extracted to remote directory " + remoteDir + "\n")
    }

    assert(localPath.exists, println("Local object to be copied does not exist."))
    if (localPath.isDirectory)
    {
      //Zip it up, scp it, then unzip
      val canonPath: Path = canonicalPath(localPath)
      val remoteParentPath = remotePath.parent.get
      val tgzName = canonPath.name + ".tgz"
      val tgzPath: Path = "../" + tgzName
      val remoteTgzPath: Path = (remoteParentPath / Path(tgzName))

      val cmd = ("ssh "+host+" ls " + remoteParentPath.path)
      val output = (cmd.!!).split("\n")
      val remoteExists = output.contains(remotePath.name)
      val remoteTgzExists = output.contains(tgzName)

      if (remoteExists) {
        println("Remote directory already exists. Skipping copy process.")
      } else {
        if (remoteTgzExists) {
          println(tgzName + " already exists on the remote "+host+" directory. Skipping transfer process.")
        } else {
          if(!tgzPath.exists) {
            compressDir(".", "../"+tgzName)
          } else {
            println(tgzName+" already exists. Skipping compression process.")
          }  
          scpFile(tgzPath, remoteTgzPath)
        }
        val out2 = ("ssh "+host+" mkdir " + remotePath.path).!!
        extractDir(remoteTgzPath.path, remotePath.path)
      }
    } else {
      scpFile(localPath, remotePath)
    }
  }
}

class LocalRunner(val instancenum: Int, val mgr: InstanceManager) extends InstanceRunner
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
    val torturePath: Path = tortureDir
    val instPath: Path = instDir
    val cfgPath: Path = config
    println("Copying torture directory to: " + canonicalPath(instPath))
    if (instPath.isDirectory)
    {
      println(canonicalPath(instPath) + " already exists. Not copying torture.")
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

class PSIRunner(val instancenum: Int, val mgr: InstanceManager) extends InstanceRunner
{
  var fileLogger = ProcessLogger(line => (), line => ())
  var locallogtime: Long = 0

  def createLogger(logtime: Long): Unit =
  {
    val logname = "output/schad" + instancenum + "_" + logtime + ".log"
    val plog = ProcessLogger(line => writeln(line, logname), line => writeln(line, logname))
    fileLogger = plog
    locallogtime = logtime
  }

  def copyTortureDir(tortureDir: String, instDir: String, config: String): Unit =
  {
    val torturePath: Path = tortureDir
    val instPath: Path = instDir
    //Complete the psi.sh script
    (torturePath / Path("partialpsi.qsub")).copyTo(torturePath / Path("psi.qsub"), replaceExisting=true)
    val writer = new FileWriter("psi.qsub", true)
    try {
      writer.write(mgr.cmdstr)
    } finally {
      writer.close()
    }
    scp(torturePath, instPath, "psi")
    scp(torturePath / Path("psi.qsub"), instPath / Path("psi.qsub"), "psi")
    scp(torturePath / Path(config), instPath / Path("config"), "psi")
  }
  
  def run(cmdstr: String, workDir: String): Process =
  {
    println("Instance output log will be placed in remote PSI file " + workDir + "schad" + instancenum + "_" + locallogtime + ".out")
    println("Instance error log will be placed in remote PSI file " + workDir + "schad" + instancenum + "_" + locallogtime + ".err")
    val sshcmd = "ssh psi cd " + workDir + " ; " + qsub(workDir)
    println(("Starting instance %d".format(instancenum)) + " remotely in PSI directory " + workDir)
    println(sshcmd)
    val proc = sshcmd.run(fileLogger)
    println("Started running instance %d\n" format(instancenum))
    proc
  }
  
  private def qsub(instDir: String): String = 
  {
    val logfile = "schad" + instancenum + "_" + locallogtime
    val wt = scala.math.round(mgr.minutes * 1.2) // Extra time so it doesn't cut the test off before it finishes.
    val walltime = (wt/60) + ":" + (wt % 60) + ":00"
    val cput = walltime // Fine to have them the same?
    
    var qsubstr = "qsub -N schad" + instancenum + " -r n -e localhost:" + instDir + "/" + logfile + ".err"
    qsubstr += " -o localhost:" + instDir + "/" + logfile + ".out -q psi -l nodes=1:ppn=1 -l mem=1024m"
    qsubstr += " -l walltime=" + walltime + " -l cput=" + cput + " psi.qsub"
    qsubstr
  }
}
