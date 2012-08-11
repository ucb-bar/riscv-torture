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
    assert(List("local","psi","ec2").contains(insttype), println("Invalid instance type specified."))
    val runner: InstanceRunner = insttype match {
      case "local" => new LocalRunner(instnum, mgr)
      case "psi" => new PSIRunner(instnum, mgr)
      case "ec2" => new EC2Runner(instnum, mgr)
    }
    return runner
  }
}

abstract class InstanceRunner
{
  val instancenum: Int
  var fileLogger = ProcessLogger(line => (), line => ())
  var locallogtime: Long = 0L
  val mgr: InstanceManager
  val fileop = overnight.FileOperations

  def copyTortureDir(tortureDir: String, instDir: String, config: String): Unit
  def run(cmdstr: String, workDir: String): Process
  def isDone(): Boolean
  def collectLogFile(permdir: String): Unit
  def createLogger(logtime: Long): Unit = //Maybe move processlogger creation to instantiation
  {
    val logname = "output/schad" + instancenum + "_" + logtime + ".log"
    val logfile = new File(logname)
    logfile.createNewFile() //Ensure that the file exists.
    val plog = ProcessLogger(line => writeln(line, logname), line => writeln(line, logname))
    fileLogger = plog
    locallogtime = logtime
    println("Instance log output will be placed in " + (new File(logname)).getCanonicalPath())
  }
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

class EC2Runner(val instancenum: Int, val mgr: InstanceManager) extends InstanceRunner
{
  val ec2mgr = mgr.asInstanceOf[EC2InstanceManager]

  def copyTortureDir(tortureDir: String, instDir: String, config: String): Unit =
  {
    val torturePath: Path = tortureDir
    val instPath: Path = instDir
    val configPath: Path = config
    if (instancenum == 0) fileop.scp(torturePath, instPath, ec2mgr.sshhost, ec2mgr.sshopts)
    if (instancenum == 0) fileop.scp(torturePath / configPath, instPath / Path("config"), ec2mgr.sshhost, ec2mgr.sshopts)
    if (instancenum != 0) fileop.scp(torturePath / configPath, instPath / Path("config"+instancenum), ec2mgr.sshhost, ec2mgr.sshopts)
    
  }
  
  def run(cmdstr: String, workDir: String): Process =
  {
    if (instancenum == 0)
    {
      println("Instance output log will be placed in EC2 directory " + workDir + "/output/schad"+instancenum+"_"+locallogtime+".log")
      val sshcmd = "ssh " + ec2mgr.sshopts + " " + ec2mgr.sshhost + " cd " + workDir + " ; " + cmdstr
      println("Starting EC2 remote schadenfreude job in " + workDir)
      println(sshcmd)
      val proc = sshcmd.run(fileLogger)
      println("Started running remote EC2 schadenfreude job.")
      proc
    } else return "echo Should not be printing this.".run
  }

  def isDone(): Boolean =
  {
    val remotefile: Path = mgr.tmpDir + "/riscv-torture/EC2DONE"
    if (instancenum != 0) return true
    else return fileop.remotePathExists(remotefile, ec2mgr.sshhost, ec2mgr.sshopts)
  }

  def collectLogFile(permdir: String): Unit =
  {
    var pdir = ""
    if (permdir != "") pdir = permdir
    else pdir = "output"
    val remotelog: Path = permdir + "riscv-torture/output/schad"+instancenum+".log"
    val locallog: Path = pdir + "/schad"+instancenum+"_"+locallogtime+".log"
    fileop.scpFileBack(remotelog, locallog, ec2mgr.sshhost, ec2mgr.sshopts)
  }
}

class LocalRunner(val instancenum: Int, val mgr: InstanceManager) extends InstanceRunner
{
  def copyTortureDir(tortureDir: String, instDir: String, config: String): Unit =
  {
    val torturePath: Path = tortureDir
    val instPath: Path = instDir
    val cfgPath: Path = config
    println("Copying torture directory to: " + instPath.normalize.path)
    if (instPath.isDirectory)
    {
      println(instPath.normalize.path + " already exists. Not copying torture.")
    } else {
      fileop.copy(torturePath, instPath)
      println("Copied torture to " + instPath.normalize.path)
    }
    fileop.copy(cfgPath, instPath / Path("config"))
    println("Using config file: " + cfgPath.path)
    println(" Cleaning up " + (instPath / Path("output")).normalize.path + " before running.")
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

  def isDone(): Boolean =
  {
    val logfile = "output/schad" + instancenum + "_" + locallogtime + ".log"
    val grepcmd = "grep Leaving " + logfile  //grep for better term.
    println(grepcmd)
    val output = grepcmd.!!
    println(output)
    return (output != "")
  }

  def collectLogFile(permdir: String): Unit =
  {
    if (!mgr.asInstanceOf[BasicInstanceManager].ec2inst) return
    val logname = "output/schad" + instancenum + "_" + locallogtime + ".log"
    val newlogname = "output/schad"+instancenum+".log"
    val mvcmd = "mv " + logname + " " + newlogname
    mvcmd.!
  }
}

class PSIRunner(val instancenum: Int, val mgr: InstanceManager) extends InstanceRunner
{
  var sshval: String = ""
  var ssherr: String = ""
  def copyTortureDir(tortureDir: String, instDir: String, config: String): Unit =
  {
    val torturePath: Path = tortureDir
    val instPath: Path = instDir
    //Complete the psi.qsub script
    fileop.copy(torturePath / Path("partialpsi.qsub"),torturePath / Path("psi.qsub"))
    val writer = new FileWriter("psi.qsub", true)
    try {
      writer.write(mgr.cmdstrRA(instancenum))
    } finally {
      writer.close()
    }
    fileop.scp(torturePath, instPath, "psi","")
    fileop.scp(torturePath / Path("psi.qsub"), instPath / Path("psi.qsub"), "psi","")
    fileop.scp(torturePath / Path(config), instPath / Path("config"), "psi","")
  }
  
  def run(cmdstr: String, workDir: String): Process =
  {
    println("Instance output log will be placed in remote PSI file " + workDir + "/schad" + instancenum + "_" + locallogtime + ".out")
    println("Instance error log will be placed in remote PSI file " + workDir + "/schad" + instancenum + "_" + locallogtime + ".err")
    val sshcmd = "ssh psi cd " + workDir + " ; " + qsub(workDir)
    println(("Starting instance %d".format(instancenum)) + " remotely in PSI directory " + workDir)
    println(sshcmd)
    val proc = sshcmd.run(fileLogger)
    println("Started running instance %d\n" format(instancenum))
    proc
  }

  def isDone(): Boolean =
  {
    assert (ssherr=="", println("Error in qsubbing."))
    val jobid = sshval.dropRight(28)
    var out = ""
    val exit = Process("ssh psi qstat " + jobid).!(ProcessLogger(line=> if (line!="") out=line, line=>if (line!="") out=line))
    return out.contains("Unknown Job Id")
  }

  def collectLogFile(permdir: String): Unit =
  {
    val remoteout: Path = mgr.tmpDir+"/schad"+instancenum+"/schad"+instancenum+"_"+locallogtime+".out"
    val remoteerr: Path = mgr.tmpDir+"/schad"+instancenum+"/schad"+instancenum+"_"+locallogtime+".err"
    var pdir = ""
    if (permdir != "") pdir = permdir
    else pdir = "output"
    val localout: Path = pdir + "/schad"+instancenum+"_"+locallogtime+".out"
    val localerr: Path = pdir + "/schad"+instancenum+"_"+locallogtime+".err"
    fileop.scpFileBack(remoteout, localout, "psi", "")
    fileop.scpFileBack(remoteerr, localerr, "psi", "")
  }
  
  override def createLogger(logtime: Long): Unit = //Maybe move processlogger creation to instantiation
  {
    val logname = "output/schad" + instancenum + "_" + logtime + ".log"
    val plog = ProcessLogger(line => { writeln(line, logname); sshval=line }, line => {writeln(line, logname); ssherr = line})
    fileLogger = plog
    locallogtime = logtime
    println("Instance log output will be placed in " + (new File(logname)).getCanonicalPath())
  }

  private def qsub(instDir: String): String = 
  {
    val logfile = "schad" + instancenum + "_" + locallogtime
    val wt = mgr.runtime * 2 // Extra time so it doesn't cut the test off before it finishes.
    val walltime = (wt/60) + ":" + (wt % 60) + ":00"
    val cput = walltime // Fine to have them the same?
    
    var qsubstr = "qsub -N schad" + instancenum + " -r n -e localhost:" + instDir + "/" + logfile + ".err"
    qsubstr += " -o localhost:" + instDir + "/" + logfile + ".out -q psi -l nodes=1:ppn=1 -l mem=1024m"
    qsubstr += " -l walltime=" + walltime + " -l cput=" + cput + " psi.qsub"
    qsubstr
  }
}
