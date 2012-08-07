package torture
package schadenfreude

import scala.sys.process._
import scalax.file.Path
import java.io.File
import java.util.Properties
import java.io.FileInputStream

object InstanceManager
{  
  def apply(confFileList: List[String], gitCommitList: List[String], permDir: String, tmpDir: String, cPath: String, rPath: String, email: String, thresh: Int, minutes: Int, instcnt: Int, insttype: String): InstanceManager = 
  {
    assert (List("local","psi").contains(insttype), println("Invalid instance type specified."))
    val mgr: InstanceManager = insttype match {
      case "local" | "psi" => new MillInstanceManager(confFileList, gitCommitList, permDir, tmpDir, cPath, rPath, email, thresh, minutes, instcnt, insttype)
      case "ec2" => new EC2InstanceManager(confFileList, gitCommitList, permDir, tmpDir, cPath, rPath, email, thresh, minutes, instcnt, insttype)
    }
    return mgr
  }
}
abstract class InstanceManager
{
  val cfgs, gitcmts: List[String]

  val cfgmap = mapOptions(cfgs,"config")
  val commitmap = mapOptions(gitcmts,"none")
  val permDir, tmpDir, cPath, rPath, email, insttype: String
  val thresh, minutes, instcnt: Int
  val runtime: Int = getWalltime()
  val instRunners: Array[InstanceRunner] = new Array(instcnt)
  val processRA: Array[Process] = new Array(instcnt)
  val cmdstrRA: Array[String] = getCommandStrings()

  def getCommandStrings(): Array[String]
  def createInstances(): Unit
  def runInstances(): Unit
  def collectLogFiles(): Unit

  def waitOnInstances(): Unit =
  {
    var canexit = false
    while (!canexit)
    {
      println("Spinning...")
      Thread.sleep(60000)
      var done = true
      for (i <- 0 until instcnt)
      {
        done  &= instRunners(i).isDone
      }
      canexit = done
    }
  }
 
  def mapOptions(optList: List[String],default: String): List[String] =
  {
    val cnt = optList.length
    assert (cnt <= instcnt, println("Number of options was greater than the number of instances specified"))
    var map: List[String] = List()
    if (cnt == 0)
    {
      for (i <- 0 until instcnt) map ++= List(default)
    } else {
      for (i <- 0 until instcnt)
      {
        val indx = i % cnt
        map ++= List(optList(indx))
      }
    }
    map
  }

  def getWalltime(): Int =
  {
    var max: Int = minutes
    for (i <- 0 until instcnt)
    {
      val config = new Properties()
      val configin = new FileInputStream(cfgmap(i))
      config.load(configin)
      configin.close()
      val imin = config.getProperty("torture.overnight.minutes","1").toInt
      if (imin > max) max = imin
    }
    max
  } 
}

class EC2InstanceManager(val cfgs: List[String], val gitcmts: List[String], val permDir: String, val tmpDir: String, val cPath: String, val rPath: String, val email: String, val thresh: Int, val minutes: Int, val instcnt: Int, val insttype: String) extends InstanceManager
{

  def getCommandStrings(): Array[String] =
  {
    //recreate the "make *schaden" command to submit to EC2
    Array("placeholder")
  }

  def createInstances(): Unit =
  {
    //create from AMI instance. copy and compile latest rocket
    EC2Configure()
  }

  def runInstances(): Unit = 
  {

  }

  def collectLogFiles(): Unit =
  {

  }

  private def EC2Configure(): Unit =
  {
    //setup instance. generate ssh keypairs. specify instance type, duration.
    //check for aws credentials. prompt if necessary.
  }

  private def EC2StopInstance(): Unit =
  {

  }

}

class MillInstanceManager(val cfgs: List[String], val gitcmts: List[String], val permDir: String, val tmpDir: String, val cPath: String, val rPath: String, val email: String, val thresh: Int, val minutes: Int, val instcnt: Int, val insttype: String) extends InstanceManager
{
  var logtime: Long = 0L
  val fileop = overnight.FileOperations

  def getCommandStrings(): Array[String] = 
  {
    val cmdRA: Array[String] = new Array(instcnt)
    var cmdstring = ""
    var cmdstring2= " OPTIONS=\""
    if (cPath != "" && rPath != "")
      cmdstring += "make crnight"
    if (cPath != "" && rPath == "")
      cmdstring += "make cnight"
    if (cPath == "" && rPath != "")
      cmdstring += "make rnight"

    val usingR = cmdstring.contains("r")
    val usingC = cmdstring.contains("c")

    assert(cmdstring != "", println("No simulators were specified"))

    cmdstring += " COMMIT=none"
    if (email != "") cmdstring2 += " -e " + email
    if (thresh != -1) cmdstring2 += " -t " + thresh
    if (minutes != -1) cmdstring2 += " -m " + minutes
    if (permDir != "") cmdstring2 += " -p " + permDir
    cmdstring2 += "\""

    var commitList: List[String] = List()
    for (i <- 0 until instcnt)
    {
      var tmpCmd = cmdstring
      val tmpcommit = commitmap(i)
      if (tmpcommit == "none")
      {
        if (usingC && cPath != "../riscv-rocket/emulator/emulator") tmpCmd += " C_SIM="+cPath
        if (usingR && rPath != "../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv") tmpCmd += " R_SIM="+rPath
        if (cmdstring2 != " OPTIONS=\"\"") tmpCmd += cmdstring2
      } else {
        if (!commitList.contains(tmpcommit))
        {
          commitList = commitList ++ List(tmpcommit)
          if (insttype=="local") checkoutRocket(tmpcommit, cPath, rPath, usingC, usingR)
          if (insttype=="psi") checkoutRocketPSI(tmpcommit, cPath, rPath, usingC, usingR)
        }
        if (usingR) tmpCmd += " R_SIM=../rocket_"+tmpcommit+"/vlsi-generic/build/vcs-sim-rtl/simv"
        if (usingC) tmpCmd += " C_SIM=../rocket_"+tmpcommit+"/emulator/emulator"
        if (cmdstring2 != " OPTIONS=\"\"") tmpCmd += cmdstring2
      }
      cmdRA(i) = tmpCmd
    }
    cmdRA
  }

  private def checkoutRocketPSI(commit: String, cPath: String, rPath: String, usingC: Boolean, usingR: Boolean): Unit =
  {
    var rocketDir = ""
    if (usingC) rocketDir = cPath.substring(0,cPath.length-18)
    if (usingR) rocketDir = rPath.substring(0,rPath.length-36)
    if (rocketDir != "")
    {
      val rocketPath: Path = rocketDir
      if (commit != "none")
      {
        val remoteDir = tmpDir + "/rocket_" + commit
        val remoteOldPath: Path = rocketDir
        val remotePath: Path = remoteDir
        val remoteCPath: Path = remoteDir+"/emulator"
        val remoteRPath: Path = remoteDir+"/vlsi-generic/build/vcs-sim-rtl"
        if (!fileop.remotePathExists(remotePath, "psi"))
        {
          fileop.gitcheckoutRemote(remoteOldPath, remotePath, commit, "psi")
          fileop.cleanRemote(remoteCPath, "psi")
          fileop.cleanRemote(remoteRPath, "psi")
        }
        if (usingR) fileop.compileRemote(remoteRPath, remoteRPath/Path("simv"), "psi")
        if (usingC) fileop.compileRemote(remoteCPath, remoteCPath/Path("emulator"), "psi")
      }
    }
  }

  private def checkoutRocket(commit: String, cPath: String, rPath: String, usingC: Boolean, usingR: Boolean): Unit =
  {
    var rocketDir = ""
    if (usingC) rocketDir = cPath.substring(0,cPath.length-18)
    if (usingR) rocketDir = rPath.substring(0,rPath.length-36)
    if (commit == "none") return
    if (rocketDir != "")
    {
      val rocketPath: Path = rocketDir
      val tmpRocketPath: Path = "../rocket_"+commit
      val emPath: Path = (tmpRocketPath/Path("emulator"))
      val vcsPath: Path = (tmpRocketPath/Path("vlsi-generic")/Path("build")/Path("vcs-sim-rtl"))
      if (!tmpRocketPath.exists)
      {
        fileop.gitcheckout(rocketPath, tmpRocketPath, commit)
        println("Doing make clean in " + (emPath.toAbsolute.normalize.path))
        fileop.clean(emPath)
        println("Doing make clean in " + (vcsPath.toAbsolute.normalize.path))
        fileop.clean(vcsPath)
      }
      if (usingC) fileop.compile(emPath, emPath/Path("emulator"))  
      if (usingR) fileop.compile(vcsPath, vcsPath/Path("simv"))
    }
  }

  def createInstances(): Unit = 
  {
    for (i <- 0 until instcnt) instRunners(i) = InstanceRunner(insttype,i,this)
  }

  def runInstances(): Unit = 
  {
    if (instcnt == 1)
    {
      println("\nRunning 1 instance of overnight.\n")
    } else {
      println("\nRunning %d instances of overnight.\n".format(instcnt))
    }
    logtime = System.currentTimeMillis
    for (i <- 0 until instcnt)
    {
      println("Starting instance %d".format(i) + "\n")
      val instance = instRunners(i)
      val instDir = tmpDir + "/schad" + i
      val tortureDir = "."
      val config = cfgmap(i)
      instance.createLogger(logtime)
      instance.copyTortureDir(tortureDir, instDir, config)
      processRA(i) = instance.run(cmdstrRA(i), instDir)
    }
    println("\nAll instances have been launched.")
  }

  def collectLogFiles(): Unit =
  {
    if (insttype == "local") return
    if (insttype == "psi")
    {
      for (i <- 0 until instcnt)
      {
        val remoteout: Path = tmpDir+"/schad"+i+"/schad"+i+"_"+logtime+".out"
        val remoteerr: Path = tmpDir+"/schad"+i+"/schad"+i+"_"+logtime+".err"
        var pdir = ""
        if (permDir != "") pdir = permDir
        else pdir= "output"
        val localout: Path = pdir + "/schad"+i+"_"+logtime+".out"
        val localerr: Path = pdir + "/schad"+i+"_"+logtime+".err"
        fileop.scpFileBack(remoteout, localout, "psi")
        fileop.scpFileBack(remoteerr, localerr, "psi")
      }
    }
  }
} 
