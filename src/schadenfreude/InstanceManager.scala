package torture
package schadenfreude

import scala.sys.process._
import scalax.file.Path
import java.io.File
import java.util.Properties
import java.io.FileInputStream

class InstanceManager(val cfgs: List[String], val gitcmts: List[String], val permDir: String, val tmpDir: String, val cPath: String, val rPath: String, val email: String, val thresh: Int, val minutes: Int, val instcnt: Int, val insttype: String)
{
  val instRunners: Array[InstanceRunner] = new Array(instcnt)
  val processRA: Array[Process] = new Array(instcnt)
  val cfgmap = mapOptions(cfgs,"config")
  val commitmap = mapOptions(gitcmts,"none")
  val cmdstrRA: Array[String] = getCommandStrings()
  val walltime = getWalltime()

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

    var commitList: List[String] = List()
    for (i <- 0 until instcnt)
    {
      var tmpCmd = cmdstring
      val tmpcommit = commitmap(i)
      if (tmpcommit == "none")
      {
        if (usingC && cPath != "../riscv-rocket/emulator/emulator") tmpCmd += " C_SIM="+cPath
        if (usingR && rPath != "../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv") tmpCmd += " R_SIM="+rPath
        tmpCmd += cmdstring2+"\""
      } else {
        if (!commitList.contains(tmpcommit))
        {
          commitList = commitList ++ List(tmpcommit)
          if (insttype=="local") checkoutRocket(tmpcommit, cPath, rPath, usingC, usingR)
          if (insttype=="psi") checkoutRocketPSI(tmpcommit, cPath, rPath, usingC, usingR)
        }
        if (usingR) tmpCmd += " R_SIM=../rocket_"+tmpcommit
        if (usingC) tmpCmd += " C_SIM=../rocket_"+tmpcommit
        tmpCmd += cmdstring2+"\""
      }
      cmdRA(i) = tmpCmd
    }
    cmdRA
  }

  def checkoutRocketPSI(commit: String, cPath: String, rPath: String, usingC: Boolean, usingR: Boolean): Unit =
  {
    var rocketDir = ""
    val fileop = overnight.FileOperations
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

  def checkoutRocket(commit: String, cPath: String, rPath: String, usingC: Boolean, usingR: Boolean): Unit =
  {
    var rocketDir = ""
    val fileop = overnight.FileOperations
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
    val logtime = System.currentTimeMillis
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

} 
