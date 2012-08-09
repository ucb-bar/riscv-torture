package torture
package schadenfreude

import scala.sys.process._
import scalax.file.Path
import java.io.File
import java.util.Properties
import java.io.FileInputStream

object InstanceManager
{  
  def apply(confFileList: List[String], gitCommitList: List[String], permDir: String, tmpDir: String, cPath: String, rPath: String, email: String, thresh: Int, minutes: Int, instcnt: Int, insttype: String, ec2inst: Boolean): InstanceManager = 
  {
    assert (List("local","psi","ec2").contains(insttype), println("Invalid instance type specified."))
    if (ec2inst) return new BasicInstanceManager(confFileList, gitCommitList, permDir, tmpDir, cPath, rPath, email, thresh, minutes, instcnt, insttype, ec2inst)
    val mgr: InstanceManager = insttype match {
      case "local" | "psi" => new BasicInstanceManager(confFileList, gitCommitList, permDir, tmpDir, cPath, rPath, email, thresh, minutes, instcnt, insttype, ec2inst)
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
  val fileop = overnight.FileOperations

  def getCommandStrings(): Array[String]
  def runInstances(): Unit
  def collectLogFiles(): Unit
  def createInstances(): Unit = 
  {
    for (i <- 0 until instcnt) instRunners(i) = InstanceRunner(insttype, i, this)
  }

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
  var logtime: Long = 0L

  def getCommandStrings(): Array[String] =
  {
    //do a make schaden -ec2 true -i local
    val cmdRA: Array[String] = new Array(instcnt)
    var cmdstring = ""
    var cmdstring2= " OPTIONS=\"-ec2 true"
    if (cPath != "" && rPath != "")
      cmdstring += "make crschaden"
    if (cPath != "" && rPath == "")
      cmdstring += "make cschaden"
    if (cPath == "" && rPath != "")
      cmdstring += "make rschaden"

    val usingR = cmdstring.contains("r")
    val usingC = cmdstring.contains("cs") || cmdstring.contains("crs")

    assert(cmdstring != "", println("No simulators were specified"))
    
    val cfgopt = " CONFIG=\"" + cfgmap.mkString(" ") + "\""
    val cmtopt = " COMMIT=\"" + commitmap.mkString(" ") + "\""
    cmdstring += cfgopt + cmtopt
    if (email != "") cmdstring2 += " -e " + email
    if (thresh != -1) cmdstring2 += " -t " + thresh
    if (minutes != -1) cmdstring2 += " -m " + minutes
    cmdstring2 += "\""
    val tmpCmd = cmdstring + cmdstring2
    for (i <- 0 until instcnt)
    {
      if (i == 0) cmdRA(i) = tmpCmd
      else cmdRA(i) = ""
    }
    cmdRA
  }

  def runInstances(): Unit = 
  {
    //run instance 0. others are dummies. 
    logtime = System.currentTimeMillis
    for (i <- 0 until instcnt)
    {
      val instance = instRunners(i)
      val instDir = tmpDir + "/riscv-torture"
      val tortureDir = "."
      val config = cfgmap(i)
      if (i == 0) instance.createLogger(logtime)
      instance.copyTortureDir(tortureDir, instDir, config)
      if (i == 0)
      {
        println("Starting remote schadenfreude job.")
        processRA(i) = instance.run(cmdstrRA(i), instDir)
      }
    }
    println("\nRemote EC2 job has been launched.")
  }

  def collectLogFiles(): Unit =
  {
    for (i <- 0 until instcnt) instRunners(i).collectLogFile(permDir)
    instRunners(0).asInstanceOf[EC2Runner].stopEC2Instance()
  }
}

class BasicInstanceManager(val cfgs: List[String], val gitcmts: List[String], val permDir: String, val tmpDir: String, val cPath: String, val rPath: String, val email: String, val thresh: Int, val minutes: Int, val instcnt: Int, val insttype: String, ec2inst: Boolean) extends InstanceManager
{
  var logtime: Long = 0L
  if (ec2inst) 
  {
    val donefile = new File("EC2DONE")
    if (donefile.exists()) donefile.delete()
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
        if (usingR && insttype == "psi") tmpCmd += " R_SIM=../rocket_"+tmpcommit+"/vlsi-generic/build/vcs-sim-rtl.psi/simv"
        else if (usingR) tmpCmd += " R_SIM=../rocket_"+tmpcommit+"/vlsi-generic/build/vcs-sim-rtl/simv"
        if (usingC) tmpCmd += " C_SIM=../rocket_"+tmpcommit+"/emulator/emulator"
        if (cmdstring2 != " OPTIONS=\"\"") tmpCmd += cmdstring2
      }
      cmdRA(i) = tmpCmd
    }
    cmdRA
  }

  private def checkoutRocketPSI(commit: String, cPath: String, rPath: String, usingC: Boolean, usingR: Boolean): Unit =
  {
    def getRocketDir(empath: String, emtype: Char): String =
    {
      var rdir = ""
      val RA = empath.split("/")
      var flag = 0
      for (dir <- RA)
      {
        if (dir == "vlsi-generic" && emtype == 'r') return rdir.dropRight(1)
        if (dir == "emulator" && emtype == 'c') return rdir.dropRight(1)
        if (dir == "") rdir += "/"
        else rdir += dir + "/"
      }
      flag = 1
      assert(flag != 1, println("Invalid rocket path was given. " + empath))
      return ""
    }

    var rocketDir = ""
    if (usingC) rocketDir = getRocketDir(cPath, 'c')
    if (usingR) rocketDir = getRocketDir(rPath, 'r')
    if (rocketDir != "")
    {
      val rocketPath: Path = rocketDir
      if (commit != "none")
      {
        val remoteDir = tmpDir + "/rocket_" + commit
        val remoteOldPath: Path = rocketDir
        val remotePath: Path = remoteDir
        val remoteCPath: Path = remoteDir+"/emulator"
        val remoteRPath: Path = remoteDir+"/vlsi-generic/build/vcs-sim-rtl.psi"
        if (!fileop.remotePathExists(remotePath, "psi", ""))
        {
          fileop.gitcheckoutRemote(remoteOldPath, remotePath, commit, "psi", "")
          fileop.cleanRemote(remoteCPath, "psi", "")
          fileop.cleanRemote(remoteRPath, "psi", "")
        }
        if (usingR) fileop.compileRemote(remoteRPath, remoteRPath/Path("simv"), "psi", "")
        if (usingC) fileop.compileRemote(remoteCPath, remoteCPath/Path("emulator"), "psi", "")
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
    for (i <- 0 until instcnt) instRunners(i).collectLogFile(permDir)
    if (ec2inst)
    {
      val donefile = new File("EC2DONE")
      donefile.createNewFile()
    }
  }
} 
