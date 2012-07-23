package torture
package schadenfreude

import scala.sys.process._

class InstanceManager(val cfgs: List[String], val permDir: String, val tmpDir: String, val cPath: String, val rPath: String, val email: String, val thresh: Int, val minutes: Int, val instcnt: Int)
{
  val instRunners: Array[InstanceRunner] = new Array(instcnt)
  val processRA: Array[Process] = new Array(instcnt)
  val cfgmap = mapConfigFiles()
  val cmdstr = getCommandString()

  def mapConfigFiles(): List[String] =
  {
    val cfgcnt = cfgs.length
    assert (cfgcnt <= instcnt, println("Number of config files was greater than the number of instances specified"))
    var map: List[String] = List()
    if (cfgcnt == 0)
    {
      for (i <- 0 until instcnt) map ++= List("config")
    } else {
      for (i <- 0 until instcnt)
      {
        val cfgindx = i % cfgcnt
        map ++= List(cfgs(cfgindx))
      }
    }
    map
  }

  def getCommandString(): String = 
  {
    var cmdstring = ""
    if (cPath != "" && rPath != "")
      cmdstring += "make crnight"
    if (cPath != "" && rPath == "")
      cmdstring += "make cnight"
    if (cPath == "" && rPath != "")
      cmdstring += "make rnight"

    assert(cmdstring != "", println("No simulators were specified"))

    if (email != "your@email.address") cmdstring += "e EMAIL=" + email
    if (cPath != "../riscv-rocket/emulator/emulator" && cPath != "") cmdstring += " C_SIM=" + cPath
    if (rPath != "../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv" && rPath != "") cmdstring += " R_SIM=" + rPath
    cmdstring += " ERRORS=" + thresh + " MINUTES=" + minutes
    cmdstring += " DIR=" + permDir
    cmdstring
  }

  def createInstances(insttype: String): Unit = 
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
      println("instRunners has length " + instRunners.length)
      val instance = instRunners(i)
      val instDir = tmpDir + "/schad" + i
      val tortureDir = "."
      val config = cfgmap(i)
      instance.copyTortureDir(tortureDir, instDir, config)
      instance.createLogger(logtime)
      processRA(i) = instance.run(cmdstr, instDir)
    }
    println("\nAll instances have been launched.")
  }

} 
