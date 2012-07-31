package torture
package schadenfreude

import scala.sys.process._

class InstanceManager(val cfgs: List[String], val gitcmts: List[String], val permDir: String, val tmpDir: String, val cPath: String, val rPath: String, val email: String, val thresh: Int, val minutes: Int, val instcnt: Int)
{
  val instRunners: Array[InstanceRunner] = new Array(instcnt)
  val processRA: Array[Process] = new Array(instcnt)
  val cfgmap = mapOptions(cfgs,"config")
  val commitmap = mapOptions(gitcmts,"none")
  val cmdstrRA: Array[String] = getCommandStrings()

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

  def getCommandStrings(): Array[String] = 
  {
    val cmdRA: Array[String] = new Array(instcnt)
    var cmdstring = ""
    var cmdstring2 = ""
    if (cPath != "" && rPath != "")
      cmdstring += "make crnight"
    if (cPath != "" && rPath == "")
      cmdstring += "make cnight"
    if (cPath == "" && rPath != "")
      cmdstring += "make rnight"

    assert(cmdstring != "", println("No simulators were specified"))

    if (email != "your@email.address") cmdstring2 += "e EMAIL=" + email
    if (cPath != "../riscv-rocket/emulator/emulator" && cPath != "") cmdstring2 += " C_SIM=" + cPath
    if (rPath != "../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv" && rPath != "") cmdstring2 += " R_SIM=" + rPath
    cmdstring2 += " ERRORS=" + thresh + " MINUTES=" + minutes
    cmdstring2 += " DIR=" + permDir

    for (i <- 0 until instcnt)
    {
      var tmpCmd = cmdstring
      if (commitmap(i) != "none") tmpCmd += "g" + cmdstring2
      if (commitmap(i) != "none") tmpCmd += " COMMIT=" + commitmap(i)
      cmdRA(i) = tmpCmd
    }
    cmdRA
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
