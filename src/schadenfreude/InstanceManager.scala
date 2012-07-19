package torture
package schadenfreude

import scala.sys.process._

class InstanceManager(cfgs: List[String], permDir: String, tmpDir: String, cPath: String, rPath: String, email: String, thresh: Int, minutes: Int, instcnt: Int)
{
  val instRunners: Array[InstanceRunner] = new Array(instcnt)
  val processRA: Array[Process] = new Array(instcnt)
  val cfgmap = mapConfigFiles()
  val cmdstr = getCommandString()

  def mapConfigFiles(): List[String] =
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
    cmdstring += " ERRORS=" + thresh + " MINUTES=" + minutes
    cmdstring += " DIR=" + permDir
    cmdstring
  }

  def createInstances(insttype: String): Unit = 
  {
    for (i <- 0 until instcnt) instRunners(i) = InstanceRunner(insttype,i)
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
      println("Starting instance %d".format(i))
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
