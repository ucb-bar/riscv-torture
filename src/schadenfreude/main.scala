package torture
package schadenfreude

import scopt.OptionParser
import scala.sys.process._
import scalax.file.Path
import scalax.file.FileSystem
import java.io.File
import java.io.FileWriter
import java.util.Properties
import java.io.FileInputStream

val DefInstType = "local"

val DefInstCnt = 1
val DefTemp = ".."

case class Options(var timeToRun: Int = -1,
  var cfgFile: String = "config",
  var emailAddress: String = "",
  var errorThreshold: Int = -1,
  var cSimPath: String = "",
  var rtlSimPath: String = "",
  var permDir: String = "",
  var tempDir: String = DefTemp,
  var confFileList: List[String] = List(),
  var gitCommitList: List[String] = List(),
  var instanceType: String = DefInstType,
  var instanceCnt: Int = DefInstCnt,
  var ec2Instance: Boolean = false)

object Schadenfreude extends App
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser[Options]("schadenfreude/run") {
      opt[String]('C', "config") valueName("<file>") text("config file") action {(s: String, c) => c.copy(confFileName = s)}
      opt[Seq[String]]('f', "configfiles") valueName("<config1>,<config2>,...") text("Config files for instances") action {(s:Seq[String], c) =>c.copy(confFileList = s)}
      opt[Seq[String]]('g', "gitcommit") valueName("<commit1>,<commit2>,...") text("git commits to check out") action {(s: Seq[String], c) => c.copy(gitCommitList = s)}
      opt[String]('p', "permdir") valueName("<dir>") text("dir to store failing tests") action {(s: String, c) => c.copy(permDir = s)}
      opt[String]('d', "instdir") valueName("<dir>") text("dir to create instance dirs in") aciton {(s: String, c) => c.copy(tempDir = s}
      opt[String]('c', "csim") valueName("<file>") text("C simulator") action {(s: String, c) => c.copy(cSimPath = s)}
      opt[String]('r', "rtlsim") valueName("<file>") text("RTL simulator") action {(s: String, c) => c.copy(rtlSimPath = s)}
      opt[String]('e', "email") valueName("<address>") text("email to report to") action {(s: String, c) => c.copy(emailAddress = s)}
      opt[String]('i', "insttype") valueName("<type>") text("Type of instance to run") action {(s: String, c) => c.copy(instanceType = s}
      opt[Int]('t', "threshold") valueName("<count>") text("number of failures to trigger email") action {(i: Int, c) => c.copy(errorThreshold = i)}
      opt[Int]('m', "minutes") valueName("<minutes>") text("number of minutes to run tests") action {(i: Int, c) => c.copy(timeToRun = i)}
      opt[Int]('n', "instcnt") valueName("<int>") text("number of instances to run") action {(i: Int, c) => c.copy(instanceCnt = i)}
      opt[Unit]("ec2instance")abbr("ec2") text("Running on EC2") action {(_, c) => c.copy(ec2Instance = true)}
    }
    parser.parse(args, Options()) match {
    case Some(opts) =>
    {
      val cfgFile = opts.cfgFile
      val confFileList = opts.confFileList
      val gitCommitList = opts.gitCommitList
      val config = new Properties()
      val configin = new FileInputStream(cfgFile)
      config.load(configin)
      configin.close()

      val typeinst = Option(config.getProperty("torture.schadenfreude.insttype"))
      val numinst  = Option(config.getProperty("torture.schadenfreude.instcnt")) map ( toInt )
      val tmpdir   = Option(config.getProperty("torture.schadenfreude.instdir"))

      val instdir      = if(opts.tempDir == DefTemp) tmpdir.getOrElse(DefTemp) else opts.tempDir
      val insttype     = if(opts.instanceType == DefInstType) typeinst.getOrElse(DefInstType) else opts.instanceType
      val instcnt      = if(opts.instanceCnt == DefInstCnt) numinst.getOrElse(DefInstCnt) else opts.instanceCnt

      val permDir      = opts.permDir
      val cPath        = opts.cSimPath
      val rPath        = opts.rtlSimPath
      val email        = opts.emailAddress
      val thresh       = opts.errorThreshold
      val minutes      = opts.timeToRun
      val ec2inst      = opts.ec2Instance

      val instmgr = InstanceManager(confFileList, gitCommitList, permDir, instdir, cPath, rPath, email, thresh, minutes, instcnt, insttype, ec2inst)
      instmgr.createInstances()
      instmgr.runInstances()
      instmgr.waitOnInstances()
      instmgr.collectFiles()
    }
    case None =>
      System.exit(1) // error message printed by parser
  }

}
