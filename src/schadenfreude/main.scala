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

case class Options(var timeToRun: Option[Int] = None,
  var cfgFile: Option[String] = None,
  var emailAddress: Option[String] = None,
  var errorThreshold: Option[Int] = None,
  var cSimPath: Option[String] = None,
  var rtlSimPath: Option[String] = None,
  var permDir: Option[String] = None,
  var tempDir: Option[String] = None,
  var confFileList: Option[List[String]] = Some(List()),
  var gitCommitList: Option[List[String]] = Some(List()),
  var instanceType: Option[String] = None,
  var instanceCnt: Option[Int] = None,
  var ec2Instance: Option[Boolean] = None)

object Schadenfreude extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("schadenfreude/run") {
      opt("C", "config", "<file>", "config file", {s: String => opts.cfgFile=Some(s)})
      opt("f", "configfiles", "<config files>", "Config files for instances",{s:String =>opts.confFileList = Some(opts.confFileList.get ++ List(s))})
      opt("g", "gitcommit", "<git commit>", "git commits to check out", {s: String => opts.gitCommitList = Some(opts.gitCommitList.get ++ List(s))})
      opt("p", "permdir", "<dir>", "dir to store failing tests", {s: String => opts.permDir = Some(s)})
      opt("d", "instdir", "<dir>", "dir to create instance dirs in", {s: String => opts.tempDir = Some(s)})
      opt("c", "csim", "<file>", "C simulator", {s: String => opts.cSimPath = Some(s)})
      opt("r", "rtlsim", "<file>", "RTL simulator", {s: String => opts.rtlSimPath = Some(s)})
      opt("e", "email", "<address>", "email to report to", {s: String => opts.emailAddress = Some(s)})
      opt("i", "insttype", "<type>", "Type of instance to run", {s: String => opts.instanceType = Some(s)})
      opt("t", "threshold", "<count>", "number of failures to trigger email", {i: String => opts.errorThreshold = Some(i.toInt)})
      opt("m", "minutes", "<int>", "number of minutes to run tests", {i: String => opts.timeToRun = Some(i.toInt)})
      opt("n", "instcnt", "<int>", "number of instances to run", {i: String => opts.instanceCnt = Some(i.toInt)})
      booleanOpt("ec2", "ec2instance", "<boolean>", "Running on EC2", {b: Boolean => opts.ec2Instance = Some(b)})
    }
    if (parser.parse(args))
    {
      val cfgFile = opts.cfgFile.getOrElse("config")
      val confFileList = opts.confFileList.get
      val gitCommitList = opts.gitCommitList.get
      val config = new Properties()
      val configin = new FileInputStream(cfgFile)
      config.load(configin)
      configin.close()

      val typeinst = config.getProperty("torture.schadenfreude.insttype","local")
      val numinst  = config.getProperty("torture.schadenfreude.instcnt","1").toInt
      val tmpdir   = config.getProperty("torture.schadenfreude.instdir","..")

      val instdir       = opts.tempDir.getOrElse(tmpdir)
      val insttype     = opts.instanceType.getOrElse(typeinst)
      val instcnt      = opts.instanceCnt.getOrElse(numinst)

      val permDir      = opts.permDir.getOrElse("")
      val cPath        = opts.cSimPath.getOrElse("")
      val rPath        = opts.rtlSimPath.getOrElse("")
      val email        = opts.emailAddress.getOrElse("")
      val thresh       = opts.errorThreshold.getOrElse(-1)
      val minutes      = opts.timeToRun.getOrElse(-1)
      val ec2inst      = opts.ec2Instance.getOrElse(false)

      val instmgr = InstanceManager(confFileList, gitCommitList, permDir, instdir, cPath, rPath, email, thresh, minutes, instcnt, insttype, ec2inst)
      instmgr.createInstances()
      instmgr.runInstances()
      instmgr.waitOnInstances()
      instmgr.collectFiles()
    }
  }

}
