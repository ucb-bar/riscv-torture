package torture
package schadenfreude

import scopt.OptionParser
import scala.sys.process._
import scalax.file.Path
import scalax.file.FileSystem
import java.io.File
import java.io.FileWriter

case class Options(var timeToRun: Option[Int] = None,
  var emailAddress: Option[String] = None,
  var errorThreshold: Option[Int] = None,
  var cSimPath: Option[String] = None,
  var rtlSimPath: Option[String] = None,
  var permDir: Option[String] = None,
  var tempDir: Option[String] = None,
  var confFileList: Option[List[String]] = Some(List()),
  var instanceType: Option[String] = None,
  var instanceCnt: Option[Int] = None)

object Schadenfreude extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("schadenfreude/run") {
      opt("C", "config", "<file>", "config file", {s: String => opts.confFileList = Some(opts.confFileList.get ++ List(s))})
      opt("p", "permdir", "<dir>", "dir to store failing tests", {s: String => opts.permDir = Some(s)})
      opt("d", "tmpdir", "<dir>", "dir to create temporary instance dirs in", {s: String => opts.tempDir = Some(s)})
      opt("c", "csim", "<file>", "C simulator", {s: String => opts.cSimPath = Some(s)})
      opt("r", "rtlsim", "<file>", "RTL simulator", {s: String => opts.rtlSimPath = Some(s)})
      opt("e", "email", "<address>", "email to report to", {s: String => opts.emailAddress = Some(s)})
      opt("i", "insttype", "<type>", "Type of instance to run", {s: String => opts.instanceType = Some(s)})
      opt("t", "threshold", "<count>", "number of failures to trigger email", {i: String => opts.errorThreshold = Some(i.toInt)})
      opt("m", "minutes", "<int>", "number of minutes to run tests", {i: String => opts.timeToRun = Some(i.toInt)})
      opt("n", "instcnt", "<int>", "number of instances to run", {i: String => opts.instanceCnt = Some(i.toInt)})
    }
    if (parser.parse(args))
    {
      val confFileList = opts.confFileList.get
      val permDir      = opts.permDir.getOrElse("output/failedtests")
      val tmpDir       = opts.tempDir.getOrElse("..")
      val cPath        = opts.cSimPath.getOrElse("")
      val rPath        = opts.rtlSimPath.getOrElse("")
      val email        = opts.emailAddress.getOrElse("your@email.address")
      val insttype     = opts.instanceType.getOrElse("local")
      val thresh       = opts.errorThreshold.getOrElse(5)
      val minutes      = opts.timeToRun.getOrElse(1)
      val instcnt      = opts.instanceCnt.getOrElse(1)

      val instmgr = new InstanceManager(confFileList, permDir, tmpDir, cPath, rPath, email, thresh, minutes, instcnt)
      instmgr.createInstances(insttype)
      instmgr.runInstances()
    }
  }

}
