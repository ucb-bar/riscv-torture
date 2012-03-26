package torture
package overnight

import java.net.InetAddress
import java.util.Properties._
import javax.mail._
import javax.mail.internet._
import scala.collection.JavaConversions._
import scala.sys.process._
import scalax.file.Path
import Path._
import scalax.file.PathSet
import scalax.file.FileSystem
import scopt.OptionParser

case class Options(var timeToRun: Option[Int] = None,
  var emailAddress: Option[String] = None,
  var errorThreshold: Option[Int] = None,
  var cSimPath: Option[String] = None,
  var rtlSimPath: Option[String] = None,
  var permDir: Option[String] = None,
  var confFileName: Option[String] = None)

object Overnight extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("overnight/run") {
      opt("C", "config", "<file>", "config file", {s: String => opts.confFileName = Some(s)})
      opt("p", "permdir", "<dir>", "dir to store failing tests", {s: String => opts.permDir = Some(s)})
      opt("c", "csim", "<file>", "C simulator", {s: String => opts.cSimPath = Some(s)})
      opt("r", "rtlsim", "<file>", "RTL simulator", {s: String => opts.rtlSimPath = Some(s)})
      opt("e", "email", "<address>", "email to report to", {s: String => opts.emailAddress = Some(s)})
      opt("t", "threshold", "<count>", "number of failures to trigger email", {i: String => opts.errorThreshold = Some(i.toInt)})
      opt("m", "minutes", "<int>", "number of minutes to run tests", {i: String => opts.timeToRun = Some(i.toInt)})
    }
    if (parser.parse(args)) {
      val confFileName = opts.confFileName.getOrElse("config")
      val permDir      = opts.permDir.getOrElse("output")
      val thresh       = opts.errorThreshold.getOrElse(5)
      val minutes      = opts.timeToRun.getOrElse(1)
      val startTime = System.currentTimeMillis
      var endTime = startTime + minutes*60*1000
      var errCount = 0
      while(System.currentTimeMillis < endTime) {
        val baseName = "test_" + System.currentTimeMillis
        val newAsmName = generator.Generator.generate(confFileName, baseName)
        val (failed, test) = testrun.TestRunner.testrun( Some(newAsmName), opts.cSimPath, opts.rtlSimPath, Some(true), Some(true), Some(confFileName)) 
        if(failed) {
          errCount += 1
          test foreach { t =>
            println(t)
            println(t.last)
            val permFiles:PathSet[Path] = Path(t.init:_*) * (t.last + "*")
            println(permFiles.mkString)
            permFiles.foreach( f => f.copyTo( permDir / f.name))
          }
        } 
        test foreach { t =>
          val targetFiles:PathSet[Path] = Path(t.init:_*) * (baseName+"*")
          targetFiles.foreach(_.delete())
        }
        if(errCount == thresh) {
          println("////////////////////////////////////////////////////////////////")
          println("//  Aborting test runs due to error threshold being exceeded  //")
          println("////////////////////////////////////////////////////////////////")
          endTime = 0
        }
      }
      opts.emailAddress foreach { addr =>
        val properties = System.getProperties
        properties.put("mail.smtp.host", "localhost")
        val hostname = InetAddress.getLocalHost().getHostName()
        val session = Session.getDefaultInstance(properties)
        val message = new MimeMessage(session)
        message.setFrom(new InternetAddress("torture@"+hostname+".millennium.berkeley.edu"))
        message.setRecipients(Message.RecipientType.TO, addr)
        message.setText( "Run complete with " + errCount + " errors. Failing tests put in " +  permDir )
        message.setSubject("Run complete on " + hostname)
        println("////////////////////////////////////////////////////////////////")
        println("//  Sending " + message + " to " + addr)
        println("////////////////////////////////////////////////////////////////")
        Transport.send(message)
      }
      println("////////////////////////////////////////////////////////////////")
      println("//  Testing complete with " + errCount + " errors.")
      println("//  Failing tests put in " + permDir)
      println("////////////////////////////////////////////////////////////////")
    }
  }
}

