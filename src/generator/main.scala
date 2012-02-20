package torture
package generator

import scopt.OptionParser
import java.io.FileWriter
import java.io.FileInputStream
import java.util.Properties
import scala.collection.JavaConversions._

case class Options(var outFileName: Option[String] = None,
  var confFileName: Option[String] = None)

object Generator extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("generator/run") {
      opt("o", "output", "<filename>", "output filename", {s: String => opts.outFileName = Some(s)})
      opt("C", "config", "<file>", "config file", {s: String => opts.confFileName = Some(s)})
    }
    if (parser.parse(args)) {
      generate(opts.confFileName.getOrElse("config"), opts.outFileName.getOrElse("test"))
    }
  }

  def generate(confFile: String, outFileName: String): String = {
    val config = new Properties()
    val in = new FileInputStream(confFile)
    config.load(in)
    in.close()
    val nseqs   = config.getProperty("torture.nseqs", "1000").toInt
    val memsize = config.getProperty("torture.memsize", "1024").toInt
    val mix     = config.filterKeys(_ contains "torture.mix").map { case (k,v) => (k.split('.')(2), v.toInt) }.asInstanceOf[Map[String,Int]]
    generate(nseqs, memsize, mix, outFileName)
  }

  def generate(nseqs: Int, memsize: Int, mix: Map[String,Int], outFileName: String): String = {
    assert (mix.values.sum == 100, println("The instruction mix specified in config does not add up to 100%"))
    assert (mix.keys.forall(List("xmem","xbranch","xalu") contains _), println("The instruction mix specified in config contains an unknown sequence type name")) 

    val prog = new Prog()
    ProgSeg.cnt = 0
    val s = prog.generate(nseqs, memsize, mix)

    val oname = "output/" + outFileName + ".S"
    val fw = new FileWriter(oname)
    fw.write(s)
    fw.close()
    oname
  }
}

