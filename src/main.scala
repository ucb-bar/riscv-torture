package torture


import java.io.FileWriter
import java.io.FileInputStream
import java.util.Properties
import scala.collection.JavaConversions._

object torture extends Application
{
  override def main(args: Array[String]) =
  {
    val config = new Properties()
    val in = new FileInputStream("config")
    config.load(in)
    in.close()
    val nseqs   = config.getProperty("torture.nseqs", "1000").toInt
    val memsize = config.getProperty("torture.memsize", "1024").toInt
    val mix     = config.filterKeys(_ contains "torture.mix").map { case (k,v) => (k.split('.')(2), v.toInt) }.asInstanceOf[Map[String,Int]]

    assert (mix.values.sum == 100, println("The instruction mix specified in config does not add up to 100%"))
    assert (mix.keys.forall(List("xmem","xbranch","xalu") contains _), println("The instruction mix specified in config contains an unknown sequence type name")) 

    val prog = new Prog()
    val s = prog.generate(nseqs, memsize, mix)

    val fw = new FileWriter("output/test.S")
    fw.write(s)
    fw.close()
  }
}
