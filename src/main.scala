package torture

import java.io.FileWriter

object torture extends Application
{
  override def main(args: Array[String]) =
  {
    val nseqs = 10000
    val memsize = 1024

    val prog = new Prog()
    val s = prog.generate(nseqs, memsize)

    val fw = new FileWriter("output/test.S")
    fw.write(s)
    fw.close()
  }
}
