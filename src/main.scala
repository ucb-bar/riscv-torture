package torture

import scala.collection.mutable.ArrayBuffer

object torture extends Application
{
  override def main(args: Array[String]) =
  {
    val hwrp = new HWRegPool()
    hwrp.init()

    val seqs = new ArrayBuffer[Seq]
    seqs += new SeqMem(1024)
    seqs += new SeqALU()
    seqs += new SeqBranch()
    seqs.map(x => x.allocate_regs(hwrp, true))
    seqs.map(x => {for (inst <- x.insts) println(inst)})
  }
}
