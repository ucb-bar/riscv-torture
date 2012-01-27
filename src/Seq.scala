package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class Seq extends HWRegAllocator
{
  val insts = new ArrayBuffer[Inst]
  var inst_ptr = 0

  def is_done = insts.length == inst_ptr

  def next_inst() =
  {
    val inst = insts(inst_ptr)
    inst_ptr += 1
    inst
  }
}

object Seq
{
  def apply(memsize: Int): Seq =
  {
    val candidates = new ArrayBuffer[(Int, () => Seq)]

    candidates += ((40, () => new SeqMem(memsize)))
    candidates += ((10, () => new SeqBranch()))

    var p = rand_range(0, 99)

    for (candidate <- candidates)
    {
      if (p < candidate._1) return candidate._2()
      p -= candidate._1
    }

    return new SeqALU()
  }
}
