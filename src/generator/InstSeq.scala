package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class InstSeq extends HWRegAllocator
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

object InstSeq
{
  def apply(prob_tbl: ArrayBuffer[(Int, () => InstSeq)]): InstSeq =
  {
    var p = rand_range(0, 99)
    for ((prob, gen_seq) <- prob_tbl)
    {
      if (p < prob) return gen_seq()
      p -= prob
    }

    assert(false, println("Probabilties should have added up to 100%"))
    new InstSeq()
  }
}
