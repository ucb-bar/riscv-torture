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
  def names = List("mem","branch","alu")
  def apply(memsize: Int, mix: Map[String, Int]): InstSeq =
  {
    val name_to_gen: Map[String, () => InstSeq] = Map( "mem" -> (() => new SeqMem(memsize)),
                                                       "branch" -> (() => new SeqBranch()),
                                                       "alu" -> (() => new SeqALU()))
    var p = rand_range(0, 99)
    for ((name, prob) <- mix)
    {
      if (p < prob) return name_to_gen(name)()
      p -= prob
    }

    assert(false, println("Probabilties should have added up to 100%"))
    new SeqALU()
  }
}
