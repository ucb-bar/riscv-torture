package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class Prog
{
  val hwrp = new HWRegPool()
  val seqs = new ArrayBuffer[Seq]
  val seqs_active = new ArrayBuffer[Seq]
  val insts = new ArrayBuffer[Inst]

  def seqs_not_allocated = seqs.filter((x) => !x.allocated)
  def is_seqs_empty = seqs_not_allocated.length == 0
  def is_seqs_active_empty = seqs_active.length == 0

  def seqs_find_active(): Unit =
  {
    for (seq <- seqs_not_allocated)
    {
      if (seq.allocate_regs(hwrp, false))
      {
        seq.allocate_regs(hwrp, true)
        seqs_active += seq
      }
      else
      {
        return
      }
    }
  }

  def generate(nseqs: Int, memsize: Int) =
  {
    hwrp.init()

    for (i <- 0 to nseqs)
      seqs += Seq(memsize)

    while (!is_seqs_empty)
    {
      seqs_find_active()

      while (!is_seqs_active_empty)
      {
        val seq = rand_array[Seq](seqs_active)
        insts += seq.next_inst()

        if (seq.is_done)
        {
          seq.free_regs()
          seqs_active -= seq
        }
      }
    }

    insts.map(println)
  }
}
