package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqSeq(xregs: HWRegPool, fregs_s: HWRegPool, fregs_d: HWRegPool, mem: Mem, nseqs: Int, mixcfg: Map[String,Int]) extends InstSeq
{
  val seqs = new ArrayBuffer[InstSeq]
  val seqs_active = new ArrayBuffer[InstSeq]
  var killed_seqs = 0

  def seqs_not_allocated = seqs.filter((x) => !x.allocated)
  def is_seqs_empty = seqs_not_allocated.length == 0
  def is_seqs_active_empty = seqs_active.length == 0

  def are_pools_fully_unallocated = List(xregs, fregs_s, fregs_d).forall(_.is_fully_unallocated)

  val name_to_seq = Map(
    "xmem" -> (() => new SeqMem(xregs, mem)),
    "vmem" -> (() => new SeqVMem(xregs, mem.asInstanceOf[VMem])), // TODO: Clean up
    "xalu" -> (() => new SeqALU(xregs, false)), //false means no divider, TODO: make better 
    "fgen" -> (() => new SeqFPU(fregs_s, fregs_d)),
    "fax" -> (() => new SeqFaX(xregs, fregs_s, fregs_d)),
    "vonly" -> (() => new SeqVOnly(xregs, fregs_s, fregs_d)))

  val prob_tbl = new ArrayBuffer[(Int, () => InstSeq)]
  mixcfg foreach {case(name, prob) => (prob_tbl += ((prob, name_to_seq(name))))}

  def gen_seq(): Unit =
  {
    val nxtseq = InstSeq(prob_tbl)
    seqs += nxtseq
    xregs.backup()
    fregs_s.backup()
    fregs_d.backup()
    if (!nxtseq.allocate_regs())
    {
      seqs -= nxtseq
      killed_seqs += 1
      if (killed_seqs < (nseqs*5)) //TODO: get a good metric
        gen_seq()
    }
    xregs.restore()
    fregs_s.restore()
    fregs_d.restore()
  }
 
  for(i <- 1 to nseqs) gen_seq()

  def seqs_find_active(): Unit =
  {
    for (seq <- seqs_not_allocated)
    {
      xregs.backup()
      fregs_s.backup()
      fregs_d.backup()

      if (seq.allocate_regs())
      {
        seqs_active += seq
      }
      else
      {
        xregs.restore()
        fregs_s.restore()
        fregs_d.restore()

        return
      }
    }
  }

  
  while(!is_seqs_empty)
  {
    seqs_find_active()

    while(!is_seqs_active_empty)
    {
      val seq = rand_pick(seqs_active)
      insts += seq.next_inst()

      if(seq.is_done)
      {
        seq.free_regs()
        seqs_active -= seq
      }
    }
  }

  if(killed_seqs >= (nseqs*5))
    println("warning: a SeqSeq killed an excessive number of sequences. (#X=%d, #Fs=%d, #Fd=%d)" format (xregs.size, fregs_s.size, fregs_d.size))
}
