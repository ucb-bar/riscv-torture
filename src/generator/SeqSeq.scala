package torture

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import Rand._

class SeqSeq(xregs: HWRegPool, fregs_s: HWRegPool, fregs_d: HWRegPool, mem: Mem, nseqs: Int, mixcfg: Map[String,Int], use_amo: Boolean, use_mul: Boolean, use_div: Boolean) extends InstSeq
{
  val seqs = new ArrayBuffer[InstSeq]
  val seqs_active = new ArrayBuffer[InstSeq]
  var killed_seqs = 0
  val seqstats = new HashMap[String,Int].withDefaultValue(0)

  def seqs_not_allocated = seqs.filter((x) => !x.allocated)
  def is_seqs_empty = seqs_not_allocated.length == 0
  def is_seqs_active_empty = seqs_active.length == 0

  def are_pools_fully_unallocated = List(xregs, fregs_s, fregs_d).forall(_.is_fully_unallocated)

  val name_to_seq = Map(
    "xmem" -> (() => new SeqMem(xregs, mem, use_amo)),
    "vmem" -> (() => new SeqVMem(xregs, mem.asInstanceOf[VMem])), // TODO: Clean up
    "valu" -> (() => new SeqVALU(xregs, use_mul, use_div)), // TODO: Clean up
    "xalu" -> (() => new SeqALU(xregs, use_mul, use_div)),
    "fgen" -> (() => new SeqFPU(fregs_s, fregs_d)),
    "fax" -> (() => new SeqFaX(xregs, fregs_s, fregs_d)),
    "vonly" -> (() => new SeqVOnly(xregs, fregs_s, fregs_d)))

  val prob_tbl = new ArrayBuffer[(Int, () => InstSeq)]
  mixcfg foreach {case(name, prob) => (prob_tbl += ((prob, name_to_seq(name))))}

  def gen_seq(): Unit =
  {
    val nxtseq = InstSeq(prob_tbl)
    seqs += nxtseq
    seqstats(nxtseq.seqname) += 1
  }
 
  def seqs_find_active(): Unit =
  {
    System.out.println("seqseq.seq_find_active")
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

  for(i <- 1 to nseqs) gen_seq()
  
  System.out.println("finished seqseq.gen_seq")
  while(!is_seqs_empty)
  {
    System.out.println("!is_seqs_empty")
    seqs_find_active()

    while(!is_seqs_active_empty)
    {
      System.out.println("seqseq.rand pick and add inst")
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
