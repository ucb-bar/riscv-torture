package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqSeq(xregs: HWRegPool, fregs_s: HWRegPool, fregs_d: HWRegPool, mem: Mem) extends InstSeq
{
  val sub_seq = new SeqALU(xregs)
  if(!sub_seq.allocate_regs()) println("error: could not allocate sub-sequence")
  
  while(!sub_seq.is_done)
  {
    insts += sub_seq.next_inst()
  }
  sub_seq.free_regs()
}
