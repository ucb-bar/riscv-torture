package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

// global variables may not be the ideal interface for this
import Prog.extra_hidden_data
import Prog.extra_visible_data

object SeqVec
{
  var cnt = 0
  def get_id = (cnt += 1)
}


class SeqVec(xregs: HWRegPool, vxregs: HWRegPool, vfregs_s: HWRegPool, vfregs_d: HWRegPool, vl: Int, memsize: Int) extends InstSeq
{
  val name = "seqvec_" + SeqVec.cnt
  SeqVec.cnt += 1

  val xreg_helper = reg_write_hidden(xregs)
  val vec_mem = new Mem(name+"_mem", memsize)

  val vxreg_test = reg_write_visible(vxregs)
  val test_init1_mem = new Mem(Array(Label(name+"_"), vxreg_test, Label("_init"))  , 8*vl)
  val test_out1_mem  = new Mem(Array(Label(name+"_"), vxreg_test, Label("_output")), 8*vl)

  Prog.extra_hidden_data  += MemDump(test_init1_mem)
  Prog.extra_visible_data += MemDump(test_out1_mem)
  Prog.extra_visible_data += MemDump(vec_mem)

  insts += LA(xreg_helper, test_init1_mem)
  insts += VLD(vxreg_test, xreg_helper)

  insts += LA(xreg_helper, test_out1_mem)
  insts += VSD(vxreg_test, xreg_helper)
  insts += FENCE_V_L(Label("// " + name))

  override def allocate_regs(): Boolean =
  {
    if(super.allocate_regs())
      // Give registers in my shadow register pools names
      return true
    else
      return false
  }
}
