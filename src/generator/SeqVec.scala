package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

object SeqVec
{
  var cnt = 0
  def get_id = (cnt += 1)
}


class SeqVec(xregs: HWRegPool, vxregs: HWRegPool, vfregs_s: HWRegPool, vfregs_d: HWRegPool, vl: Int, memsize: Int) extends InstSeq
{
  val name = "seqvec_" + SeqVec.cnt
  SeqVec.cnt += 1
  override def toString = name

  val xreg_helper = reg_write_hidden(xregs)
  val vec_mem = new Mem(name+"_mem", memsize)

  val vxreg_test = reg_write_visible(vxregs)
  val test_init1_mem = new Mem(Array(Label(name+"_"), vxreg_test, Label("_init"))  , 8*vl)
  val test_out1_mem  = new Mem(Array(Label(name+"_"), vxreg_test, Label("_output")), 8*vl)

  val vf_test = new ProgSeg(name+"_vf_1")
  vf_test.insts += ILLEGAL(Label("0x%08x" format rand_word))
  vf_test.insts += STOP()

  extra_code += ProgSegDump(vf_test)

  extra_hidden_data  += MemDump(test_init1_mem)
  extra_visible_data += MemDump(test_out1_mem)
  extra_visible_data += MemDump(vec_mem)

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
