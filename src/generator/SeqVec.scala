package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

// global variables may not be the ideal interface for this
import Prog.extra_hidden_data
import Prog.extra_visible_data

object SeqVec
{
  var cnt = 0
}


class SeqVec(xregs: HWRegPool, vxregs: HWRegPool, vfregs_s: HWRegPool, vfregs_d: HWRegPool, vl: Int, memsize: Int) extends InstSeq
{
  val name = "seqvec_" + SeqVec.cnt
  SeqVec.cnt += 1

  val xreg_helper = reg_write_hidden(xregs)
  val vec_mem = new Mem(name+"_mem", memsize)

  val vxreg_test = reg_write_visible(vxregs)
  val test_init1_mem = new Mem(name+"_vxreg1_"+"init", 8*vl)
  val test_out1_mem  = new Mem(name+"_out", 8*vl)

  Prog.extra_hidden_data  += test_init1_mem
  Prog.extra_visible_data += test_out1_mem
  Prog.extra_visible_data += vec_mem

  insts += LA(xreg_helper, Label(test_init1_mem.name))
  insts += VLD(vxreg_test, xreg_helper)

  insts += LA(xreg_helper, Label(test_out1_mem.name))
  insts += VSD(vxreg_test, xreg_helper)
  insts += FENCE_V_L(Label("// " + name))
}
