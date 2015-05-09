package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqVALU(xregs: HWRegPool, use_mul: Boolean, use_div: Boolean) extends InstSeq //TODO: better configuration
{
  override val seqname = "valu"
  def seq_src1(op: Opcode) = () =>
  {
    val src1 = reg_read_any(xregs)
    val dest = reg_write(xregs, src1)
    insts += op(dest, src1, src1)
  }

  def seq_src2(op: Opcode) = () =>
  {
    val src1 = reg_read_any(xregs)
    val src2 = reg_read_any(xregs)
    val dest = reg_write(xregs, src1, src2)
    insts += op(dest, src1, src2)
  }

  val candidates = new ArrayBuffer[() => insts.type]

  val oplist = new ArrayBuffer[Opcode]

  oplist += (VADD, VSUB, VSLL, VXOR, VSRL, VSRA, VOR, VAND)
  oplist += (VADDW, VSUBW, VSLLW, VSRLW, VSRAW)
  if (use_mul) oplist += (VMUL, VMULH, VMULHSU, VMULHU, VMULW)
  if (use_div) oplist += (VDIV, VDIVU, VREM, VREMU, VDIVW, VDIVUW, VREMW, VREMUW)

  for (op <- oplist)
  {
    candidates += seq_src1(op)
    candidates += seq_src2(op)
  }

  System.out.println("valu")
  rand_pick(candidates)()
}
