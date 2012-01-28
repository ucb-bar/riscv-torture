package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqALU extends Seq
{
  def seq_src1(op: Opcode) = () =>
  {
    val src1 = reg_read_any()
    val dest = reg_write(src1)
    insts += op(dest, src1, src1)
  }

  def seq_src1_immfn(op: Opcode, immfn: () => Int) = () =>
  {
    val src1 = reg_read_any()
    val dest = reg_write(src1)
    val imm = Imm(immfn())
    insts += op(dest, src1, imm)
  }

  def seq_src1_zero(op: Opcode) = () =>
  {
    val src1 = reg_read_any()
    val dest = reg_write(src1)
    val tmp = reg_write_visible()
    insts += ADDI(tmp, reg_read_zero(), Imm(rand_imm()))
    insts += op(dest, tmp, tmp)
  }

  def seq_src2(op: Opcode) = () =>
  {
    val src1 = reg_read_any()
    val src2 = reg_read_any()
    val dest = reg_write(src1, src2)
    insts += op(dest, src1, src2)
  }

  def seq_src2_zero(op: Opcode) = () =>
  {
    val src1 = reg_read_any()
    val dest = reg_write(src1)
    val tmp1 = reg_write_visible()
    val tmp2 = reg_write_visible()
    insts += ADDI(tmp1, reg_read_zero(), Imm(rand_imm()))
    insts += ADDI(tmp2, reg_read_zero(), Imm(rand_imm()))
    insts += op(dest, tmp1, tmp2)
  }

  val candidates = new ArrayBuffer[() => insts.type]

  candidates += seq_src1_immfn(ADDI, rand_imm)
  candidates += seq_src1_immfn(SLLI, rand_shamt)
  candidates += seq_src1_immfn(SLTI, rand_imm)
  candidates += seq_src1_immfn(SLTIU, rand_imm)
  candidates += seq_src1_immfn(XORI, rand_imm)
  candidates += seq_src1_immfn(SRLI, rand_shamt)
  candidates += seq_src1_immfn(SRAI, rand_shamt)
  candidates += seq_src1_immfn(ORI, rand_imm)
  candidates += seq_src1_immfn(ANDI, rand_imm)
  candidates += seq_src1_immfn(LUI, rand_bigimm)
  candidates += seq_src1_immfn(ADDIW, rand_imm)
  candidates += seq_src1_immfn(SLLIW, rand_shamtw)
  candidates += seq_src1_immfn(SRLIW, rand_shamtw)
  candidates += seq_src1_immfn(SRAIW, rand_shamtw)

  val oplist = new ArrayBuffer[Opcode]

  oplist += (ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND)
  oplist += (MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU)
  oplist += (ADDW, SUBW, SLLW, SRLW, SRAW)
  oplist += (MULW, DIVW, DIVUW, REMW, REMUW)

  for (op <- oplist)
  {
    candidates += seq_src1(op)
    candidates += seq_src1_zero(op)
    candidates += seq_src2(op)
    candidates += seq_src2_zero(op)
  }

  rand_pick(candidates)()
}
