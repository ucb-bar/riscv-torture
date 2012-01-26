package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqBranch extends Seq
{
  val label_taken = Label("__needs_patch")
  val label_nottakens = ArrayBuffer[Label](Label("crash_backward"), Label("crash_forward"))
  val label_nottaken = label_nottakens(rand_range(0,label_nottakens.length-1))

  def helper_two_srcs_same() =
  {
    val reg_src = reg_read_any()
    (reg_src, reg_src)
  }

  def helper_two_srcs_different() =
  {
    val reg_src = reg_read_any()
    val reg_dest1 = reg_write(reg_src)
    val reg_dest2 = reg_write(reg_src)

    insts += ADDI(reg_dest1, reg_src, Imm(0))
    insts += ADDI(reg_dest2, reg_src, Imm(rand_filter(rand_imm, (x) => x == 0)))

    (reg_dest1, reg_dest2)
  }

  def helper_two_srcs_flip_sign_bits() =
  {
    val reg_src1 = reg_read_any()
    val reg_src2 = reg_read_any()
    val reg_dest1 = reg_write(reg_src1)
    val reg_dest2 = reg_write(reg_src2)
    val reg_one = reg_write_visible()
    val reg_mask = reg_write_visible()

    insts += ADDI(reg_one, reg_read_zero(), Imm(1))
    insts += SLL(reg_one, reg_one, Imm(63))
    insts += ADDI(reg_mask, reg_read_zero(), Imm(-1))
    insts += XOR(reg_mask, reg_mask, reg_one)
    insts += AND(reg_dest1, reg_src1, reg_mask)
    insts += OR(reg_dest2, reg_src2, reg_one)

    (reg_dest1, reg_dest2)
  }

  def seq_taken_j() = () =>
  {
    insts += J(label_taken)
  }

  def seq_taken_jal() = () =>
  {
    val reg_x1 = reg_write_ra()
    insts += JAL(label_taken)
  }

  def seq_taken_beq() = () =>
  {
    val regs = helper_two_srcs_same()
    insts += BEQ(regs._1, regs._2, label_taken)
  }

  def seq_taken_bne() = () =>
  {
    val regs = helper_two_srcs_different()
    insts += BNE(regs._1, regs._2, label_taken)
  }

  def seq_taken_blt() = () =>
  {
    val regs = helper_two_srcs_flip_sign_bits()
    insts += BLT(regs._2, regs._1, label_taken)
  }

  def seq_taken_bge() = () =>
  {
    val regs = helper_two_srcs_same()
    insts += BGE(regs._1, regs._2, label_taken)
  }

  def seq_taken_bltu() = () =>
  {
    val regs = helper_two_srcs_flip_sign_bits()
    insts += BLTU(regs._1, regs._2, label_taken)
  }

  def seq_taken_bgeu() = () =>
  {
    val regs = helper_two_srcs_same()
    insts += BGEU(regs._1, regs._2, label_taken)
  }

  def seq_nottaken_beq() = () =>
  {
    val regs = helper_two_srcs_different()
    insts += BEQ(regs._1, regs._2, label_nottaken)
  }

  def seq_nottaken_bne() = () =>
  {
    val regs = helper_two_srcs_same()
    insts += BNE(regs._1, regs._2, label_nottaken)
  }

  def seq_nottaken_blt() = () =>
  {
    val regs = helper_two_srcs_flip_sign_bits()
    insts += BLT(regs._1, regs._2, label_nottaken)
  }

  def seq_nottaken_bge() = () =>
  {
    val regs = helper_two_srcs_different()
    insts += BGE(regs._1, regs._2, label_nottaken)
  }

  def seq_nottaken_bltu() = () =>
  {
    val regs = helper_two_srcs_flip_sign_bits()
    insts += BLTU(regs._2, regs._1, label_nottaken)
  }

  def seq_nottaken_bgeu() = () =>
  {
    val regs = helper_two_srcs_different()
    insts += BGEU(regs._1, regs._2, label_nottaken)
  }

  val candidates = new ArrayBuffer[() => insts.type]

  candidates += seq_taken_j()
  candidates += seq_taken_jal()
  candidates += seq_taken_beq()
  candidates += seq_taken_bne()
  candidates += seq_taken_blt()
  candidates += seq_taken_bge()
  candidates += seq_taken_bltu()
  candidates += seq_taken_bgeu()
  candidates += seq_nottaken_beq()
  candidates += seq_nottaken_bne()
  candidates += seq_nottaken_blt()
  candidates += seq_nottaken_bge()
  candidates += seq_nottaken_bltu()
  candidates += seq_nottaken_bgeu()

  candidates(rand_range(0, candidates.length-1))()
}
