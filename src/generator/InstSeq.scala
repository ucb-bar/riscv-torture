package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class InstSeq extends HWRegAllocator
{
  val insts = new ArrayBuffer[Inst]
  var inst_ptr = 0
  val seqname = "Unnamed"

  val extra_code = new ArrayBuffer[DataChunk]
  val extra_hidden_data = new ArrayBuffer[DataChunk]
  val extra_visible_data = new ArrayBuffer[DataChunk]
  
  def is_done = insts.length == inst_ptr

  def next_inst() =
  {
    val inst = insts(inst_ptr)
    inst_ptr += 1
    inst
  }
}

object InstSeq
{
  def apply(prob_tbl: ArrayBuffer[(Int, () => InstSeq)]): InstSeq =
  {
    var p = rand_range(0, 99)
    for ((prob, gen_seq) <- prob_tbl)
    {
      if (p < prob) return gen_seq()
      p -= prob
    }

    assert(false, println("Probabilties should have added up to 100%"))
    new InstSeq()
  }
}

import HWReg._

class HWRegAllocator
{
  val regs = new ArrayBuffer[Reg]
  var allocated = false

  def reg_fn(hwrp: HWRegPool, filter: (HWReg) => Boolean, alloc: (HWReg) => Unit, free: (HWReg) => Unit) =
  {
    val reg = new RegNeedsAlloc(hwrp, filter, alloc, free)
    regs += reg
    reg
  }

  def reg_read_zero(hwrp: HWRegPool) = { reg_fn(hwrp, filter_read_zero, alloc_read, free_read) }
  def reg_read_any(hwrp: HWRegPool) = { reg_fn(hwrp, filter_read_any, alloc_read, free_read) }
  def reg_read_any_other(hwrp: HWRegPool, other: Reg) = { reg_fn(hwrp, filter_read_any_other(other), alloc_read, free_read) }
  def reg_read_visible(hwrp: HWRegPool) = { reg_fn(hwrp, filter_read_visible, alloc_read, free_read) }
  def reg_write_ra(hwrp: HWRegPool) = { reg_fn(hwrp, filter_write_ra, alloc_write(false), free_write) }
  def reg_write_visible(hwrp: HWRegPool) = { reg_fn(hwrp, filter_write_visible, alloc_write(true), free_write) }
  def reg_write_hidden(hwrp: HWRegPool) = { reg_fn(hwrp, filter_write_hidden, alloc_write(false), free_write) }
  def reg_write(hwrp: HWRegPool, regs: Reg*) = { reg_fn(hwrp, filter_write_dep(regs.toList), alloc_write_dep(regs.toList), free_write) }
  def reg_write_other(hwrp: HWRegPool, other: Reg, regs: Reg*) = { reg_fn(hwrp, filter_write_dep_other(other, regs.toList), alloc_write_dep(regs.toList), free_write) }

  def allocate_regs(): Boolean =
  {
    for (reg <- regs)
    {
      val regna = reg.asInstanceOf[RegNeedsAlloc]
      val candidates = regna.hwrp.hwregs.filter(regna.filter)

      if (candidates.length == 0)
        return false

      val hwreg = rand_pick(candidates)
      regna.alloc(hwreg)
      regna.hwreg = hwreg
    }

    System.out.println("allocated_regs")
    allocated = true

    return true
  }

  def free_regs() =
  {
    for (reg <- regs)
    {
      val regna = reg.asInstanceOf[RegNeedsAlloc]
      regna.free(regna.hwreg)
    }
  }
}
