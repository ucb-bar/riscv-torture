package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqVMem(xregs: HWRegPool, vregs: HWRegPool, aregs: HWRegPool,  mem: VMem, use_amo: Boolean) extends VFInstSeq
{
  override val seqname = "vmem"
  def helper_setup_address(reg_addr: Reg, reg_vaddr: Reg, baseaddr: Int) =
  {
    insts += LA(reg_addr, BaseImm(mem.toString, baseaddr))
    insts += VMSA(reg_vaddr, reg_addr)
  }

  def seq_load_addrfn(op: Opcode, addrfn: (Int) => Int) = () =>
  {
    val reg_addr   = reg_write_hidden(xregs)
    val reg_vaddr  = reg_write_hidden(aregs)
    val reg_dest   = reg_write_visible(vregs)
    val addr = addrfn(mem.ut_size)

    helper_setup_address(reg_addr, reg_vaddr, addr)
    vinsts += op(reg_dest, reg_vaddr)
  }

  def seq_store_addrfn(op: Opcode, addrfn: (Int) => Int) = () =>
  {
    val reg_addr   = reg_write_hidden(xregs)
    val reg_vaddr  = reg_write_hidden(aregs)
    val reg_src    = reg_read_visible(vregs)
    val addr = addrfn(mem.ut_size)

    helper_setup_address(reg_addr, reg_vaddr, addr)
    vinsts += op(reg_src, reg_vaddr)
  }

  def seq_amo_addrfn(op: Opcode, addrfn: (Int) => Int) = () =>
  {
    val reg_addr = reg_write_hidden(xregs)
    val reg_vaddr  = reg_write_hidden(aregs)
    val reg_dest = reg_write_visible(vregs)
    val reg_src = reg_read_visible(vregs)
    val addr = addrfn(mem.ut_size)

    helper_setup_address(reg_addr, reg_vaddr, addr)
    vinsts += op(reg_dest, reg_src, reg_vaddr)
  }

  val candidates = new ArrayBuffer[() => vinsts.type]

  candidates += seq_load_addrfn(VLB, rand_addr_b)
  candidates += seq_load_addrfn(VLBU, rand_addr_b)
  candidates += seq_load_addrfn(VLH, rand_addr_h)
  candidates += seq_load_addrfn(VLHU, rand_addr_h)
  candidates += seq_load_addrfn(VLW, rand_addr_w)
  candidates += seq_load_addrfn(VLWU, rand_addr_w)
  candidates += seq_load_addrfn(VLD, rand_addr_d)

  candidates += seq_store_addrfn(VSB, rand_addr_b)
  candidates += seq_store_addrfn(VSH, rand_addr_h)
  candidates += seq_store_addrfn(VSW, rand_addr_w)
  candidates += seq_store_addrfn(VSD, rand_addr_d)

  if(use_amo)
  {
    candidates += seq_amo_addrfn(VAMOADD_W, rand_addr_w)
    candidates += seq_amo_addrfn(VAMOSWAP_W, rand_addr_w)
    candidates += seq_amo_addrfn(VAMOAND_W, rand_addr_w)
    candidates += seq_amo_addrfn(VAMOOR_W, rand_addr_w)
    candidates += seq_amo_addrfn(VAMOMIN_W, rand_addr_w)
    candidates += seq_amo_addrfn(VAMOMINU_W, rand_addr_w)
    candidates += seq_amo_addrfn(VAMOMAX_W, rand_addr_w)
    candidates += seq_amo_addrfn(VAMOMAXU_W, rand_addr_w)

    candidates += seq_amo_addrfn(VAMOADD_D, rand_addr_d)
    candidates += seq_amo_addrfn(VAMOSWAP_D, rand_addr_d)
    candidates += seq_amo_addrfn(VAMOAND_D, rand_addr_d)
    candidates += seq_amo_addrfn(VAMOOR_D, rand_addr_d)
    candidates += seq_amo_addrfn(VAMOMIN_D, rand_addr_d)
    candidates += seq_amo_addrfn(VAMOMINU_D, rand_addr_d)
    candidates += seq_amo_addrfn(VAMOMAX_D, rand_addr_d)
    candidates += seq_amo_addrfn(VAMOMAXU_D, rand_addr_d)
  }

  rand_pick(candidates)()
}
