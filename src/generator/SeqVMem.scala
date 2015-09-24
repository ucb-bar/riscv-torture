package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqVMem(xregs: HWRegPool, vregs: HWRegPool, sregs:HWRegPool, aregs: HWRegPool,  mem: VMem, use_amo: Boolean, use_seg: Boolean, use_stride: Boolean) extends VFInstSeq
{
  override val seqname = "vmem"
  def helper_setup_address(reg_addr: Reg, reg_vaddr: Reg, baseaddr: Int, reg_vstride: Option[Reg] = None, stride: Int = 0) =
  {
    insts += LA(reg_addr, BaseImm(mem.toString, baseaddr))
    insts += VMSA(reg_vaddr, reg_addr)
    reg_vstride match {
      case Some(reg) => 
      {
        insts += LI(reg_addr, Imm(stride))
        insts += VMSA(reg, reg_addr)
      }
      case None => {}
    }
  }
  def helper_setup_scalar(reg_addr: Reg, reg_vaddr: Reg, baseaddr: Int, reg_vstride: Option[Reg] = None, stride: Int = 0) =
  {
    insts += LA(reg_addr, BaseImm(mem.toString, baseaddr))
    insts += VMSS(reg_vaddr, reg_addr)
    reg_vstride match {
      case Some(reg) =>
      {
        insts += LI(reg_addr, Imm(stride))
        insts += VMSS(reg, reg_addr)
      }
      case None => {}
    }
  }

  def seq_load_addrfn(op: Opcode, addrfn: (Int) => Int, seg: Option[Int] = None, stride: Option[Reg] = None) = () =>
  {
    val reg_addr   = reg_write_hidden(xregs)
    val reg_vaddr  = reg_write_hidden(aregs)
    val reg_dest   = seg match {
      case Some(segs) => reg_write_visible_consec(vregs, segs+1)//resever seglen+1 regs
      case None => reg_write_visible(vregs)
    }
    val addr = addrfn(mem.ut_size)

    helper_setup_address(reg_addr, reg_vaddr, addr, stride, addrfn(mem.ut_size))
    (seg, stride) match {
      case (Some(segs), Some(reg)) => vinsts += op(reg_dest, reg_vaddr, reg, Imm(segs))
      case (Some(segs), None) => vinsts += op(reg_dest, reg_vaddr, Imm(segs))
      case (None, Some(reg)) => vinsts += op(reg_dest, reg_vaddr, reg)
      case (None, None) => vinsts += op(reg_dest, reg_vaddr)
    }
  }

  def seq_load_seg_addrfn(op: Opcode, addrfn: (Int) => Int, bytes :Int) =
  {
    val seglen = rand_seglen
    assert(bytes*seglen <= mem.ut_size,
      "Per uthread memory must be larger than seglen*bytes")
    seq_load_addrfn(op, addrfn, Some(seglen), None)
  }

  def seq_load_stride_addrfn(op: Opcode, addrfn: (Int) => Int) =
  {
    val reg_vstride= reg_write_hidden(aregs)
    seq_load_addrfn(op, addrfn, None, Some(reg_vstride))
  }

  def seq_load_seg_stride_addrfn(op: Opcode, addrfn: (Int) => Int, bytes :Int) =
  {
    val seglen = rand_seglen
    assert(bytes*seglen <= mem.ut_size,
      "Per uthread memory must be larger than seglen*bytes")
    val reg_vstride= reg_write_hidden(aregs)
    seq_load_addrfn(op, addrfn, Some(seglen), Some(reg_vstride))
  }

  def seq_store_addrfn(op: Opcode, addrfn: (Int) => Int, seg: Option[Int] = None, stride: Option[Reg] = None) = () =>
  {
    val reg_addr   = reg_write_hidden(xregs)
    val reg_vaddr  = reg_write_hidden(aregs)
    val reg_src   = seg match {
      case Some(segs) => reg_read_visible_consec(vregs, segs+1) //reserve seglen+1 regs
      case None => reg_read_visible(vregs)
    }
    val addr = addrfn(mem.ut_size)

    helper_setup_address(reg_addr, reg_vaddr, addr, stride, addrfn(mem.ut_size))
    (seg, stride) match {
      case (Some(segs), Some(reg)) => vinsts += op(reg_src, reg_vaddr, reg, Imm(segs))
      case (Some(segs), None) => vinsts += op(reg_src, reg_vaddr, Imm(segs))
      case (None, Some(reg)) => vinsts += op(reg_src, reg_vaddr, reg)
      case (None, None) => vinsts += op(reg_src, reg_vaddr)
    }
  }

  def seq_store_seg_addrfn(op: Opcode, addrfn: (Int) => Int, bytes: Int) =
  {
    val seglen = rand_seglen
    assert(bytes*seglen <= mem.ut_size,
      "Per uthread memory must be larger than seglen*bytes")
    seq_store_addrfn(op, addrfn, Some(seglen), None)
  }

  def seq_store_stride_addrfn(op: Opcode, addrfn: (Int) => Int) =
  {
    val reg_vstride= reg_write_hidden(aregs)
    seq_store_addrfn(op, addrfn, None, Some(reg_vstride))
  }

  def seq_store_seg_stride_addrfn(op: Opcode, addrfn: (Int) => Int, bytes: Int) =
  {
    val seglen = rand_seglen
    assert(bytes*seglen <= mem.ut_size,
      "Per uthread memory must be larger than seglen*bytes")
    val reg_vstride= reg_write_hidden(aregs)
    seq_store_addrfn(op, addrfn, Some(seglen), Some(reg_vstride))
  }

  def seq_amo_addrfn(op: Opcode, addrfn: (Int) => Int) = () =>
  {
    val reg_addr = reg_write_hidden(xregs)
    val reg_vaddr  = reg_write_hidden(sregs)
    val reg_dest = reg_write_visible(vregs)
    val reg_src = reg_read_visible(vregs)
    val addr = addrfn(mem.ut_size)

    helper_setup_scalar(reg_addr, reg_vaddr, addr)
    vinsts += op(reg_dest, RegImm(reg_vaddr, 0), reg_src)
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

  if(use_seg)
  {
    candidates += seq_load_seg_addrfn(VLSEGB, rand_addr_b, 1)
    candidates += seq_load_seg_addrfn(VLSEGBU, rand_addr_b, 1)
    candidates += seq_load_seg_addrfn(VLSEGH, rand_addr_h, 2)
    candidates += seq_load_seg_addrfn(VLSEGHU, rand_addr_h, 2)
    candidates += seq_load_seg_addrfn(VLSEGW, rand_addr_w, 4)
    candidates += seq_load_seg_addrfn(VLSEGWU, rand_addr_w, 4)
    candidates += seq_load_seg_addrfn(VLSEGD, rand_addr_d, 8)

    candidates += seq_store_seg_addrfn(VSSEGB, rand_addr_b, 1)
    candidates += seq_store_seg_addrfn(VSSEGH, rand_addr_h, 2)
    candidates += seq_store_seg_addrfn(VSSEGW, rand_addr_w, 4)
    candidates += seq_store_seg_addrfn(VSSEGD, rand_addr_d, 8)
  }

  if(use_stride)
  {
    candidates += seq_load_stride_addrfn(VLSTB, rand_addr_b)
    candidates += seq_load_stride_addrfn(VLSTBU, rand_addr_b)
    candidates += seq_load_stride_addrfn(VLSTH, rand_addr_h)
    candidates += seq_load_stride_addrfn(VLSTHU, rand_addr_h)
    candidates += seq_load_stride_addrfn(VLSTW, rand_addr_w)
    candidates += seq_load_stride_addrfn(VLSTWU, rand_addr_w)
    candidates += seq_load_stride_addrfn(VLSTD, rand_addr_d)

    candidates += seq_store_stride_addrfn(VSSTB, rand_addr_b)
    candidates += seq_store_stride_addrfn(VSSTH, rand_addr_h)
    candidates += seq_store_stride_addrfn(VSSTW, rand_addr_w)
    candidates += seq_store_stride_addrfn(VSSTD, rand_addr_d)
    if(use_seg)
    {
      candidates += seq_load_seg_stride_addrfn(VLSEGSTB, rand_addr_b, 1)
      candidates += seq_load_seg_stride_addrfn(VLSEGSTBU, rand_addr_b, 1)
      candidates += seq_load_seg_stride_addrfn(VLSEGSTH, rand_addr_h, 2)
      candidates += seq_load_seg_stride_addrfn(VLSEGSTHU, rand_addr_h, 2)
      candidates += seq_load_seg_stride_addrfn(VLSEGSTW, rand_addr_w, 4)
      candidates += seq_load_seg_stride_addrfn(VLSEGSTWU, rand_addr_w, 4)
      candidates += seq_load_seg_stride_addrfn(VLSEGSTD, rand_addr_d, 8)

      candidates += seq_store_seg_stride_addrfn(VSSEGSTB, rand_addr_b, 1)
      candidates += seq_store_seg_stride_addrfn(VSSEGSTH, rand_addr_h, 2)
      candidates += seq_store_seg_stride_addrfn(VSSEGSTW, rand_addr_w, 4)
      candidates += seq_store_seg_stride_addrfn(VSSEGSTD, rand_addr_d, 8)
    }
  }

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
