package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

object SeqVec
{
  var cnt = 0
  def get_id = (cnt += 1)
}


class SeqVec(xregs: HWRegPool, vxregs: HWRegPool, vfregs_s: HWRegPool, vfregs_d: HWRegPool, vl: Int, cfg: Map[String, Int]) extends InstSeq
{
  val memsize = cfg.getOrElse("memsize", 1024)
  val vfnum   = cfg.getOrElse("vf", 10)
  val seqnum  = cfg.getOrElse("seq", 100)
  val mixcfg = cfg.filterKeys(_ contains "mix.").map { case (k,v) => (k.split('.')(1), v) }.asInstanceOf[Map[String,Int]]

  val name = "seqvec_" + SeqVec.cnt
  SeqVec.cnt += 1
  override def toString = name

  val xreg_helper = reg_write_hidden(xregs)
  val vec_mem = new Mem(name+"_mem", memsize)
  extra_visible_data += MemDump(vec_mem)

  // Determine how many per type of vector register need to checkout for writing
  def get_rand_reg_num(max: Int) =  // TODO: discuss this
  {
    val randtype = rand_range(0, 99)
    val attempt =
      if(randtype < 5)          //  5% use a lot of registers
        rand_range(max/2, max)  
      else if(randtype < 10)    //  5% use very little registers
        rand_range(0, 3)
      else                      // 90% use moderate number
        rand_range(3, max/2)
    Math.min(max, attempt)
  }
  val num_vxreg   = get_rand_reg_num(vxregs.size-1) // excluding x0
  val num_vfreg_s = get_rand_reg_num(vfregs_s.size)
  val num_vfreg_d = get_rand_reg_num(vfregs_d.size)

  // Create shadow register pools to mimic those registers
  val shadow_vxregs   = new ShadowRegPool
  val shadow_vfregs_s = new ShadowRegPool
  val shadow_vfregs_d = new ShadowRegPool

  // Check them out and add to shadow pools
  val vxregs_checkout = new ArrayBuffer[Reg]
  shadow_vxregs.hwregs += new HWShadowReg(reg_read_zero(vxregs), "x0_shadow", true, false)
    // x0 does not need to be added to checked out list since will not write to it
  for(i <- 1 to num_vxreg)
  {
    val vreg_adding = reg_write_visible(vxregs)
    vxregs_checkout += vreg_adding
    shadow_vxregs.hwregs += new HWShadowReg(vreg_adding, "x_shadow", true, true)
  }

  val vfregs_s_checkout = new ArrayBuffer[Reg]
  for(i <- 1 to num_vfreg_s)
  {
    val vreg_adding = reg_write_visible(vfregs_s)
    vfregs_s_checkout += vreg_adding
    shadow_vfregs_s.hwregs += new HWShadowReg(vreg_adding, "vf_s_shadow", true, true)
  }

  val vfregs_d_checkout = new ArrayBuffer[Reg]
  for(i <- 1 to num_vfreg_d)
  {
    val vreg_adding = reg_write_visible(vfregs_d)
    vfregs_d_checkout += vreg_adding
    shadow_vfregs_d.hwregs += new HWShadowReg(vreg_adding, "vf_d_shadow", true, true)
  }
  
  // Handle initialization of vreg from memories
  for(vreg <- vxregs_checkout)
  {
    val init_mem = new Mem(Array(Label(name+"_"), vreg, Label("_init"))  , 8*vl)
    extra_hidden_data  += MemDump(init_mem)
    insts += LA(xreg_helper, init_mem)
    insts += VLD(vreg, xreg_helper)
  }
  
  for(vreg <- vfregs_s_checkout)
  {
    val init_mem = new Mem(Array(Label(name+"_"), vreg, Label("_init"))  , 4*vl)
    extra_hidden_data  += MemDump(init_mem)
    insts += LA(xreg_helper, init_mem)
    insts += VFLW(vreg, xreg_helper)
  }

  for(vreg <- vfregs_d_checkout)
  {
    val init_mem = new Mem(Array(Label(name+"_"), vreg, Label("_init"))  , 8*vl)
    extra_hidden_data  += MemDump(init_mem)
    insts += LA(xreg_helper, init_mem)
    insts += VFLD(vreg, xreg_helper)
  }

  for(i <- 1 to vfnum)
  {
    // Create SeqSeq to create some vector instructions
    val vf_instseq = new SeqSeq(shadow_vxregs, shadow_vfregs_s, shadow_vfregs_d, vec_mem, seqnum, mixcfg)

    // Dump that SeqSeq into a VF Instruction block
    val vf_block = new ProgSeg(name+"_vf_"+i)
    while(!vf_instseq.is_done)
    {
      vf_block.insts += vf_instseq.next_inst()
    }
    vf_block.insts += STOP()
    extra_code += ProgSegDump(vf_block)

    insts += LUI(xreg_helper, Label("%hi("+vf_block.name+")"))
    insts += VF(RegStrImm(xreg_helper, "%lo("+vf_block.name+")"))
  }

  // Handling dumping of vreg to output memories 
  for((shadow_reg, vreg) <- shadow_vxregs.pairings(_.is_visible))
  {
    val out_mem = new Mem(Array(Label(name+"_"), vreg, Label("_output"))  , 8*vl)
    extra_visible_data  += MemDump(out_mem)
    insts += LA(xreg_helper, out_mem)
    insts += VSD(vreg, xreg_helper)
  }

  for((shadow_reg, vreg) <- shadow_vfregs_s.pairings(_.is_visible))
  {
    val out_mem = new Mem(Array(Label(name+"_"), vreg, Label("_output"))  , 4*vl)
    extra_visible_data  += MemDump(out_mem)
    insts += LA(xreg_helper, out_mem)
    insts += VFSW(vreg, xreg_helper)
  }
  
  for((shadow_reg, vreg) <- shadow_vfregs_d.pairings(_.is_visible))
  {
    val out_mem = new Mem(Array(Label(name+"_"), vreg, Label("_output"))  , 8*vl)
    extra_visible_data  += MemDump(out_mem)
    insts += LA(xreg_helper, out_mem)
    insts += VFSD(vreg, xreg_helper)
  }

  // Fence to close out the vector sequence
  insts += FENCE_V_L(Label("// " + name))
}
