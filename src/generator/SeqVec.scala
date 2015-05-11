package torture

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import Rand._

object SeqVec
{
  var cnt = 0
  def get_id = (cnt += 1)
}


class SeqVec(xregs: HWRegPool, vxregs: HWRegPool, vpregs: HWRegPool, vsregs: HWRegPool, varegs: HWRegPool, vl: Int, cfg: Map[String, Int]) extends InstSeq
{
  override val seqname = "vec"
  val memsize = cfg.getOrElse("memsize", 32)
  val vfnum   = cfg.getOrElse("vf", 10)
  val seqnum  = cfg.getOrElse("seq", 100)
  val use_amo = cfg.getOrElse("amo", "true")
  val mixcfg = cfg.filterKeys(_ contains "mix.").map { case (k,v) => (k.split('.')(1), v) }.asInstanceOf[Map[String,Int]]
  val vseqstats = new HashMap[String,Int].withDefaultValue(0)

  val name = "seqvec_" + SeqVec.cnt
  SeqVec.cnt += 1
  override def toString = name

  val xreg_helper = reg_write_hidden(xregs)
  val vareg_helper = reg_write_hidden(varegs)
  val vec_mem = new VMem(name+"_mem", memsize, vl)
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
  val num_vxreg   = get_rand_reg_num(vxregs.size)
  val num_vpreg   = get_rand_reg_num(vpregs.size) 

  val vxregs_checkout = new ArrayBuffer[Reg]
  for(i <- 1 to num_vxreg)
  {
    val vreg_adding = reg_write_visible(vxregs)
    vxregs_checkout += vreg_adding
  }

  val vpregs_checkout = new ArrayBuffer[Reg]
  for(i <- 1 to num_vpreg)
  {
    val vreg_adding = reg_write_visible(vpregs)
    vpregs_checkout += vreg_adding
  }

  // Handle initialization of vreg from memories
  for((vreg,i) <- vxregs_checkout.zipWithIndex)
  {
    val init_mem = new Mem(Array(Label(name+"_"), vreg, Label("_init"))  , 8*vl)
    extra_hidden_data  += MemDump(init_mem)
    insts += LA(xreg_helper, init_mem)
    insts += VMSA(vareg_helper, xreg_helper)
    val vf_init_block = new ProgSeg(name+"_"+i+"_vf_init")
    vf_init_block.insts += VLD(vreg, vareg_helper)
    vf_init_block.insts += VSTOP()
    extra_code += ProgSegDump(vf_init_block)
    insts += LUI(xreg_helper, Label("%hi("+vf_init_block.name+")"))
    insts += VF(RegStrImm(xreg_helper, "%lo("+vf_init_block.name+")"))
  }
  
  for(i <- 1 to vfnum)
  {
    // Create SeqSeq to create some vector instructions
    val vf_instseq = new SeqSeq(vxregs, vpregs, vsregs, vec_mem, seqnum, mixcfg, true, true, false) //TODO: Enable configuration of enabling amo,mul,div ops
    //val vf_instseq = new SeqVALU(vxregs, true, false) //TODO: Enable configuration of enabling amo,mul,div ops
    for ((seqname, seqcnt) <- vf_instseq.seqstats)
    {
      vseqstats(seqname) += seqcnt
    }

    // Dump that SeqSeq into a VF Instruction block
    val vf_block = new ProgSeg(name+"_vf_"+i)
    while(!vf_instseq.is_done)
    {
      vf_block.insts += vf_instseq.next_inst()
    }
    vf_block.insts += VSTOP()
    extra_code += ProgSegDump(vf_block)

    insts += LUI(xreg_helper, Label("%hi("+vf_block.name+")"))
    insts += VF(RegStrImm(xreg_helper, "%lo("+vf_block.name+")"))
  }

  // Handling dumping of vreg to output memories 
  for((vreg,i) <- vxregs_checkout.zipWithIndex)
  {
    if(vreg.hwreg.is_visible) {
      val out_mem = new Mem(Array(Label(name+"_"), vreg, Label("_output"))  , 8*vl)
      extra_visible_data  += MemDump(out_mem)
      insts += LA(xreg_helper, out_mem)
      insts += VMSA(vareg_helper, xreg_helper)
      val vf_init_block = new ProgSeg(name+"_"+i+"_vf_dump")
      vf_init_block.insts += VSD(vreg, vareg_helper)
      vf_init_block.insts += VSTOP()
      extra_code += ProgSegDump(vf_init_block)
      insts += LUI(xreg_helper, Label("%hi("+vf_init_block.name+")"))
      insts += VF(RegStrImm(xreg_helper, "%lo("+vf_init_block.name+")"))
    }
  }

  // Fence to close out the vector sequence
  insts += FENCE_V(Label("// " + name))
}
