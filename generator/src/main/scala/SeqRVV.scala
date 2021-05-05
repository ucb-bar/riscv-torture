package torture

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqRVV(rvvregs: HWRegPool, xregs: HWRegPool, fregs_s: HWRegPool, fregs_d: HWRegPool, mem: Mem,
rv_vmem_unit: Boolean, rv_vmem_const: Boolean, rv_vmem_vect: Boolean, rv_vmem_zvlsseg: Boolean,
rv_vinteger: Boolean, rv_vfixed: Boolean, rv_vfloat: Boolean, rv_vreduce: Boolean, rv_vmask: Boolean,
rv_vpermute: Boolean, rv_vamo: Boolean, rv_wide: Boolean, rv_narrow: Boolean, lmul: String, sew: Int, nr: Int, nf: Int,
mask: Boolean, gen_config: Boolean, multi_config: Boolean) extends InstSeq
{
  override val seqname = "rvec"

  val inst_seq_0  = "op x[rd], x[rs1], e<sew>, m<lmul>, ta, ma"
  val inst_seq_1  = "op vd, addr(x[rs1])"
  val inst_seq_2  = "op vd, addr(x[rs1]), x[rs2]"
  val inst_seq_3  = "op vd, addr(x[rs1]), vs2"
  val inst_seq_4  = "op vd, addr(x[rs1]), vs2, vd, v0.t"
  val inst_seq_5  = "op x0, addr(x[rs1]), vs2, vs3, v0.t"
  val inst_seq_6  = "op vd, vs1, vs2"
  val inst_seq_7  = "op vd, vs1, x[rs2]"
  val inst_seq_8  = "op vd, vs1, imm"
  val inst_seq_9  = "op vd, vs1"
  val inst_seq_10 = "op vd, vs1, vs2, v0"
  val inst_seq_11 = "op vd, vs1, x[rs2], v0"				// reverse numbering of source regs
  val inst_seq_12 = "op vd, vs1, imm, v0"
  val inst_seq_13 = "op vd, x[rs1], vs2"
  val inst_seq_14 = "op vd, x[rs1]"
  val inst_seq_15 = "op vd, imm"
  val inst_seq_16 = "op vd, vs1, f[rs2]"
  val inst_seq_17 = "op vd, f[rs1], vs2"
  val inst_seq_18 = "op vd, f[rs1]"
  val inst_seq_19 = "op x[rd], vs1"
  val inst_seq_20 = "op vd"
  val inst_seq_21 = "op f[rd], vs1"
  val inst_seq_22 = "op vd, vs1, f[rs2], v0"

  //------------------------------ seq 0 ----------------------------------//
  def seq_0(op: Opcode)= () =>  // "op x[rd], x[rs1], e<sew>, m<lmul>, ta, ma"
  {
    var dest = reg_write(xregs)
    val vl   = reg_write(xregs)
    insts += LI(vl, Imm(2048))
    insts += op(dest, vl, SEW(sew), LMUL(lmul), TA(), MA() )
  }
  
  //------------------------------ seq 1 ----------------------------------//
  def seq_1(op: Opcode,grouped_reg :String, addrfn: (Int) => Int) = () =>  //"op vd addr(x[rs1])"
  {
    var dest       = dest_n(grouped_reg)
    val reg_addr   = reg_write(xregs)
    val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val addr       = addrfn(mem.size)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()
    insts         += LA(reg_addr,  BaseImm(mem.toString, addr))

    if(mask && mask_rand)
      insts         += op(reg_dest,  RegImm(reg_addr, 0),mask_reg)
    else
      insts         += op(reg_dest,  RegImm(reg_addr, 0))
  }

  def seq1_um(op: Opcode, addrfn: (Int) => Int) = () =>  //"op vd addr(x[rs1])"
  {
    var dest       = dest_n(lmul)
    val reg_addr   = reg_write(xregs)
    val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val addr       = addrfn(mem.size)

    insts         += LA(reg_addr,  BaseImm(mem.toString, addr))
    insts         += op(reg_dest,  RegImm(reg_addr, 0))
  }

  //------------------------------ seq 2 ----------------------------------//
  def seq_2(op: Opcode, grouped_reg :String, addrfn: (Int) => Int) = () =>  //op vd addr(x[rs1]) x[rs2]"
  { 
    var dest       = dest_n(grouped_reg)
    val reg_addr   = reg_write(xregs)
    val reg_stride = reg_write(xregs)
    val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val addr       = addrfn(mem.size)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()
    
    insts         += LA(reg_addr,   BaseImm(mem.toString, addr))
    insts         += LI(reg_stride, Imm(8))
    if(mask && mask_rand)
      insts         += op(reg_dest,   RegImm(reg_addr, 0),reg_stride,mask_reg)
    else
      insts         += op(reg_dest,   RegImm(reg_addr, 0),reg_stride)
  }

  //------------------------------ seq 3 ----------------------------------// 
  def seq_3(op: Opcode,grouped_reg :String ,addrfn: (Int) => Int) = () => //"op vd addr(x[rs1]) vs2"
  {
  	var dest       = dest_n(grouped_reg)
  	var src1       = src1_n(grouped_reg , dest)
	  val reg_addr   = reg_write(xregs)
	  val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val src2	     = vreg_write_src1_n(rvvregs, lmul, src1)
    val addr       = addrfn(mem.size)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    insts += LA(reg_addr,   BaseImm(mem.toString, addr))
    insts += VMV_V_I(src2,  Imm(8))

    if(mask && mask_rand)
      insts += op(reg_dest,   RegImm(reg_addr,0), src2,mask_reg)
    else
      insts += op(reg_dest,   RegImm(reg_addr,0), src2)
  }

  //------------------------------ seq 4 ----------------------------------//
  def seq_4(op: Opcode, addrfn: (Int) => Int) = () => //"op vd addr(x[rs1]) vs2 vd v0.t"
  {
	  var dest       = dest_n(lmul)
  	var src1       = src1_n(lmul, dest)
	  val reg_addr   = reg_write(xregs)
	  val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val src2	     = vreg_write_src1_n(rvvregs, lmul, src1)
    val addr       = addrfn(mem.size)
    val mask_reg   = V0t("v0.t")

    insts += LA(reg_addr,   BaseImm(mem.toString, addr))
    insts += VMV_V_I(src2,  Imm(8))
    insts += op(reg_dest, RegImm(reg_addr, 0), src2, reg_dest , mask_reg)
  }

  //------------------------------ seq 5 ----------------------------------//
  def seq_5(op: Opcode, addrfn: (Int) => Int) = () => // "op x0 addr(x[rs1]) vs2 vs3 v0.t"
  {
	  var dest       = dest_n(lmul)
  	var src1       = src1_n(lmul, dest)
	  val reg_addr   = reg_write(xregs)
    val reg_zero   = reg_read_zero(xregs)
	  val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val src2	     = vreg_write_src1_n(rvvregs, lmul, src1)
    val addr       = addrfn(mem.size)
    val mask_reg   = V0t("v0.t")

    insts += LA(reg_addr,   BaseImm(mem.toString, addr))
    insts += VMV_V_I(src2,  Imm(8))
    insts += op(reg_zero, RegImm(reg_addr, 0), src2, reg_dest , mask_reg)
  }

  //------------------------------ seq 6 ----------------------------------//
  def seq_6(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var dest       = dest_n(lmul)
  	var src1       = dest_n(lmul)
  	var src2       = dest_n(lmul)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_n(rvvregs,lmul,src1)
    val src2_reg   = vreg_read_dest_n(rvvregs,lmul,src2)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else  
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq6_um(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var dest     = dest_n(lmul)
  	var src1     = dest_n(lmul)
  	var src2     = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_n(rvvregs,lmul,src1)
    val src2_reg = vreg_read_dest_n(rvvregs,lmul,src2)

    insts += op(dest_reg, src1_reg, src2_reg)
  }
 
  def seq6_w(op: Opcode) = () => // "op vd vs1 vs2"
  {	
  	var dest       = dest_w(lmul)
  	var src1       = src1_w(lmul,dest)
  	var src2       = src2_w(lmul,dest, src1)
    val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_w(rvvregs,lmul,src1)
    val src2_reg   = vreg_read_src2_w(rvvregs,lmul,src2)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }
 
  def seq6_n(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var src1       = dest_w(lmul)
    var dest       = dest_nm(lmul, src1)
  	var src2       = src1_w(lmul, src1)
  	val dest_reg   = vreg_write_src1_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg   = vreg_read_src1_w(rvvregs,lmul,src2)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts       += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts       += op(dest_reg, src1_reg, src2_reg)
  }

  def seq6_wx(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var dest       = dest_w(lmul)
  	var src1       = dest_w(lmul)
  	if(lmul=="8")   
      src1=dest 
  	var src2       = dest_wo(lmul,dest, src1)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg   = vreg_read_src2_w(rvvregs,lmul,src2)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts        += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts        += op(dest_reg, src1_reg, src2_reg)
  }

  def seq6_p(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var dest       = dest_n(lmul)
  	var src1       = src1_n(lmul,dest)
  	var src2       = src2_n(lmul,dest,src1)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg   = vreg_read_src2_n(rvvregs,lmul,src2)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand) 
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else  
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq6_p_um(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var dest      = dest_n(lmul)
  	var src1      = src1_n(lmul,dest)
  	var src2      = src2_n(lmul,dest,src1)
  	val dest_reg  = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg  = vreg_read_src2_n(rvvregs,lmul,src2)

    insts += op(dest_reg, src1_reg, src2_reg)
  }

  //------------------------------ seq 7 ----------------------------------//
  def seq_7(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest       = dest_n(lmul)
  	var src1       = dest_n(lmul)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_n(rvvregs,lmul,src1)
    val src2_reg   = reg_read_any(xregs)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq7_um(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest     = dest_n(lmul)
  	var src1     = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_n(rvvregs,lmul,src1)
    val src2_reg = reg_read_any(xregs)

    insts += op(dest_reg, src1_reg, src2_reg)
  }
  
  def seq7_w(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest       = dest_w(lmul)
  	var src1       = src1_w(lmul,dest)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_w(rvvregs,lmul,src1)
    val src2_reg   = reg_read_any(xregs)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand) 
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else  
      insts += op(dest_reg, src1_reg, src2_reg)
  }
 
  def seq7_wx(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest       = dest_w(lmul)
  	var src1       = dest_w(lmul)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg   = reg_read_any(xregs)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask) 
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }
  
  def seq7_n(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var src1       = dest_w(lmul)
    var dest       = dest_nm(lmul,src1)
  	val dest_reg   = vreg_write_src1_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg   = reg_read_any(xregs)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand) 
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq7_p(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest       = dest_n(lmul)
  	var src1       = src1_n(lmul,dest)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg   = reg_read_any(xregs)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand) 
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)  
  }

  //------------------------------ seq 8 ----------------------------------//
  def seq_8(op: Opcode, immfn: () => Int) = () => // "op vd vs1 imm"
  {
    var dest       = dest_n(lmul)
  	var src1       = dest_n(lmul)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg   = Imm(immfn())
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else 
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq8_um(op: Opcode, immfn: () => Int) = () => // "op vd vs1 imm"
  {
    var dest      = dest_n(lmul)
  	var src1      = dest_n(lmul)
  	val dest_reg  = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg  = Imm(immfn())

    insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq8_p(op: Opcode, immfn: () => Int) = () => // "op vd vs1 imm"
  {
    var dest       = dest_n(lmul)
  	var src1       = src1_n(lmul,dest)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg   = Imm(immfn())
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else  
      insts += op(dest_reg, src1_reg, src2_reg)
  }
  
  def seq8_n(op: Opcode, immfn: () => Int) = () => // "op vd vs1 imm"
  {
    var src1       = dest_w(lmul)
    var dest       = dest_nm(lmul,src1)
  	val dest_reg   = vreg_write_src1_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg   = Imm(immfn())
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  //------------------------------ seq 9 ----------------------------------//
  def seq_9(op: Opcode) = () => // "op vd vs1"
  {
    var dest       = dest_w(lmul)
  	var src1       = src1_w(lmul, dest)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_w(rvvregs,lmul,src1)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg,mask_reg)
    else  
      insts += op(dest_reg, src1_reg)
  }

  def seq9_um(op: Opcode) = () => // "op vd vs1"
  {
    var dest      = dest_w(lmul)
  	var src1      = src1_w(lmul, dest)
  	val dest_reg  = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_src1_w(rvvregs,lmul,src1)

    insts += op(dest_reg, src1_reg)
  }
  
  def seq9_nr_um(op: Opcode) = () => // "op vd vs1"
  {
    var dest      = dest_n(nr.toString)
  	var src1      = dest_nm(nr.toString,dest)
  	val dest_reg  = vreg_write_dest_n(rvvregs,nr.toString,dest)
    val src1_reg  = vreg_read_src1_n(rvvregs,nr.toString,src1)

    insts += op(dest_reg, src1_reg)
  }

  def seq9_m(op: Opcode) = () => // "op vd vs1"
  {
    var dest       = src1_n(lmul,0)
  	var src1       = src1_n(lmul,dest)
  	val dest_reg   = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_n(rvvregs,lmul,src1)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg)
  }

  def seq9_m_um(op: Opcode) = () => // "op vd vs1"
  {
    var dest      = src1_n(lmul,0)
  	var src1      = src1_n(lmul,dest)
  	val dest_reg  = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_src1_n(rvvregs,lmul,src1)

    insts += op(dest_reg, src1_reg)
  }

  def seq9_w(op: Opcode) = () => // "op vd vs1"
  {
    var dest       = dest_w(lmul)
  	var src1       = src1_w(lmul,dest)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_w(rvvregs,lmul,src1)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg)
  }

  def seq9_n(op: Opcode) = () => // "op vd vs1"
  {
    var src1       = dest_w(lmul)
    var dest       = dest_nm(lmul,src1)
  	val dest_reg   = vreg_write_src1_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_w(rvvregs,lmul,src1)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg)
  }

  //------------------------------ seq 10 ----------------------------------//
  def seq10_um(op: Opcode) = () => //"op vd vs1 vs2 v0"
  {
    var dest      = src1_n(lmul,0)
  	var src1      = src1_n(lmul,0)
  	var src2      = src2_n(lmul,0, src1)
  	val src3_reg  = reg_read_v0(rvvregs)
  	val dest_reg  = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg  = vreg_read_src2_n(rvvregs,lmul,src2)

    insts += op(dest_reg, src1_reg, src2_reg,src3_reg)
  }

  //------------------------------ seq 11 ----------------------------------//
  def seq11_um(op: Opcode) = () => // "op vd vs1 x[rs2] v0"
  {
    var dest      = src1_n(lmul,0)
  	var src1      = src1_n(lmul,0)
  	val src3_reg  = reg_read_v0(rvvregs)
  	val dest_reg  = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg  = reg_read_any(xregs)

    insts += op(dest_reg, src1_reg, src2_reg,src3_reg)
  }

  //------------------------------ seq 12 ----------------------------------//
  def seq12_um(op: Opcode, immfn: () => Int) = () => //"op vd vs1 imm v0"
  {
    var dest      = src1_n(lmul,0)
  	var src1      = src1_n(lmul,0)
  	val src3_reg  = reg_read_v0(rvvregs)
  	val dest_reg  = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg  = Imm(immfn())

    insts += op(dest_reg, src1_reg, src2_reg,src3_reg)
  }

  //------------------------------ seq 13 ----------------------------------//
  def seq_13(op: Opcode) = () => // "op vd x[rs1] vs2"
  {
    var dest       = dest_n(lmul)
  	var src2       = dest_n(lmul)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = reg_read_any(xregs)
    val src2_reg   = vreg_read_dest_n(rvvregs,lmul,src2)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg) 
  }

  def seq13_w(op: Opcode) = () => // "op vd x[rs1] vs2"
  {
    var dest       = dest_w(lmul)
  	var src2       = src1_w(lmul,dest)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = reg_read_any(xregs)
    val src2_reg   = vreg_read_src1_w(rvvregs,lmul,src2)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  //------------------------------ seq 14 ----------------------------------//
  def seq14_um(op: Opcode) = () => //"op vd x[rs1]"
  {
    var dest      = dest_n(lmul)
  	val dest_reg  = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg  = reg_read_any(xregs)

    insts += op(dest_reg, src1_reg)
  }

  //------------------------------ seq 15 ----------------------------------//
  def seq15_um(op: Opcode, immfn: () => Int) = () => //"op vd imm"
  {
    var dest      = dest_n(lmul)
  	val dest_reg  = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg  = Imm(immfn())

    insts += op(dest_reg, src1_reg)
  }

  //------------------------------ seq 16 ----------------------------------//
  def seq_16(op: Opcode) = () => //"op vd vs1 f[rs2]"
  {
    var dest       = dest_n(lmul)
  	var src1       = dest_n(lmul)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_n(rvvregs,lmul,src1)
	  val src2_reg   = reg_read_any(fregs_s)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)  
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq16_w(op: Opcode) = () => //"op vd vs1 f[rs2]"
  {
    var dest       = dest_w(lmul)
  	var src1       = src1_w(lmul,dest)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_w(rvvregs,lmul,src1)
	  val src2_reg   = reg_read_any(fregs_s)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)  
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq16_wx(op: Opcode) = () => //"op vd vs1 f[rs2]"
  {
    var dest       = dest_w(lmul)
  	var src1       = dest_w(lmul)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_dest_w(rvvregs,lmul,src1)
	  val src2_reg   = reg_read_any(fregs_s)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand)  
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)
  }
  
  def seq16_p(op: Opcode) = () => //"op vd vs1 f[rs2]"
  {
    var dest       = dest_n(lmul)
  	var src1       = src1_n(lmul,dest)
  	val dest_reg   = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg   = vreg_read_src1_n(rvvregs,lmul,src1)
  	val src2_reg   = reg_read_any(fregs_s)  
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand) 
      insts += op(dest_reg, src1_reg, src2_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg, src2_reg)  
  }

  //------------------------------ seq 17 ----------------------------------//
  def seq17_um(op: Opcode) = () => //"op vd f[rs1] vs2"
  {
    var dest      = dest_n(lmul)
  	var src1      = dest_n(lmul)
  	val dest_reg  = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_dest_n(rvvregs,lmul,src1)
	  val src2_reg  = reg_read_any(fregs_s)  

    insts += op(dest_reg, src2_reg, src1_reg)
  }

  def seq17_w_um(op: Opcode) = () => //"op vd f[rs1] vs2"
  {
    var dest     = dest_w(lmul)
  	var src1     = src1_w(lmul,dest)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_w(rvvregs,lmul,src1)
	  val src2_reg = reg_read_any(fregs_s)  

    insts += op(dest_reg, src2_reg, src1_reg)
  }

  //------------------------------ seq 18 ----------------------------------//
  def seq18_um(op: Opcode) = () => //"op vd f[rs1]"
  {
    var dest     = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
	  val src1_reg = reg_read_any(fregs_s)  

    insts += op(dest_reg, src1_reg) 
  }

  //------------------------------ seq 19 ----------------------------------//
  def seq19_um(op: Opcode) = () => //"op x[rd] vs1"
  {
    var src1     = dest_n(lmul)
  	val dest_reg = reg_write(xregs)
	  val src1_reg = vreg_write_dest_n(rvvregs,lmul,src1)
    insts += op(dest_reg, src1_reg)
  }

  def seq_19(op: Opcode) = () => //"op x[rd] vs1"
  {
    var src1       = dest_n(lmul)
  	val dest_reg   = reg_write(xregs)
	  val src1_reg   = vreg_write_dest_n(rvvregs,lmul,src1)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand) 
      insts += op(dest_reg, src1_reg,mask_reg)
    else
      insts += op(dest_reg, src1_reg)
  }

  //------------------------------ seq 20 ----------------------------------//
  def seq_20(op: Opcode) = () => //"op vd"
  {
    var dest       = dest_n(lmul)
  	val dest_reg   = vreg_write_dest_w(rvvregs,lmul,dest)
    val mask_reg   = V0t("v0.t")
    val mask_rand  = rand_mask()

    if(mask && mask_rand) 
      insts += op(dest_reg,mask_reg)
    else
      insts += op(dest_reg)  
  }

  //------------------------------ seq 21 ----------------------------------//
  def seq21_um(op: Opcode) = () => //"op f[rd] vs1"
  {
  	var src1     = dest_n(lmul)
	  val dest_reg = reg_write(fregs_s)
    val src1_reg = vreg_read_dest_n(rvvregs,lmul,src1)
    insts += op(dest_reg, src1_reg)
  }

  //------------------------------ seq 22 ----------------------------------//
  def seq22_um(op: Opcode) = () => // "op vd vs1 f[rs2] v0"
  {
    var dest      = src1_n(lmul,0)
  	var src1      = src1_n(lmul,0)
  	val src3_reg  = reg_read_v0(rvvregs)
  	val dest_reg  = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg  = vreg_read_src1_n(rvvregs,lmul,src1)
	  val src2_reg  = reg_read_any(fregs_s)  

    insts += op(dest_reg, src1_reg, src2_reg, src3_reg)
  }

  //----------------------------- oplists ---------------------------------//
  val oplist0		    = new ArrayBuffer[Opcode]
  val oplist1		    = new ArrayBuffer[Opcode]
  val oplist1z      = new ArrayBuffer[Opcode]
  val oplist1_um		= new ArrayBuffer[Opcode]
  val oplist2		    = new ArrayBuffer[Opcode]
  val oplist2z		  = new ArrayBuffer[Opcode]
  val oplist3		    = new ArrayBuffer[Opcode]
  val oplist3z		  = new ArrayBuffer[Opcode]
  val oplist4		    = new ArrayBuffer[Opcode]
  val oplist6		    = new ArrayBuffer[Opcode]
  val oplist6_um		= new ArrayBuffer[Opcode]
  val oplist6_w		  = new ArrayBuffer[Opcode]
  val oplist6_wx	  = new ArrayBuffer[Opcode]
  val oplist6_n		  = new ArrayBuffer[Opcode]
  val oplist6_p		  = new ArrayBuffer[Opcode]
  val oplist6_p_um  = new ArrayBuffer[Opcode]
  val oplist6_fw	  = new ArrayBuffer[Opcode]
  val oplist6_fwx	  = new ArrayBuffer[Opcode]
  val oplist7		    = new ArrayBuffer[Opcode]
  val oplist7_um		= new ArrayBuffer[Opcode]
  val oplist7_w		  = new ArrayBuffer[Opcode]
  val oplist7_wx	  = new ArrayBuffer[Opcode]
  val oplist7_n		  = new ArrayBuffer[Opcode]
  val oplist7_p		  = new ArrayBuffer[Opcode]
  val oplist8		    = new ArrayBuffer[Opcode]
  val oplist8_um		= new ArrayBuffer[Opcode]
  val oplist8_p		  = new ArrayBuffer[Opcode]
  val oplist8_n		  = new ArrayBuffer[Opcode]
  val oplist8_f		  = new ArrayBuffer[Opcode]
  val oplist8_f31	  = new ArrayBuffer[Opcode]
  val oplist8_i31	  = new ArrayBuffer[Opcode]
  val oplist9		    = new ArrayBuffer[Opcode]
  val oplist9_um		= new ArrayBuffer[Opcode]
  val oplist9_nr_um	= new ArrayBuffer[Opcode]
  val oplist9_m		  = new ArrayBuffer[Opcode]
  val oplist9_m_um  = new ArrayBuffer[Opcode]
  val oplist9_w		  = new ArrayBuffer[Opcode]
  val oplist9_n		  = new ArrayBuffer[Opcode]
  val oplist10_um	  = new ArrayBuffer[Opcode]
  val oplist11_um	  = new ArrayBuffer[Opcode]
  val oplist12_um	  = new ArrayBuffer[Opcode]
  val oplist13		  = new ArrayBuffer[Opcode]
  val oplist13_w	  = new ArrayBuffer[Opcode]
  val oplist14_um		= new ArrayBuffer[Opcode]
  val oplist15_um		= new ArrayBuffer[Opcode]
  val oplist16		  = new ArrayBuffer[Opcode]
  val oplist16_w	  = new ArrayBuffer[Opcode]
  val oplist16_wx	  = new ArrayBuffer[Opcode]
  val oplist16_p	  = new ArrayBuffer[Opcode]
  val oplist17_um		= new ArrayBuffer[Opcode]
  val oplist17_w_um	= new ArrayBuffer[Opcode]
  val oplist18		  = new ArrayBuffer[Opcode]
  val oplist18_um		= new ArrayBuffer[Opcode]
  val oplist19		  = new ArrayBuffer[Opcode]
  val oplist19_um		= new ArrayBuffer[Opcode]
  val oplist20		  = new ArrayBuffer[Opcode]
  val oplist21_um		= new ArrayBuffer[Opcode]
  val oplist22_um		= new ArrayBuffer[Opcode]
  val default 		  = new ArrayBuffer[Opcode]

  def rvv_mem_rand(sew: Int) : Int => Int =   // "op vd vs1 f[rs2] v0"
  {
	  if(sew == 16)
      return rand_addr_h_rvv
	  if(sew == 32)
      return rand_addr_w_rvv
	  if(sew == 64)
      return rand_addr_d_rvv
    if(sew == 128)
      return rand_addr_128_rvv
	  if(sew == 256)
      return rand_addr_256_rvv
	  if(sew == 512)
      return rand_addr_512_rvv
    if(sew == 1024)
      return rand_addr_1024_rvv

    rand_addr_b_rvv
  }

  val lmul_ext_2 = List("1", "2", "4", "8", "f2", "f4")
  val lmul_ext_4 = List("1", "2", "4", "8", "f2")
  val lmul_ext_8 = List("1", "2", "4", "8")
  val ext_2 = (sew == 16 || sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 || sew == 1024) && lmul_ext_2.contains(lmul)
  val ext_4 = (sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 || sew == 1024) && lmul_ext_4.contains(lmul)
  val ext_8 = (sew == 64 || sew == 128 || sew == 256 || sew == 512 || sew == 1024) && lmul_ext_8.contains(lmul)

  var check = rv_vmem_unit | rv_vmem_const | rv_vmem_vect | rv_vmem_zvlsseg | rv_vfloat | rv_vfixed | rv_vpermute | rv_vamo | rv_vreduce | rv_vmask | rv_vinteger

  if(!check)
    default += (VADD_VI, VRSUB_VI, VOR_VI, VAND_VI, VXOR_VI, VMSEQ_VI, VMSNE_VI, VMSLEU_VI, VMSLE_VI, VMSGTU_VI, VMSGT_VI)

  //-----------------------------------------------------------------------//
  oplist0 += (RVV_VSETVLI)

  if (rv_vmem_unit)
  {
    if(sew == 8  || sew == 16 || sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist1	+= (VLE8_V,VSE8_V,VLE8FF_V)
    if(sew == 16 || sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist1	+= (VLE16_V,VSE16_V,VLE16FF_V)
    if(sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist1	+= (VLE32_V,VSE32_V,VLE32FF_V)
    if(sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist1	+= (VLE64_V,VSE64_V,VLE64FF_V)
    if(sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist1	+= (VLE128_V,VSE128_V,VLE128FF_V)
    if(sew == 256 || sew == 512 ||  sew == 1024)
      oplist1	+= (VLE256_V,VSE256_V,VLE256FF_V)
    if(sew == 512 ||  sew == 1024)
      oplist1	+= (VLE512_V,VSE512_V,VLE512FF_V)
    if(sew == 1024)
      oplist1	+= (VLE1024_V,VSE1024_V,VLE1024FF_V)
  }

  if (rv_vmem_zvlsseg)
  {
    if(sew == 8  || sew == 16 || sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if(nf == 2)
        oplist1z	+= (VLSEG2E8_V,VSSEG2E8_V)
      if(nf == 3)
        oplist1z	+= (VLSEG3E8_V,VSSEG3E8_V)
      if(nf == 4)
        oplist1z	+= (VLSEG4E8_V,VSSEG4E8_V)
      if(nf == 5)
        oplist1z	+= (VLSEG5E8_V,VSSEG5E8_V)
      if(nf == 6)
        oplist1z	+= (VLSEG6E8_V,VSSEG6E8_V)
      if(nf == 7)
        oplist1z	+= (VLSEG7E8_V,VSSEG7E8_V)
      if(nf == 8)
        oplist1z	+= (VLSEG8E8_V,VSSEG8E8_V)
    }
    if(sew == 16 || sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if(nf == 2)
        oplist1z  += (VLSEG2E16_V, VSSEG2E16_V)
      if(nf == 3)
        oplist1z  += (VLSEG3E16_V,VSSEG3E16_V)
      if(nf ==4)
        oplist1z	+= (VLSEG4E16_V,VSSEG4E16_V)
      if(nf ==5)
        oplist1z	+= (VLSEG5E16_V,VSSEG5E16_V)
      if(nf ==6)
        oplist1z	+= (VLSEG6E16_V,VSSEG6E16_V)
      if(nf ==7)
        oplist1z	+= (VLSEG7E16_V,VSSEG7E16_V)
      if(nf ==8)
        oplist1z	+= (VLSEG8E16_V,VSSEG8E16_V)
    }
    if(sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if(nf == 2)
        oplist1z += (VLSEG2E32_V,VSSEG2E32_V)
      if(nf == 3)
        oplist1z	+= (VLSEG3E32_V,VSSEG3E32_V)
      if(nf == 4)
        oplist1z	+= (VLSEG4E32_V,VSSEG4E32_V)
      if(nf == 5)
        oplist1z	+= (VLSEG5E32_V,VSSEG5E32_V)
      if(nf == 6)
        oplist1z	+= (VLSEG6E32_V,VSSEG6E32_V)
      if(nf == 7)
        oplist1z	+= (VLSEG7E32_V,VSSEG7E32_V)
      if(nf == 8)
        oplist1z	+= (VLSEG8E32_V,VSSEG8E32_V)
    }
    if(sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if(nf == 2)
        oplist1z += (VLSEG2E64_V , VSSEG2E64_V)
      if(nf == 3)
        oplist1z	+= (VLSEG3E64_V,VSSEG3E64_V)
      if(nf == 4)
        oplist1z	+= (VLSEG4E64_V,VSSEG4E64_V)
      if(nf == 5)
        oplist1z	+= (VLSEG5E64_V,VSSEG5E64_V)
      if(nf == 6)
        oplist1z	+= (VLSEG6E64_V,VSSEG6E64_V)
      if(nf == 7)
        oplist1z	+= (VLSEG7E64_V,VSSEG7E64_V)
      if(nf == 8)
        oplist1z	+= (VLSEG8E64_V,VSSEG8E64_V)
    }
    if(sew == 128 || sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if(nf == 2)
        oplist1z += (VLSEG2E128_V , VSSEG2E128_V)
      if(nf == 3)
        oplist1z	+= (VLSEG3E128_V,VSSEG3E128_V)
      if(nf == 4)
        oplist1z	+= (VLSEG4E128_V,VSSEG4E128_V)
      if(nf == 5)
        oplist1z	+= (VLSEG5E128_V,VSSEG5E128_V)
      if(nf == 6)
        oplist1z	+= (VLSEG6E128_V,VSSEG6E128_V)
      if(nf == 7)
        oplist1z	+= (VLSEG7E128_V,VSSEG7E128_V)
      if(nf == 8)
        oplist1z	+= (VLSEG8E128_V,VSSEG8E128_V)
    }
    if(sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if(nf ==2)
        oplist1z += (VLSEG2E256_V , VSSEG2E256_V)
      if(nf == 3)
        oplist1z	+= (VLSEG3E256_V,VSSEG3E256_V)
      if(nf == 4)
        oplist1z	+= (VLSEG4E256_V,VSSEG4E256_V)
      if(nf == 5)
        oplist1z	+= (VLSEG5E256_V,VSSEG5E256_V)
      if(nf == 6)
        oplist1z	+= (VLSEG6E256_V,VSSEG6E256_V)
      if(nf == 7)
        oplist1z	+= (VLSEG7E256_V,VSSEG7E256_V)
      if(nf == 8)
        oplist1z	+= (VLSEG8E256_V,VSSEG8E256_V)
    }
    if(sew == 512 ||  sew == 1024 )
    {
      if(nf == 2)
        oplist1z += (VLSEG2E512_V , VSSEG2E512_V)
      if(nf == 3)
        oplist1z	+= (VLSEG3E512_V,VSSEG3E512_V)
      if(nf == 4)
        oplist1z	+= (VLSEG4E512_V,VSSEG4E512_V)
      if(nf == 5)
        oplist1z	+= (VLSEG5E512_V,VSSEG5E512_V)
      if(nf == 6)
        oplist1z	+= (VLSEG6E512_V,VSSEG6E512_V)
      if(nf == 7)
        oplist1z	+= (VLSEG7E512_V,VSSEG7E512_V)
      if(nf == 8)
        oplist1z	+= (VLSEG8E512_V,VSSEG8E512_V)
    }
    if(sew == 1024 )
    {
      if(nf == 2)
        oplist1z += (VLSEG2E1024_V , VSSEG2E1024_V)
      if(nf == 3)
        oplist1z	+= (VLSEG3E1024_V,VSSEG3E1024_V)
      if(nf == 4)
        oplist1z	+= (VLSEG4E1024_V,VSSEG4E1024_V)
      if(nf == 5)
        oplist1z	+= (VLSEG5E1024_V,VSSEG5E1024_V)
      if(nf == 6)
        oplist1z	+= (VLSEG6E1024_V,VSSEG6E1024_V)
      if(nf == 7)
        oplist1z	+= (VLSEG7E1024_V,VSSEG7E1024_V)
      if(nf == 8)
        oplist1z	+= (VLSEG8E1024_V,VSSEG8E1024_V)
    }
  }

  if(rv_vmem_unit)
    if(nf == 1)
      oplist1_um	+= (VL1R_V,VS1R_V)

  if(rv_vmem_const)
  {
    if(sew == 8  || sew == 16 || sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist2	+= (VLSE8_V,VSSE8_V)
    if(sew == 16 || sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist2	+= (VLSE16_V,VSSE16_V)
    if(sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist2	+= (VLSE32_V,VSSE32_V)
    if(sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist2	+= (VLSE64_V,VSSE64_V)
    if(sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist2	+= (VLSE128_V,VSSE128_V)  
    if(sew == 256 || sew == 512 ||  sew == 1024)
      oplist2	+= (VLSE256_V,VSSE256_V)  
    if(sew == 512 ||  sew == 1024)
      oplist2	+= (VLSE512_V,VSSE512_V)  
    if(sew == 1024)
      oplist2	+= (VLSE1024_V,VSSE1024_V)
  }

  if(rv_vmem_zvlsseg)
  {
    if(sew == 8  || sew == 16 || sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if(nf == 2)
        oplist2z	+= (VLSSEG2E8_V,VSSSEG2E8_V)
      if(nf == 3)
        oplist2z	+= (VLSSEG3E8_V,VSSSEG3E8_V)
      if(nf == 4)
        oplist2z	+= (VLSSEG4E8_V,VSSSEG4E8_V)
      if(nf == 5)
        oplist2z	+= (VLSSEG5E8_V,VSSSEG5E8_V)
      if(nf == 6)
        oplist2z	+= (VLSSEG6E8_V,VSSSEG6E8_V)
      if(nf == 7)
        oplist2z	+= (VLSSEG7E8_V,VSSSEG7E8_V)
      if(nf == 8)
        oplist2z	+= (VLSSEG8E8_V,VSSSEG8E8_V)
    }
    if(sew == 16 || sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if (nf == 2)
        oplist2z += (VLSSEG2E16_V, VSSSEG2E16_V)
      if(nf == 3)
        oplist2z	+= (VLSSEG3E16_V,VSSSEG3E16_V)
      if(nf ==4)
        oplist2z	+= (VLSSEG4E16_V,VSSSEG4E16_V)
      if(nf ==5)
        oplist2z	+= (VLSSEG5E16_V,VSSSEG5E16_V)
      if(nf ==6)
        oplist2z	+= (VLSSEG6E16_V,VSSSEG6E16_V)
      if(nf ==7)
        oplist2z	+= (VLSSEG7E16_V,VSSSEG7E16_V)
      if(nf ==8)
        oplist2z	+= (VLSSEG8E16_V,VSSSEG8E16_V)
    }
    if(sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if (nf == 2)
        oplist2z += (VLSSEG2E32_V,VSSSEG2E32_V)
      if(nf == 3)
        oplist2z	+= (VLSSEG3E32_V,VSSSEG3E32_V)
      if(nf == 4)
        oplist2z	+= (VLSSEG4E32_V,VSSSEG4E32_V)
      if(nf == 5)
        oplist2z	+= (VLSSEG5E32_V,VSSSEG5E32_V)
      if(nf == 6)
        oplist2z	+= (VLSSEG6E32_V,VSSSEG6E32_V)
      if(nf == 7)
        oplist2z	+= (VLSSEG7E32_V,VSSSEG7E32_V)
      if(nf == 8)
        oplist2z	+= (VLSSEG8E32_V,VSSSEG8E32_V)
    }
    if(sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if (nf == 2)
        oplist2z += (VLSSEG2E64_V , VSSSEG2E64_V)
      if(nf == 3)
        oplist2z	+= (VLSSEG3E64_V,VSSSEG3E64_V)
      if(nf == 4)
        oplist2z	+= (VLSSEG4E64_V,VSSSEG4E64_V)
      if(nf == 5)
        oplist2z	+= (VLSSEG5E64_V,VSSSEG5E64_V)
      if(nf == 6)
        oplist2z	+= (VLSSEG6E64_V,VSSSEG6E64_V)
      if(nf == 7)
        oplist2z	+= (VLSSEG7E64_V,VSSSEG7E64_V)
      if(nf == 8)
        oplist2z	+= (VLSSEG8E64_V,VSSSEG8E64_V)
    }
    if(sew == 128 || sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if (nf == 2)
        oplist2z += (VLSSEG2E128_V , VSSSEG2E128_V)
      if(nf == 3)
        oplist2z	+= (VLSSEG3E128_V,VSSSEG3E128_V)
      if(nf == 4)
        oplist2z	+= (VLSSEG4E128_V,VSSSEG4E128_V)
      if(nf == 5)
        oplist2z	+= (VLSSEG5E128_V,VSSSEG5E128_V)
      if(nf == 6)
        oplist2z	+= (VLSSEG6E128_V,VSSSEG6E128_V)
      if(nf == 7)
        oplist2z	+= (VLSSEG7E128_V,VSSSEG7E128_V)
      if(nf == 8)
        oplist2z	+= (VLSSEG8E128_V,VSSSEG8E128_V)
    }
    if(sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if (nf ==2)
        oplist2z += (VLSSEG2E256_V , VSSSEG2E256_V)
      if(nf == 3)
        oplist2z	+= (VLSSEG3E256_V,VSSSEG3E256_V)
      if(nf == 4)
        oplist2z	+= (VLSSEG4E256_V,VSSSEG4E256_V)
      if(nf == 5)
        oplist2z	+= (VLSSEG5E256_V,VSSSEG5E256_V)
      if(nf == 6)
        oplist2z	+= (VLSSEG6E256_V,VSSSEG6E256_V)
      if(nf == 7)
        oplist2z	+= (VLSSEG7E256_V,VSSSEG7E256_V)
      if(nf == 8)
        oplist2z	+= (VLSSEG8E256_V,VSSSEG8E256_V)
    }
    if(sew == 512 ||  sew == 1024 )
    {
      if (nf == 2)
        oplist2z += (VLSSEG2E512_V , VSSSEG2E512_V)
      if(nf == 3)
        oplist2z	+= (VLSSEG3E512_V,VSSSEG3E512_V)
      if(nf == 4)
        oplist2z	+= (VLSSEG4E512_V,VSSSEG4E512_V)
      if(nf == 5)
        oplist2z	+= (VLSSEG5E512_V,VSSSEG5E512_V)
      if(nf == 6)
        oplist2z	+= (VLSSEG6E512_V,VSSSEG6E512_V)
      if(nf == 7)
        oplist2z	+= (VLSSEG7E512_V,VSSSEG7E512_V)
      if(nf == 8)
        oplist2z	+= (VLSSEG8E512_V,VSSSEG8E512_V)
    }
    if(sew == 1024 )
    {
      if (nf == 2)
        oplist2z += (VLSSEG2E1024_V , VSSSEG2E1024_V)
      if(nf == 3)
        oplist2z	+= (VLSSEG3E1024_V,VSSSEG3E1024_V)
      if(nf == 4)
        oplist2z	+= (VLSSEG4E1024_V,VSSSEG4E1024_V)
      if(nf == 5)
        oplist2z	+= (VLSSEG5E1024_V,VSSSEG5E1024_V)
      if(nf == 6)
        oplist2z	+= (VLSSEG6E1024_V,VSSSEG6E1024_V)
      if(nf == 7)
        oplist2z	+= (VLSSEG7E1024_V,VSSSEG7E1024_V)
      if(nf == 8)
        oplist2z	+= (VLSSEG8E1024_V,VSSSEG8E1024_V)
    }
  }

  if(rv_vmem_vect)
  {
    if(sew == 8  || sew == 16 || sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist3	+= (VLXEI8_V,VSXEI8_V)
    if(sew == 16 || sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist3	+= (VLXEI16_V,VSXEI16_V)
    if(sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist3	+= (VLXEI32_V,VSXEI32_V)
    if(sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist3	+= (VLXEI64_V,VSXEI64_V)  
    if(sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
      oplist3	+= (VLXEI128_V,VSXEI128_V)  
    if(sew == 256 || sew == 512 ||  sew == 1024)
      oplist3	+= (VLXEI256_V,VSXEI256_V)  
    if(sew == 512 ||  sew == 1024)
      oplist3	+= (VLXEI512_V,VSXEI512_V) 
    if(sew == 1024)
      oplist3	+= (VLXEI1024_V,VSXEI1024_V)
  }

  if(rv_vmem_zvlsseg)
  {
    if(sew == 8  || sew == 16 || sew == 32 || sew == 64|| sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if(nf == 2)
        oplist3z	+= (VLXSEG2E8_V,VSXSEG2E8_V)
      if(nf == 3)
        oplist3z	+= (VLXSEG3E8_V,VSXSEG3E8_V)
      if(nf == 4)
        oplist3z	+= (VLXSEG4E8_V,VSXSEG4E8_V)
      if(nf == 5)
        oplist3z	+= (VLXSEG5E8_V,VSXSEG5E8_V)
      if(nf == 6)
        oplist3z	+= (VLXSEG6E8_V,VSXSEG6E8_V)
      if(nf == 7)
        oplist3z	+= (VLXSEG7E8_V,VSXSEG7E8_V)
      if(nf == 8)
        oplist3z	+= (VLXSEG8E8_V,VSXSEG8E8_V)
    }
    if(sew == 16 || sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if (nf == 2)
        oplist3z += (VLXSEG2E16_V, VSXSEG2E16_V)
      if(nf == 3)
        oplist3z	+= (VLXSEG3E16_V,VSXSEG3E16_V)
      if(nf ==4)
        oplist3z	+= (VLXSEG4E16_V,VSXSEG4E16_V)
      if(nf ==5)
        oplist3z	+= (VLXSEG5E16_V,VSXSEG5E16_V)
      if(nf ==6)
        oplist3z	+= (VLXSEG6E16_V,VSXSEG6E16_V)
      if(nf ==7)
        oplist3z	+= (VLXSEG7E16_V,VSXSEG7E16_V)
      if(nf ==8)
        oplist3z	+= (VLXSEG8E16_V,VSXSEG8E16_V)
    }
    if(sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024)
    {
      if (nf == 2)
        oplist3z += (VLXSEG2E32_V,VSXSEG2E32_V)
      if(nf == 3)
        oplist3z	+= (VLXSEG3E32_V,VSXSEG3E32_V)
      if(nf == 4)
        oplist3z	+= (VLXSEG4E32_V,VSXSEG4E32_V)
      if(nf == 5)
        oplist3z	+= (VLXSEG5E32_V,VSXSEG5E32_V)
      if(nf == 6)
        oplist3z	+= (VLXSEG6E32_V,VSXSEG6E32_V)
      if(nf == 7)
        oplist3z	+= (VLXSEG7E32_V,VSXSEG7E32_V)
      if(nf == 8)
        oplist3z	+= (VLXSEG8E32_V,VSXSEG8E32_V)
    }
    if(sew == 64 || sew == 128 || sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if (nf == 2)
        oplist3z += (VLXSEG2E64_V , VSXSEG2E64_V)
      if(nf == 3)
        oplist3z	+= (VLXSEG3E64_V,VSXSEG3E64_V)
      if(nf == 4)
        oplist3z	+= (VLXSEG4E64_V,VSXSEG4E64_V)
      if(nf == 5)
        oplist3z	+= (VLXSEG5E64_V,VSXSEG5E64_V)
      if(nf == 6)
        oplist3z	+= (VLXSEG6E64_V,VSXSEG6E64_V)
      if(nf == 7)
        oplist3z	+= (VLXSEG7E64_V,VSXSEG7E64_V)
      if(nf == 8)
        oplist3z	+= (VLXSEG8E64_V,VSXSEG8E64_V)
    }
    if(sew == 128 || sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if (nf == 2)
        oplist3z += (VLXSEG2E128_V , VSXSEG2E128_V)
      if(nf == 3)
        oplist3z	+= (VLXSEG3E128_V,VSXSEG3E128_V)
      if(nf == 4)
        oplist3z	+= (VLXSEG4E128_V,VSXSEG4E128_V)
      if(nf == 5)
        oplist3z	+= (VLXSEG5E128_V,VSXSEG5E128_V)
      if(nf == 6)
        oplist3z	+= (VLXSEG6E128_V,VSXSEG6E128_V)
      if(nf == 7)
        oplist3z	+= (VLXSEG7E128_V,VSXSEG7E128_V)
      if(nf == 8)
        oplist3z	+= (VLXSEG8E128_V,VSXSEG8E128_V)
    }
    if(sew == 256 || sew == 512 ||  sew == 1024 )
    {
      if (nf ==2)
        oplist3z += (VLXSEG2E256_V , VSXSEG2E256_V)
      if(nf == 3)
        oplist3z	+= (VLXSEG3E256_V,VSXSEG3E256_V)
      if(nf == 4)
        oplist3z	+= (VLXSEG4E256_V,VSXSEG4E256_V)
      if(nf == 5)
        oplist3z	+= (VLXSEG5E256_V,VSXSEG5E256_V)
      if(nf == 6)
        oplist3z	+= (VLXSEG6E256_V,VSXSEG6E256_V)
      if(nf == 7)
        oplist3z	+= (VLXSEG7E256_V,VSXSEG7E256_V)
      if(nf == 8)
        oplist3z	+= (VLXSEG8E256_V,VSXSEG8E256_V)
    }
    if(sew == 512 ||  sew == 1024 )
    {
      if (nf == 2)
        oplist3z += (VLXSEG2E512_V , VSXSEG2E512_V)
      if(nf == 3)
        oplist3z	+= (VLXSEG3E512_V,VSXSEG3E512_V)
      if(nf == 4)
        oplist3z	+= (VLXSEG4E512_V,VSXSEG4E512_V)
      if(nf == 5)
        oplist3z	+= (VLXSEG5E512_V,VSXSEG5E512_V)
      if(nf == 6)
        oplist3z	+= (VLXSEG6E512_V,VSXSEG6E512_V)
      if(nf == 7)
        oplist3z	+= (VLXSEG7E512_V,VSXSEG7E512_V)
      if(nf == 8)
        oplist3z	+= (VLXSEG8E512_V,VSXSEG8E512_V)
    }
    if(sew == 1024 )
    {
      if (nf == 2)
        oplist3z += (VLXSEG2E1024_V , VSXSEG2E1024_V)
      if(nf == 3)
        oplist3z	+= (VLXSEG3E1024_V,VSXSEG3E1024_V)
      if(nf == 4)
        oplist3z	+= (VLXSEG4E1024_V,VSXSEG4E1024_V)
      if(nf == 5)
        oplist3z	+= (VLXSEG5E1024_V,VSXSEG5E1024_V)
      if(nf == 6)
        oplist3z	+= (VLXSEG6E1024_V,VSXSEG6E1024_V)
      if(nf == 7)
        oplist3z	+= (VLXSEG7E1024_V,VSXSEG7E1024_V)
      if(nf == 8)
        oplist3z	+= (VLXSEG8E1024_V,VSXSEG8E1024_V)
    }
  }

  //-------------------------------------------------------------------------------------------------------------//
  // ** Vector AMO instructions are only supported for the memory data element widths supported by AMOs in the   //
  // ** implementation’s scalar architecture. Other element widths raise an illegal instruction exception.       //

  if(rv_vamo)
  {
    if(sew == 32 || sew == 64 || sew == 128 || sew == 256 || sew == 512 || sew == 1024)
      oplist4 += (VAMOSWAPEI32_V, VAMOADDEI32_V, VAMOXOREI32_V, VAMOANDEI32_V, VAMOOREI32_V, VAMOMINEI32_V, VAMOMAXEI32_V, VAMOMINUEI32_V, VAMOMAXUEI32_V)
    if(sew == 64 || sew == 128 || sew == 256 || sew == 512 || sew == 1024)
      oplist4 += (VAMOSWAPEI64_V, VAMOADDEI64_V, VAMOXOREI64_V, VAMOANDEI64_V, VAMOOREI64_V, VAMOMINEI64_V, VAMOMAXEI64_V, VAMOMINUEI64_V, VAMOMAXUEI64_V)
    if(sew == 128 || sew == 256 || sew == 512 || sew == 1024)
      oplist4 += (VAMOSWAPEI128_V, VAMOADDEI128_V, VAMOXOREI128_V, VAMOANDEI128_V, VAMOOREI128_V, VAMOMINEI128_V, VAMOMAXEI128_V, VAMOMINUEI128_V, VAMOMAXUEI128_V)
  }

  if(rv_vinteger)
  {
    oplist6	     += (VADD_VV,VSUB_VV,VAND_VV,VOR_VV,VXOR_VV,VSLL_VV,VSRL_VV,VSRA_VV,
		                 VMSEQ_VV,VMSNE_VV,VMSLTU_VV,VMSLT_VV,VMSLEU_VV,VMSLE_VV,VMINU_VV,
		                 VMIN_VV,VMAXU_VV,VMAX_VV,VMUL_VV,VMULH_VV,VMULHU_VV,VMULHSU_VV,VDIVU_VV,VDIV_VV,
	                   VREMU_VV,VREM_VV,VMACC_VV,VNMSAC_VV,VMADD_VV,VNMSUB_VV)

    oplist6_um   += (VMADC_VV,VMSBC_VV)

    if(rv_wide)
      oplist6_w	 += (VWADDU_VV, VWSUBU_VV, VWADD_VV, VWSUB_VV, VWMACCU_VV, VWMACC_VV, VWMACCSU_VV, VWMUL_VV, VWMULU_VV, VWMULSU_VV)
    if(rv_wide)
      oplist6_wx += (VWADDU_WV, VWSUBU_WV, VWADD_WV, VWSUB_WV)
    if(rv_narrow)
      oplist6_n	 += (VNSRL_WV, VNSRA_WV)
  }

  if(rv_vmask)
    oplist6_um += (VMAND_MM,VMNAND_MM,VMANDNOT_MM,VMXOR_MM,VMOR_MM,VMNOR_MM,VMORNOT_MM,VMXNOR_MM)

  if(rv_vfixed)		
  {
    oplist6	+= (VSADDU_VV, VSADD_VV, VSSUBU_VV, VSSUB_VV, VAADDU_VV, VAADD_VV, VASUBU_VV, VASUB_VV, VSMUL_VV, VSSRL_VV, VSSRA_VV)
    
    if(rv_narrow)
      oplist6_n	+= (VNCLIPU_WV,VNCLIP_WV)
  }

  if(rv_vfloat)
  {
    oplist6	+= (VFADD_VV,VFSUB_VV,VFMUL_VV,VFDIV_VV,VFMACC_VV,VFNMACC_VV,VFMSAC_VV,VFNMSAC_VV,
								VMFNE_VV,VMFLT_VV,VFMSUB_VV,VFNMSUB_VV,VFMIN_VV,VFMAX_VV,VFMADD_VV,VFNMADD_VV,
								VFSGNJ_VV,VFSGNJN_VV,VFSGNJX_VV,VMFEQ_VV,VMFLE_VV)	
 
  	if(rv_wide)
  	  oplist6_fw  += (VFWADD_VV,VFWSUB_VV,VFWMUL_VV,VFWMACC_VV,VFWNMACC_VV,VFWMSAC_VV,VFWNMSAC_VV)
  	if(rv_wide)
  	  oplist6_fwx += (VFWADD_WV,VFWSUB_WV)
  }

  if(rv_vreduce)	
  {
    oplist6 += (VREDSUM_VS, VREDMAXU_VS, VREDMAX_VS, VREDMINU_VS, VREDMIN_VS, VREDAND_VS, VREDOR_VS, VREDXOR_VS)
  	
  	if(rv_wide)
  	  oplist6_w += (VWREDSUM_VS,VWREDSUMU_VS)
  	if(rv_vfloat)
  	  oplist6 += (VFREDOSUM_VS,VFREDSUM_VS,VFREDMAX_VS,VFREDMIN_VS)
  	if(rv_wide && rv_vfloat)
  	  oplist6_fw += (VFWREDOSUM_VS,VFWREDSUM_VS)
  }

  if(rv_vpermute)
    oplist6_p += (VRGATHER_VV)

  if(rv_vpermute)
    oplist6_p_um += (VCOMPRESS_VM)

  if(rv_vinteger)
  {
    oplist7 += (VADD_VX,VSUB_VX,VRSUB_VX,VAND_VX,VOR_VX,VXOR_VX,
		            VSLL_VX,VSRL_VX,VSRA_VX,VMSEQ_VX,VMSNE_VX,VMSLTU_VX,VMSLT_VX,VMSLEU_VX,
		            VMSLE_VX,VMSGTU_VX,VMSGT_VX,VMINU_VX,VMIN_VX,VMAXU_VX,VMAX_VX,
		            VMULHU_VX,VMULHSU_VX,VDIVU_VX,VMUL_VX,VMULH_VX,VDIV_VX,VREMU_VX,VREM_VX)

    oplist7_um += (VMADC_VX, VMSBC_VX)

  	if(rv_wide)
  	  oplist7_w	 += (VWADDU_VX,VWSUBU_VX,VWADD_VX,VWSUB_VX,VWMUL_VX,VWMULU_VX,VWMULSU_VX)
  	if(rv_wide)
  	  oplist7_wx += (VWADDU_WX,VWSUBU_WX,VWADD_WX,VWSUB_WX)
  	if(rv_narrow)
  	  oplist7_n	 += (VNSRL_WX,VNSRA_WX)
  }

  if(rv_vfixed)		  
  {
    oplist7 += (VSADDU_VX,VSADD_VX,VSSUBU_VX,VSSUB_VX,VAADDU_VX,VAADD_VX,VASUBU_VX,
								VASUB_VX,VSMUL_VX,VSSRL_VX,VSSRA_VX)

  	if(rv_narrow)
  	  oplist7_n	+= (VNCLIPU_WX,VNCLIP_WX)
  }

  if(rv_vpermute)
    oplist7_p	+= (VSLIDEUP_VX,VSLIDEDOWN_VX,VSLIDE1UP_VX,VSLIDE1DOWN_VX,VRGATHER_VX)

  if(rv_vinteger)
  {
    oplist8 += (VADD_VI,VRSUB_VI,VOR_VI,VAND_VI,VXOR_VI,VMSEQ_VI,VMSNE_VI,
		    				VMSLEU_VI,VMSLE_VI,VMSGTU_VI,VMSGT_VI)

    oplist8_um	+= (VMADC_VI)
  }

  if(rv_vinteger)
    oplist8_i31 += (VSRL_VI,VSRA_VI,VSLL_VI)

  if(rv_vfixed)
  {
    oplist8_f += (VSADDU_VI,VSADD_VI)
    if(rv_narrow)
      oplist8_n += (VNCLIPU_WI,VNCLIP_WI,VNSRA_WI,VNSRL_WI)
  }

  if(rv_vfixed)
    oplist8_f31 += (VSSRL_VI,VSSRA_VI)
  
  if(rv_vpermute)
    oplist8_p += (VSLIDEUP_VI,VSLIDEDOWN_VI,VRGATHER_VI)

  if(rv_vinteger)
    oplist9_um += (VMV_V_V)

  if(rv_vinteger && ext_2)
    oplist9_um += (VZEXT_VF2,VSEXT_VF2)

  if(rv_vinteger && ext_4)
    oplist9_um += (VZEXT_VF4,VSEXT_VF4)

  if(rv_vinteger && ext_8)
    oplist9_um += (VZEXT_VF8,VSEXT_VF8)

  if(rv_vmask)
    oplist9_m_um += (VMSIF_M,VMSOF_M,VIOTA_M)

  if(rv_vmask)
    oplist9_m += (VMSBF_M)

  if(rv_vfloat)
  {
    oplist9 += (VFSQRT_V,VFCLASS_V,VFCVT_XU_F_V,VFCVT_X_F_V,VFCVT_RTZ_XU_F_V,VFCVT_RTZ_X_F_V,
 								VFCVT_F_XU_V,VFCVT_F_X_V)

  	if(rv_wide)
  	  oplist9_w	+= (VFWCVT_XU_F_V,VFWCVT_X_F_V,VFWCVT_RTZ_XU_F_V,VFWCVT_RTZ_X_F_V,
								  	VFWCVT_F_XU_V,VFWCVT_F_X_V,VFWCVT_F_F_V)
    if(rv_narrow)
      oplist9_n += (VFNCVT_XU_F_W,VFNCVT_X_F_W,VFNCVT_RTZ_XU_F_W,VFNCVT_RTZ_X_F_W,VFNCVT_F_XU_W,
						    		VFNCVT_F_X_W,VFNCVT_F_F_W,VFNCVT_ROD_F_F_W)
  }

  if(rv_vpermute)
  {
    if(nr == 1)	oplist9_nr_um += (VMV1R_V)
		if(nr == 2)	oplist9_nr_um += (VMV2R_V)
		if(nr == 4)	oplist9_nr_um += (VMV4R_V)
		if(nr == 8)	oplist9_nr_um += (VMV8R_V)
  }

  if(rv_vinteger)	oplist10_um	+= (VADC_VVM,VMADC_VVM,VSBC_VVM,VMSBC_VVM,VMERGE_VVM)
  if(rv_vinteger)	oplist11_um	+= (VADC_VXM,VMADC_VXM,VSBC_VXM,VMSBC_VXM,VMERGE_VXM)
  if(rv_vinteger)	oplist12_um	+= (VADC_VIM,VMADC_VIM,VMERGE_VIM)

  if(rv_vinteger)	
  {
    oplist13 += (VMACC_VX,VNMSAC_VX,VMADD_VX,VNMSUB_VX)

    if(rv_wide)
      oplist13_w += (VWMACCU_VX,VWMACC_VX,VWMACCSU_VX,VWMACCUS_VX)
  }

  if(rv_vinteger) oplist14_um	+= (VMV_V_X)
  if(rv_vpermute)	oplist14_um	+= (VMV_S_X)
  if(rv_vinteger)	oplist15_um	+= (VMV_V_I)

  if(rv_vfloat)
  {
    oplist16 += (VFADD_VF,VFSUB_VF,VFRSUB_VF,VFMUL_VF,VFDIV_VF,
		     				 VFRDIV_VF,VFMIN_VF,VFMAX_VF,VFSGNJ_VF,VFSGNJN_VF,VFSGNJX_VF,VMFEQ_VF,VMFNE_VF,
		     				 VMFLT_VF,VMFLE_VF,VMFGT_VF,VMFGE_VF)

  	if(rv_wide)	oplist16_w	  += (VFWADD_VF,VFWSUB_VF,VFWMUL_VF)
  	if(rv_wide)	oplist16_wx  += (VFWADD_WF,VFWSUB_WF)
  }

  if(rv_vpermute && rv_vfloat ) oplist16_p += (VFSLIDE1UP_VF,VFSLIDE1DOWN_VF)

  if(rv_vfloat)
  {
    oplist17_um += (VFMACC_VF,VFNMACC_VF,VFMSAC_VF,VFNMSAC_VF,VFMADD_VF,VFNMADD_VF,VFMSUB_VF,VFNMSUB_VF)
  	
  	if(rv_wide)
  	  oplist17_w_um += (VFWMACC_VF,VFWNMACC_VF,VFWMSAC_VF,VFWNMSAC_VF)
  }

  if(rv_vfloat)
    oplist18_um += (VFMV_V_F)

  if(rv_vpermute && rv_vfloat)
    oplist18_um	+= (VFMV_S_F)

  if(rv_vpermute)
    oplist19_um	+= (VMV_X_S)

  if(rv_vmask)
    oplist19 += (VPOPC_M,VFIRST_M)

  if(rv_vmask)
    oplist20 += (VID_V)

  if(rv_vpermute && rv_vfloat && mask)
    oplist21_um	+= (VFMV_F_S)

  if(rv_vfloat)
    oplist22_um	+= (VFMERGE_VFM)

  //-------------------------Instruction Generation---------------------------------------
  val candidates = new ArrayBuffer[() => insts.type]
  val config_candidate = new ArrayBuffer[() => insts.type]

  for (op <- oplist0)	    	{config_candidate += seq_0(op)}
  for (op <- oplist1)		    {candidates += seq_1(op,lmul,rvv_mem_rand(sew))}
  for (op <- oplist1z)	    {candidates += seq_1(op,nf.toString,rvv_mem_rand(sew))}
  for (op <- oplist1_um)		{candidates += seq1_um(op,rvv_mem_rand(sew))}
  for (op <- oplist2)		    {candidates += seq_2(op,lmul,rvv_mem_rand(sew))}
  for (op <- oplist2z)	    {candidates += seq_2(op,nf.toString,rvv_mem_rand(sew))}
  for (op <- oplist3)		    {candidates += seq_3(op,lmul,rvv_mem_rand(sew))}
  for (op <- oplist3z)	    {candidates += seq_3(op,nf.toString,rvv_mem_rand(sew))}
  for (op <- oplist4)		    {candidates += seq_4(op,rvv_mem_rand(sew))}
  for (op <- oplist4)		    {candidates += seq_5(op,rvv_mem_rand(sew))}
  for (op <- oplist6)		    {candidates += seq_6(op)}
  for (op <- oplist6_um)		{candidates += seq6_um(op)}
  for (op <- oplist6_fw)	  {candidates += seq6_w(op)}
  for (op <- oplist6_fwx)	  {candidates += seq6_wx(op)}
  for (op <- oplist6_w)	    {candidates += seq6_w(op)}
  for (op <- oplist6_wx)	  {candidates += seq6_wx(op)}
  for (op <- oplist6_n)	    {candidates += seq6_n(op)}
  for (op <- oplist6_p)	    {candidates += seq6_p(op)}
  for (op <- oplist6_p_um)	{candidates += seq6_p_um(op)}
  for (op <- oplist7)		    {candidates += seq_7(op)}
  for (op <- oplist7_um)		{candidates += seq7_um(op)}
  for (op <- oplist7_w)	    {candidates += seq7_w(op)}
  for (op <- oplist7_wx)	  {candidates += seq7_wx(op)}
  for (op <- oplist7_n)	    {candidates += seq7_n(op)}
  for (op <- oplist7_p)	    {candidates += seq7_p(op)}
  for (op <- oplist8)		    {candidates += seq_8(op,rand_rvv15)}
  for (op <- oplist8_um)		{candidates += seq8_um(op,rand_rvv15)}
  for (op <- oplist8_i31)	  {candidates += seq_8(op,rand_rvv31)}
  for (op <- oplist8_f)	    {candidates += seq_8(op,rand_rvv15)}
  for (op <- oplist8_f31)	  {candidates += seq_8(op,rand_rvv31)}
  for (op <- oplist8_p)	    {candidates += seq8_p(op,rand_rvv31)}
  for (op <- oplist8_n)	    {candidates += seq8_n(op,rand_rvv31)}
  for (op <- oplist9)		    {candidates += seq_9(op)}
  for (op <- oplist9_um)		{candidates += seq9_um(op)}
  for (op <- oplist9_nr_um)	{candidates += seq9_nr_um(op)}
  for (op <- oplist9_m)	    {candidates += seq9_m(op)}
  for (op <- oplist9_m_um)	{candidates += seq9_m_um(op)}
  for (op <- oplist9_w)	    {candidates += seq9_w(op)}
  for (op <- oplist9_n)	    {candidates += seq9_n(op)}
  for (op <- oplist10_um)	  {candidates += seq10_um(op)}
  for (op <- oplist11_um)	  {candidates += seq11_um(op)}
  for (op <- oplist12_um)	  {candidates += seq12_um(op,rand_rvv15)}
  for (op <- oplist13)	    {candidates += seq_13(op)}
  for (op <- oplist13_w)	  {candidates += seq13_w(op)}
  for (op <- oplist14_um)	  {candidates += seq14_um(op)}
  for (op <- oplist15_um)	  {candidates += seq15_um(op,rand_rvv15)}
  for (op <- oplist16)	    {candidates += seq_16(op)}
  for (op <- oplist16_p)	  {candidates += seq16_p(op)}
  for (op <- oplist16_wx)	  {candidates += seq16_wx(op)}
  for (op <- oplist16_w)	  {candidates += seq16_w(op)}
  for (op <- oplist17_um)	  {candidates += seq17_um(op)}
  for (op <- oplist17_w_um) {candidates += seq17_w_um(op)}
  for (op <- oplist18_um)   {candidates += seq18_um(op)}
  for (op <- oplist19)    	{candidates += seq_19(op)}
  for (op <- oplist20)    	{candidates += seq_20(op)}
  for (op <- oplist21_um)   {candidates += seq21_um(op)}
  for (op <- oplist22_um)   {candidates += seq22_um(op)}
  for (op <- default)	    	{candidates += seq_8(op,rand_rvv)}

	if(gen_config && multi_config)
		rand_pick(config_candidate)()
	else
		rand_pick(candidates)()
}
