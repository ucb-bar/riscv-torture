package torture

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqRVV(rvvregs: HWRegPool, xregs: HWRegPool, fregs_s: HWRegPool, fregs_d: HWRegPool, mem: Mem, rv_vmem_unit: Boolean, 
rv_vmem_const: Boolean,rv_vmem_vect: Boolean, 
rv_vinteger: Boolean, rv_vfixed: Boolean, rv_vfloat: Boolean, rv_vreduce: Boolean, rv_vmask: Boolean, 
rv_vpermute: Boolean, rv_wide: Boolean, rv_narrow: Boolean, lmul: String, sew: Int, nr: Int, nf: Int) extends InstSeq
{
  override val seqname = "rvec"
  
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
  

 def seq_1(op: Opcode, addrfn: (Int) => Int) = () =>  //"op vd addr(x[rs1])"
  {
    var dest = dest_n(lmul)
    val reg_addr   = reg_write(xregs)
    val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val addr       = addrfn(mem.size)
    insts += LA(reg_addr,  BaseImm(mem.toString, addr))
    insts += op(reg_dest,  RegImm(reg_addr, 0))
  }

//------------------------------ seq 2 ----------------------------------//
  def seq_2(op: Opcode, addrfn: (Int) => Int) = () =>  //op vd addr(x[rs1]) x[rs2]"
  { 
    var dest = dest_n(lmul)
    val reg_addr   = reg_write(xregs)
    val reg_stride = reg_write(xregs)
    val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val addr       = addrfn(mem.size)
    
    insts += LA(reg_addr,   BaseImm(mem.toString, addr))
    insts += LI(reg_stride, Imm(8))
    insts += op(reg_dest,   RegImm(reg_addr, 0),reg_stride)
  }
  
 //------------------------------ seq 3 ----------------------------------// 
  
  def seq_3(op: Opcode, addrfn: (Int) => Int) = () => //"op vd addr(x[rs1]) vs2"
  {
  	var dest = dest_n(lmul)
  	var src1 = src1_n(lmul, dest)
	val reg_addr   = reg_write(xregs)
	val reg_dest   = vreg_write_dest_n(rvvregs, lmul, dest)
    val src2	   = vreg_write_src1_n(rvvregs, lmul, src1)
    val addr       = addrfn(mem.size)
    insts += LA(reg_addr,   BaseImm(mem.toString, addr))
    insts += VMV_V_I(src2,  Imm(8))
    insts += op(reg_dest,       RegImm(reg_addr,0), src2)
  }

  def seq_6(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var dest = dest_n(lmul)
  	var src1 = dest_n(lmul)
  	var src2 = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_n(rvvregs,lmul,src1)
    val src2_reg = vreg_read_dest_n(rvvregs,lmul,src2)
    insts += op(dest_reg, src1_reg, src2_reg)
  }
 
  def seq6_w(op: Opcode) = () => // "op vd vs1 vs2"
  {	
  	var dest = dest_w(lmul)
  	var src1 = src1_w(lmul,dest)
  	var src2 = src2_w(lmul,dest, src1)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_w(rvvregs,lmul,src1)
    val src2_reg = vreg_read_src2_w(rvvregs,lmul,src2)
    insts += op(dest_reg, src1_reg, src2_reg)
  }
 
  def seq6_n(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var src1 = dest_w(lmul)
    var dest = src1_w(lmul, src1)
  	var src2 = src1_w(lmul, src1)
  	val dest_reg = vreg_write_src1_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg = vreg_read_src1_w(rvvregs,lmul,src2)
    insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq6_wx(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var dest = dest_w(lmul)
  	var src1 = dest_w(lmul)
  	if(lmul=="8")  {src1=dest}
  	var src2 = dest_wo(lmul,dest, src1)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg = vreg_read_src2_w(rvvregs,lmul,src2)
    insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq6_p(op: Opcode) = () => // "op vd vs1 vs2"
  {
    var dest = dest_n(lmul)
  	var src1 = src1_n(lmul,dest)
  	var src2 = src2_n(lmul,dest,src1)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg = vreg_read_src2_n(rvvregs,lmul,src2)
    insts += op(dest_reg, src1_reg, src2_reg)
  }
 
//------------------------------ seq 7 ----------------------------------//
  
  def seq_7(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest = dest_n(lmul)
  	var src1 = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_n(rvvregs,lmul,src1)
    val src2_reg = reg_read_any(xregs)
    insts += op(dest_reg, src1_reg, src2_reg)
  }
  
  def seq7_w(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest = dest_w(lmul)
  	var src1 = src1_w(lmul,dest)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_w(rvvregs,lmul,src1)
    val src2_reg = reg_read_any(xregs)
    insts += op(dest_reg, src1_reg, src2_reg)  
  }
 
  def seq7_wx(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest = dest_w(lmul)
  	var src1 = dest_w(lmul)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg = reg_read_any(xregs)
    insts += op(dest_reg, src1_reg, src2_reg)
  }
  
  def seq7_n(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var src1 = dest_w(lmul)
    var dest = src1_w(lmul,src1)
  	val dest_reg = vreg_write_src1_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg = reg_read_any(xregs)
    insts += op(dest_reg, src1_reg, src2_reg)
  }

 def seq7_p(op: Opcode) = () => // "op vd vs1 x[rs2]"
  {
    var dest = dest_n(lmul)
  	var src1 = src1_n(lmul,dest)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg = reg_read_any(xregs)
    insts += op(dest_reg, src1_reg, src2_reg)
  }
//------------------------------ seq 8 ----------------------------------//

  def seq_8(op: Opcode, immfn: () => Int) = () => // "op vd vs1 imm"
  {
    var dest = dest_n(lmul)
  	var src1 = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg = Imm(immfn())
    insts += op(dest_reg, src1_reg, src2_reg)
  }

 def seq8_p(op: Opcode, immfn: () => Int) = () => // "op vd vs1 imm"
  {
    var dest = dest_n(lmul)
  	var src1 = src1_n(lmul,dest)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg = Imm(immfn())
    insts += op(dest_reg, src1_reg, src2_reg)
  }
  
  def seq8_n(op: Opcode, immfn: () => Int) = () => // "op vd vs1 imm"
  {
    var src1 = dest_w(lmul)
    var dest = src1_w(lmul,src1)
  	val dest_reg = vreg_write_src1_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_w(rvvregs,lmul,src1)
    val src2_reg = Imm(immfn())
    insts += op(dest_reg, src1_reg, src2_reg)
  }

//------------------------------ seq 9 ----------------------------------//

  def seq_9(op: Opcode) = () => // "op vd vs1"
  {
    var dest = dest_w(lmul)
  	var src1 = src1_w(lmul, dest)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_w(rvvregs,lmul,src1)
    insts += op(dest_reg, src1_reg)
  }
  
   def seq9_nr(op: Opcode) = () => // "op vd vs1"
  {
    var dest = dest_n(nr.toString)
  	var src1 = src1_n(nr.toString,dest)
  	val dest_reg = vreg_write_dest_n(rvvregs,nr.toString,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,nr.toString,src1)
    insts += op(dest_reg, src1_reg)
  }


  def seq9_m(op: Opcode) = () => // "op vd vs1"
  {
    var dest = src1_n(lmul,0)
  	var src1 = src1_n(lmul,dest)
  	val dest_reg = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
    insts += op(dest_reg, src1_reg)
  }

  def seq9_w(op: Opcode) = () => // "op vd vs1"
  {
    var dest = dest_w(lmul)
  	var src1 = src1_w(lmul,dest)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_w(rvvregs,lmul,src1)
    insts += op(dest_reg, src1_reg)
  }

  def seq9_n(op: Opcode) = () => // "op vd vs1"
  {
    var src1 = dest_w(lmul)
    var dest = src1_w(lmul,src1)
  	val dest_reg = vreg_write_src1_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_w(rvvregs,lmul,src1)
    insts += op(dest_reg, src1_reg)
  }

//------------------------------ seq 10 ----------------------------------//


  def seq_10(op: Opcode) = () => //"op vd vs1 vs2 v0"
  {
    var dest = src1_n(lmul,0)
  	var src1 = src1_n(lmul,0)
  	var src2 = src2_n(lmul,0, src1)
  	val src3_reg = reg_read_v0(rvvregs)
  	val dest_reg = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg = vreg_read_src2_n(rvvregs,lmul,src2)
    insts += op(dest_reg, src1_reg, src2_reg,src3_reg)
  }

//------------------------------ seq 11 ----------------------------------//

  def seq_11(op: Opcode) = () => // "op vd vs1 x[rs2] v0"
  {
    var dest = src1_n(lmul,0)
  	var src1 = src1_n(lmul,0)
  	val src3_reg = reg_read_v0(rvvregs)
  	val dest_reg = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg = reg_read_any(xregs)
    insts += op(dest_reg, src1_reg, src2_reg,src3_reg)
  }

//------------------------------ seq 12 ----------------------------------//

  def seq_12(op: Opcode, immfn: () => Int) = () => //"op vd vs1 imm v0"
  {
    var dest = src1_n(lmul,0)
  	var src1 = src1_n(lmul,0)
  	val src3_reg = reg_read_v0(rvvregs)
  	val dest_reg = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
    val src2_reg = Imm(immfn())
    insts += op(dest_reg, src1_reg, src2_reg,src3_reg)
  }

//------------------------------ seq 13 ----------------------------------//

  def seq_13(op: Opcode) = () => // "op vd x[rs1] vs2"
  {
    var dest = dest_n(lmul)
  	var src2 = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = reg_read_any(xregs)
    val src2_reg = vreg_read_dest_n(rvvregs,lmul,src2)
    insts += op(dest_reg, src1_reg, src2_reg) 
  }

  def seq13_w(op: Opcode) = () => // "op vd x[rs1] vs2"
  {
    var dest = dest_w(lmul)
  	var src2 = src1_w(lmul,dest)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = reg_read_any(xregs)
    val src2_reg = vreg_read_src1_w(rvvregs,lmul,src2)
    insts += op(dest_reg, src1_reg, src2_reg)
  }

//------------------------------ seq 14 ----------------------------------//

  def seq_14(op: Opcode) = () => //"op vd x[rs1]"
  {
    var dest = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = reg_read_any(xregs)
    insts += op(dest_reg, src1_reg)
  }

//------------------------------ seq 15 ----------------------------------//

  def seq_15(op: Opcode, immfn: () => Int) = () => //"op vd imm"
  {
    var dest = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = Imm(immfn())
    insts += op(dest_reg, src1_reg)
  }

//------------------------------ seq 16 ----------------------------------//

  def seq_16(op: Opcode) = () => //"op vd vs1 f[rs2]"
  {
    var dest = dest_n(lmul)
  	var src1 = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_n(rvvregs,lmul,src1)
	val src2_reg = reg_read_any(fregs_s)  
    insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq16_w(op: Opcode) = () => //"op vd vs1 f[rs2]"
  {
    var dest = dest_w(lmul)
  	var src1 = src1_w(lmul,dest)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_w(rvvregs,lmul,src1)
	val src2_reg = reg_read_any(fregs_s)  
    insts += op(dest_reg, src1_reg, src2_reg)
  }

  def seq16_wx(op: Opcode) = () => //"op vd vs1 f[rs2]"
  {
    var dest = dest_w(lmul)
  	var src1 = dest_w(lmul)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_w(rvvregs,lmul,src1)
	val src2_reg = reg_read_any(fregs_s)  
    insts += op(dest_reg, src1_reg, src2_reg)
  }
  
  def seq16_p(op: Opcode) = () => //"op vd vs1 f[rs2]"
  {
    var dest = dest_n(lmul)
  	var src1 = src1_n(lmul,dest)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
	val src2_reg = reg_read_any(fregs_s)  
    insts += op(dest_reg, src1_reg, src2_reg)
  }
  
//------------------------------ seq 17 ----------------------------------//

  def seq_17(op: Opcode) = () => //"op vd f[rs1] vs2"
  {
    var dest = dest_n(lmul)
  	var src1 = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_dest_n(rvvregs,lmul,src1)
	val src2_reg = reg_read_any(fregs_s)  
    insts += op(dest_reg, src2_reg, src1_reg)
  }

  def seq17_w(op: Opcode) = () => //"op vd f[rs1] vs2"
  {
    var dest = dest_w(lmul)
  	var src1 = src1_w(lmul,dest)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_w(rvvregs,lmul,src1)
	val src2_reg = reg_read_any(fregs_s)  
    insts += op(dest_reg, src2_reg, src1_reg)
  }

//------------------------------ seq 18 ----------------------------------//

  def seq_18(op: Opcode) = () => //"op vd f[rs1]"
  {
    var dest = dest_n(lmul)
  	val dest_reg = vreg_write_dest_n(rvvregs,lmul,dest)
	val src1_reg = reg_read_any(fregs_s)  
    insts += op(dest_reg, src1_reg) 
  }

//------------------------------ seq 19 ----------------------------------//

  def seq_19(op: Opcode) = () => //"op x[rd] vs1"
  {
    var src1 = dest_n(lmul)
  	val dest_reg = reg_write(xregs)
	val src1_reg = vreg_write_dest_n(rvvregs,lmul,src1)
    insts += op(dest_reg, src1_reg)
  }

//------------------------------ seq 20 ----------------------------------//

  def seq_20(op: Opcode) = () => //"op vd"
  {
    var dest = dest_n(lmul)
  	val dest_reg = vreg_write_dest_w(rvvregs,lmul,dest)
    insts += op(dest_reg)
  }

//------------------------------ seq 21 ----------------------------------//

  def seq_21(op: Opcode) = () => //"op f[rd] vs1"
  {
  	var src1 = dest_n(lmul)
	val dest_reg = reg_write(fregs_s)
    val src1_reg = vreg_read_dest_n(rvvregs,lmul,src1)
    insts += op(dest_reg, src1_reg)
  }

//------------------------------ seq 22 ----------------------------------//

  def seq_22(op: Opcode) = () => // "op vd vs1 f[rs2] v0"
  {
    var dest = src1_n(lmul,0)
  	var src1 = src1_n(lmul,0)
  	val src3_reg = reg_read_v0(rvvregs)
  	val dest_reg = vreg_write_src1_n(rvvregs,lmul,dest)
    val src1_reg = vreg_read_src1_n(rvvregs,lmul,src1)
	val src2_reg = reg_read_any(fregs_s)  
    insts += op(dest_reg, src1_reg, src2_reg, src3_reg)
  }

//------------------------------------------- oplists --------------------------------------------//

  val oplist1		= new ArrayBuffer[Opcode]
  val oplist2		= new ArrayBuffer[Opcode]
  val oplist3		= new ArrayBuffer[Opcode]

  val oplist6		= new ArrayBuffer[Opcode]
  val oplist6_w		= new ArrayBuffer[Opcode]
  val oplist6_wx	= new ArrayBuffer[Opcode]
  val oplist6_n		= new ArrayBuffer[Opcode]
  val oplist6_p		= new ArrayBuffer[Opcode]
  val oplist6_fw	= new ArrayBuffer[Opcode]
  val oplist6_fwx	= new ArrayBuffer[Opcode]
  val oplist7		= new ArrayBuffer[Opcode]
  val oplist7_w		= new ArrayBuffer[Opcode]
  val oplist7_wx	= new ArrayBuffer[Opcode]
  val oplist7_n		= new ArrayBuffer[Opcode]
  val oplist7_p		= new ArrayBuffer[Opcode]
  val oplist8		= new ArrayBuffer[Opcode]
  val oplist8_p		= new ArrayBuffer[Opcode]
  val oplist8_n		= new ArrayBuffer[Opcode]
  val oplist8_f		= new ArrayBuffer[Opcode]
  val oplist8_f31	= new ArrayBuffer[Opcode]
  val oplist8_i31	= new ArrayBuffer[Opcode]
  val oplist9		= new ArrayBuffer[Opcode]
  val oplist9_nr	= new ArrayBuffer[Opcode]
  val oplist9_m		= new ArrayBuffer[Opcode]
  val oplist9_w		= new ArrayBuffer[Opcode]
  val oplist9_n		= new ArrayBuffer[Opcode]
  val oplist10		= new ArrayBuffer[Opcode]
  val oplist11		= new ArrayBuffer[Opcode]
  val oplist12		= new ArrayBuffer[Opcode]
  val oplist13		= new ArrayBuffer[Opcode]
  val oplist13_w	= new ArrayBuffer[Opcode]
  val oplist14		= new ArrayBuffer[Opcode]
  val oplist15		= new ArrayBuffer[Opcode]
  val oplist16		= new ArrayBuffer[Opcode]
  val oplist16_w	= new ArrayBuffer[Opcode]
  val oplist16_wx	= new ArrayBuffer[Opcode]
  val oplist16_p	= new ArrayBuffer[Opcode]
  val oplist17		= new ArrayBuffer[Opcode]
  val oplist17_w	= new ArrayBuffer[Opcode]
  val oplist18		= new ArrayBuffer[Opcode]
  val oplist19		= new ArrayBuffer[Opcode]
  val oplist20		= new ArrayBuffer[Opcode]
  val oplist21		= new ArrayBuffer[Opcode]
  val oplist22		= new ArrayBuffer[Opcode]
  val default 		= new ArrayBuffer[Opcode]

 def rvv_mem_rand(sew: Int) : Int=> Int= // "op vd vs1 f[rs2] v0"
  {
	if(sew == 16){return rand_addr_h_rvv}
	if(sew == 32){return rand_addr_w_rvv}
	if(sew == 64){return rand_addr_d_rvv}
    rand_addr_b_rvv
  }

  var check = rv_vmem_unit | rv_vmem_const | rv_vmem_vect | rv_vfloat | rv_vfixed | rv_vpermute | rv_vreduce | rv_vmask | rv_vinteger 
if(!check)
{default += (VADD_VI,VRSUB_VI,VOR_VI,VAND_VI,VXOR_VI,VMADC_VI,VMSEQ_VI,VMSNE_VI, VMSLEU_VI,VMSLE_VI,VMSGTU_VI,VMSGT_VI)}

//-----------------------------------------------------------------------------------------------------------------//

if (rv_vmem_unit){
                    if(sew == 8  || sew == 16 || sew == 32 || sew == 64) {oplist1	+= (VLE8_V,VSE8_V,VLE8FF_V)}
			              if(sew == 16 || sew == 32 || sew == 64 )             {oplist1	+= (VLE16_V,VSE16_V,VLE16FF_V)}
			              if(sew == 32 || sew == 64)                           {oplist1	+= (VLE32_V,VSE32_V,VLE32FF_V)}
			              if(sew == 64 )                                       {oplist1	+= (VLE64_V,VSE64_V,VLE64FF_V)}
                  }
			 
if (rv_vmem_unit){if(nf == 1){oplist1	+= (VL1R_V,VS1R_V)}}
			           
//----------------------- oplist 2 --------------------------------------//

if (rv_vmem_const){ 
                    if(sew == 8  || sew == 16 || sew == 32 || sew == 64) {oplist2	+= (VLSE8_V,VSSE8_V)}
			              if(sew == 16 || sew == 32 || sew == 64 )             {oplist2	+= (VLSE16_V,VSSE16_V)}
			              if(sew == 32 || sew == 64)                           {oplist2	+= (VLSE32_V,VSSE32_V)}
			              if(sew == 64 )                                       {oplist2	+= (VLSE64_V,VSSE64_V)}
                  }

//----------------------- oplist 3 --------------------------------------//

if (rv_vmem_vect){
                    if(sew == 8  || sew == 16 || sew == 32 || sew == 64)  {oplist3	+= (VLXEI8_V,VSXEI8_V,VSUXEI8_V)}
			              if(sew == 16 || sew == 32 || sew == 64 )              {oplist3	+= (VLXEI16_V,VSXEI16_V,VSUXEI16_V)}
			              if(sew == 32 || sew == 64)                            {oplist3	+= (VLXEI32_V,VSXEI32_V,VSUXEI32_V)}
			              if(sew == 64 )                                        {oplist3	+= (VLXEI64_V,VSXEI64_V,VSUXEI64_V)}
                 }

//------------------------------------------------------------------------------------------------------------------//


if (rv_vinteger)	{oplist6	+= (VADD_VV,VSUB_VV,VMADC_VV,VMSBC_VV,VAND_VV,VOR_VV,VXOR_VV,VSLL_VV,VSRL_VV,VSRA_VV,
		    						VMSEQ_VV,VMSNE_VV,VMSLTU_VV,VMSLT_VV,VMSLEU_VV,VMSLE_VV,VMINU_VV,
		    						VMIN_VV,VMAXU_VV,VMAX_VV,VMUL_VV,VMULH_VV,VMULHU_VV,VMULHSU_VV,VDIVU_VV,VDIV_VV,
 									VREMU_VV,VREM_VV,VMACC_VV,VNMSAC_VV,VMADD_VV,VNMSUB_VV)
	if (rv_wide)	{oplist6_w	+= (VWADDU_VV,VWSUBU_VV,VWADD_VV,VWSUB_VV,VWMACCU_VV,VWMACC_VV,VWMACCSU_VV,VWMUL_VV,
									VWMULU_VV,VWMULSU_VV)}
	if (rv_wide)	{oplist6_wx	+= (VWADDU_WV,VWSUBU_WV,VWADD_WV,VWSUB_WV)}	 
	if (rv_narrow)	{oplist6_n	+= (VNSRL_WV,VNSRA_WV)}}
if (rv_vmask)		{oplist6	+= (VMAND_MM,VMNAND_MM,VMANDNOT_MM,VMXOR_MM,VMOR_MM,VMNOR_MM,VMORNOT_MM,VMXNOR_MM)}
if (rv_vfixed)		{oplist6	+= (VSADDU_VV,VSADD_VV,VSSUBU_VV,VSSUB_VV,VAADDU_VV,VAADD_VV,VASUBU_VV,VASUB_VV,
									VSMUL_VV,VSSRL_VV,VSSRA_VV) 
    if (rv_narrow)	{oplist6_n	+= (VNCLIPU_WV,VNCLIP_WV)}}
if (rv_vfloat)		{oplist6	+= (VFADD_VV,VFSUB_VV,VFMUL_VV,VFDIV_VV,VFMACC_VV,VFNMACC_VV,VFMSAC_VV,VFNMSAC_VV,
									VMFNE_VV,VMFLT_VV,VFMSUB_VV,VFNMSUB_VV,VFMIN_VV,VFMAX_VV,VFMADD_VV,VFNMADD_VV,
									VFSGNJ_VV,VFSGNJN_VV,VFSGNJX_VV,VMFEQ_VV,VMFLE_VV)					   			
	if(rv_wide)		{oplist6_fw	+= (VFWADD_VV,VFWSUB_VV,VFWMUL_VV,VFWMACC_VV,VFWNMACC_VV,VFWMSAC_VV,VFWNMSAC_VV)}
	if(rv_wide)		{oplist6_fwx+= (VFWADD_WV,VFWSUB_WV)}}
if (rv_vreduce)		{oplist6	+= (VREDSUM_VS,VREDMAXU_VS,VREDMAX_VS,VREDMINU_VS,VREDMIN_VS,VREDAND_VS,VREDOR_VS,VREDXOR_VS)
	if(rv_wide)		{oplist6_w	+= (VWREDSUM_VS,VWREDSUMU_VS)}
	if(rv_vfloat)	{oplist6	+= (VFREDOSUM_VS,VFREDSUM_VS,VFREDMAX_VS,VFREDMIN_VS)
		if(rv_wide)	{oplist6_fw	+= (VFWREDOSUM_VS,VFWREDSUM_VS)}}}
if (rv_vpermute)	{oplist6_p	+= (VRGATHER_VV, VCOMPRESS_VM)} 

if(rv_vinteger)		{oplist7	+= (VADD_VX,VSUB_VX,VRSUB_VX,VMADC_VX,VMSBC_VX,VAND_VX,VOR_VX,VXOR_VX,
		    						VSLL_VX,VSRL_VX,VSRA_VX,VMSEQ_VX,VMSNE_VX,VMSLTU_VX,VMSLT_VX,VMSLEU_VX,
		    						VMSLE_VX,VMSGTU_VX,VMSGT_VX,VMINU_VX,VMIN_VX,VMAXU_VX,VMAX_VX,
		    						VMULHU_VX,VMULHSU_VX,VDIVU_VX,VMUL_VX,VMULH_VX,VDIV_VX,VREMU_VX,VREM_VX)
	if (rv_wide)	{oplist7_w	+= (VWADDU_VX,VWSUBU_VX,VWADD_VX,VWSUB_VX,VWMUL_VX,VWMULU_VX,VWMULSU_VX)}
	if (rv_wide)	{oplist7_wx	+= (VWADDU_WX,VWSUBU_WX,VWADD_WX,VWSUB_WX)}
	if (rv_narrow)	{oplist7_n	+= (VNSRL_WX,VNSRA_WX)}}
if(rv_vfixed)		{oplist7	+= (VSADDU_VX,VSADD_VX,VSSUBU_VX,VSSUB_VX,VAADDU_VX,VAADD_VX,VASUBU_VX,
									VASUB_VX,VSMUL_VX,VSSRL_VX,VSSRA_VX)
	if (rv_narrow)	{oplist7_n	+= (VNCLIPU_WX,VNCLIP_WX)}}
if(rv_vpermute)		{oplist7_p	+= (VSLIDEUP_VX,VSLIDEDOWN_VX,VSLIDE1UP_VX,VSLIDE1DOWN_VX,VRGATHER_VX)} 
    
if(rv_vinteger)		{oplist8	+= (VADD_VI,VRSUB_VI,VOR_VI,VAND_VI,VXOR_VI,VMADC_VI,VMSEQ_VI,VMSNE_VI,
		    						VMSLEU_VI,VMSLE_VI,VMSGTU_VI,VMSGT_VI)}
if(rv_vinteger)		{oplist8_i31+= (VSRL_VI,VSRA_VI,VSLL_VI)}		    						
if(rv_vfixed)		{oplist8_f	+= (VSADDU_VI,VSADD_VI)
	if(rv_narrow)	{oplist8_n	+= (VNCLIPU_WI,VNCLIP_WI,VNSRA_WI,VNSRL_WI)}}
if(rv_vfixed)		{oplist8_f31+= (VSSRL_VI,VSSRA_VI)}		    
if(rv_vpermute)		{oplist8_p	+= (VSLIDEUP_VI,VSLIDEDOWN_VI,VRGATHER_VI)}

if(rv_vinteger)		{oplist9	+= (VMV_V_V)}
    val lmul_ext_2 = List("1","2","4","8","f2", "f4")
    val lmul_ext_4 = List("1","2","4","8","f2")
    val lmul_ext_8 = List("1","2","4","8")
 	val ext_2 = (sew == 16 || sew == 32 || sew == 64) && lmul_ext_2.contains(lmul)
 	val ext_4 = (sew == 32 || sew == 64) && lmul_ext_4.contains(lmul)
 	val ext_8 = (sew == 64) && lmul_ext_8.contains(lmul)
if(rv_vinteger && ext_2) {oplist9 +=  (VZEXT_VF2,VSEXT_VF2)}
if(rv_vinteger && ext_4) {oplist9 +=  (VZEXT_VF4,VSEXT_VF4)}
if(rv_vinteger && ext_8) {oplist9 +=  (VZEXT_VF8,VSEXT_VF8)}

if(rv_vmask)		{oplist9_m	+= (VMSBF_M,VMSIF_M,VMSOF_M,VIOTA_M)}
if(rv_vfloat)		{oplist9	+= (VFSQRT_V,VFCLASS_V,VFCVT_XU_F_V,VFCVT_X_F_V,VFCVT_RTZ_XU_F_V,VFCVT_RTZ_X_F_V,
 									VFCVT_F_XU_V,VFCVT_F_X_V)
	if(rv_wide)		{oplist9_w	+= (VFWCVT_XU_F_V,VFWCVT_X_F_V,VFWCVT_RTZ_XU_F_V,VFWCVT_RTZ_X_F_V,
									VFWCVT_F_XU_V,VFWCVT_F_X_V,VFWCVT_F_F_V)}
	if(rv_narrow)	{oplist9_n	+= (VFNCVT_XU_F_W,VFNCVT_X_F_W,VFNCVT_RTZ_XU_F_W,VFNCVT_RTZ_X_F_W,VFNCVT_F_XU_W,
						    		VFNCVT_F_X_W,VFNCVT_F_F_W,VFNCVT_ROD_F_F_W)}}

if(rv_vpermute){if(nr == 1)	{oplist9_nr	+= (VMV1R_V)}
				if(nr == 2)	{oplist9_nr	+= (VMV2R_V)}
				if(nr == 4)	{oplist9_nr	+= (VMV4R_V)}
				if(nr == 8)	{oplist9_nr	+= (VMV8R_V)}}

if(rv_vinteger)		{oplist10	+= (VADC_VVM,VMADC_VVM,VSBC_VVM,VMSBC_VVM,VMERGE_VVM)}

if(rv_vinteger)		{oplist11	+= (VADC_VXM,VMADC_VXM,VSBC_VXM,VMSBC_VXM,VMERGE_VXM)}

if(rv_vinteger)		{oplist12	+= (VADC_VIM,VMADC_VIM,VMERGE_VIM)}

if(rv_vinteger)		{oplist13	+= (VMACC_VX,VNMSAC_VX,VMADD_VX,VNMSUB_VX)
	if(rv_wide)		{oplist13_w	+= (VWMACCU_VX,VWMACC_VX,VWMACCSU_VX,VWMACCUS_VX)}}

if(rv_vinteger)		{oplist14	+= (VMV_V_X)}
if(rv_vpermute)		{oplist14	+= (VMV_S_X)}  
 
if(rv_vinteger)		{oplist15	+= (VMV_V_I)}

if(rv_vfloat)		{oplist16	+= (VFADD_VF,VFSUB_VF,VFRSUB_VF,VFMUL_VF,VFDIV_VF,
		     						VFRDIV_VF,VFMIN_VF,VFMAX_VF,VFSGNJ_VF,VFSGNJN_VF,VFSGNJX_VF,VMFEQ_VF,VMFNE_VF,
		     						VMFLT_VF,VMFLE_VF,VMFGT_VF,VMFGE_VF)
	if(rv_wide)		{oplist16_w	+= (VFWADD_VF,VFWSUB_VF,VFWMUL_VF)}
	if(rv_wide)		{oplist16_wx+= (VFWADD_WF,VFWSUB_WF)}}
if(rv_vpermute && rv_vfloat )		{oplist16_p	+= (VFSLIDE1UP_VF,VFSLIDE1DOWN_VF)}	

if(rv_vfloat)		{oplist17	+= (VFMACC_VF,VFNMACC_VF,VFMSAC_VF,VFNMSAC_VF,VFMADD_VF,VFNMADD_VF,VFMSUB_VF,VFNMSUB_VF)
	if(rv_wide)		{oplist17_w	+= (VFWMACC_VF,VFWNMACC_VF,VFWMSAC_VF,VFWNMSAC_VF)}}

if(rv_vfloat)		{oplist18	+= (VFMV_V_F)}
if(rv_vpermute && rv_vfloat )		{oplist18	+= (VFMV_S_F)}

if(rv_vpermute)		{oplist19	+= (VMV_X_S)}
if(rv_vmask)		{oplist19	+= (VPOPC_M,VFIRST_M)}

if(rv_vmask)		{oplist20	+= (VID_V)}

if(rv_vpermute && rv_vfloat )		{oplist21	+= (VFMV_F_S)} 

if(rv_vfloat)		{oplist22	+= (VFMERGE_VFM)}


val candidates = new ArrayBuffer[() => insts.type]

//-------------------------Instruction Generation------------------------------------------------

for (op <- oplist1)		{candidates += seq_1(op,rvv_mem_rand(sew))}
for (op <- oplist2)		{candidates += seq_2(op,rvv_mem_rand(sew))}
for (op <- oplist3)		{candidates += seq_3(op,rvv_mem_rand(sew))}
for (op <- oplist6)		{candidates += seq_6(op)}
for (op <- oplist6_fw)	{candidates += seq6_w(op)}
for (op <- oplist6_fwx)	{candidates += seq6_wx(op)}
for (op <- oplist6_w)	{candidates += seq6_w(op)}
for (op <- oplist6_wx)	{candidates += seq6_wx(op)}
for (op <- oplist6_n)	{candidates += seq6_n(op)}
for (op <- oplist6_p)	{candidates += seq6_p(op)}
for (op <- oplist7)		{candidates += seq_7(op)}
for (op <- oplist7_w)	{candidates += seq7_w(op)}
for (op <- oplist7_wx)	{candidates += seq7_wx(op)}
for (op <- oplist7_n)	{candidates += seq7_n(op)}
for (op <- oplist7_p)	{candidates += seq7_p(op)}
for (op <- oplist8)		{candidates += seq_8(op,rand_rvv15)}
for (op <- oplist8_i31)	{candidates += seq_8(op,rand_rvv31)}
for (op <- oplist8_f)	{candidates += seq_8(op,rand_rvv15)}
for (op <- oplist8_f31)	{candidates += seq_8(op,rand_rvv31)}
for (op <- oplist8_p)	{candidates += seq8_p(op,rand_rvv31)}
for (op <- oplist8_n)	{candidates += seq8_n(op,rand_rvv31)}
for (op <- oplist9)		{candidates += seq_9(op)}
for (op <- oplist9_nr)	{candidates += seq9_nr(op)}
for (op <- oplist9_m)	{candidates += seq9_m(op)}
for (op <- oplist9_w)	{candidates += seq9_w(op)}
for (op <- oplist9_n)	{candidates += seq9_n(op)}
for (op <- oplist10)	{candidates += seq_10(op)}
for (op <- oplist11)	{candidates += seq_11(op)}
for (op <- oplist12)	{candidates += seq_12(op,rand_rvv15)}
for (op <- oplist13)	{candidates += seq_13(op)}
for (op <- oplist13_w)	{candidates += seq13_w(op)}
for (op <- oplist14)	{candidates += seq_14(op)}
for (op <- oplist15)	{candidates += seq_15(op,rand_rvv15)}
for (op <- oplist16)	{candidates += seq_16(op)}
for (op <- oplist16_p)	{candidates += seq16_p(op)}
for (op <- oplist16_wx)	{candidates += seq16_wx(op)}
for (op <- oplist16_w)	{candidates += seq16_w(op)}
for (op <- oplist17)	{candidates += seq_17(op)}
for (op <- oplist17_w)	{candidates += seq17_w(op)}
for (op <- oplist18)	{candidates += seq_18(op)}
for (op <- oplist19)	{candidates += seq_19(op)}
for (op <- oplist20)	{candidates += seq_20(op)}
for (op <- oplist21)	{candidates += seq_21(op)}
for (op <- oplist22)	{candidates += seq_22(op)}
for (op <- default)		{candidates += seq_8(op,rand_rvv)}

  rand_pick(candidates)()
}
