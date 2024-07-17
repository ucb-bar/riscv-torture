package torture

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import Rand._
import java.util.Date
import java.text.DateFormat

class ProgSeg(val name: String)
{
  var insts = new ArrayBuffer[Inst]

  override def toString = ((name + ":\n") /: insts.map((x) => "\t" + x + "\n"))(_ + _)
}

object ProgSeg
{
  var cnt = 0

  def apply() =
  {
    val res = new ProgSeg("pseg_" + cnt)
    cnt += 1
    res
  }
}

class Prog(memsize: Int, veccfg: Map[String,String], loop : Boolean)
{
  // Setup scalar core memory
  val core_memory = new Mem("test_memory", memsize)

  // Setup register pools
  val num_vxregs = rand_range(5, 256)
  val use_pop = veccfg.getOrElse("pop", "true") == "true"
  val pred_alu = veccfg.getOrElse("pred_alu", "true") == "true"
  val pred_mem = veccfg.getOrElse("pred_mem", "true") == "true"
  val min_pregs   = if(pred_alu || pred_mem || use_pop) 2 else 1
  val num_vpregs = rand_range(min_pregs, 16)
  val num_vsregs = veccfg.getOrElse("numsregs","64").toInt
  val max_vl = (Math.floor(256/(num_vxregs-1))).toInt * 8
  val used_vl = Math.min(max_vl, rand_range(1, max_vl))

  val xregs = new XRegsPool()
  val fregs = new FRegsMaster()
  val vregs = new VRegsMaster(num_vxregs, num_vpregs, num_vsregs)
  //-------------- RISC-V Vector Registers --------------------
  val rvvregs = new  RISCV_VRegsPool()
  //-----------------------------------------------------------
  val fregpools = fregs.extract_pools()
  val vregpools = vregs.extract_pools()
  val (fregs_s, fregs_d) = (fregpools(0), fregpools(1))
  val (vxregs, vpregs, vsregs, varegs) = (vregpools(0), vregpools(1), vregpools(2), vregpools(3))

  val seqs = new ArrayBuffer[InstSeq]
  val seqs_active = new ArrayBuffer[InstSeq]
  val progsegs = new ArrayBuffer[ProgSeg]

  var killed_seqs = 0
  var nseqs = 0
  var prob_tbl = new ArrayBuffer[(Int, ()=>InstSeq)]

  val opstats = new HashMap[String, scala.collection.mutable.Map[String,Int]]
  val catstats = new HashMap[String,Int]
  val seqstats = new HashMap[String,Int].withDefaultValue(0)
  val vseqstats = new HashMap[String,Int].withDefaultValue(0)
  val regstats = new HashMap[String,Int].withDefaultValue(0)
    for (cat <- List(("alu"),("cmp"),("branch"),("jalr"),                       // Added RISC-V Vector functionality
    ("jmp"),("la"),("mem"),("amo"),("misc"),("fpalu"),("fpcmp"),
    ("fpfma"),("fpmem"),("fpcvt"),("fpmisc"),("Hwacha_vmem"),("Hwacha_vamo"),("Hwacha_valu"),
    ("Hwacha_vmisc"),("Hwacha_vfpalu"),("Hwacha_vfpfma"),("Hwacha_vfpcvt"),("Hwacha_vsmem"),("Hwacha_vshared"),("Hwacha_vpred"),("Hwacha_vcmp"),
    ("rvv_vmem_unit"),("rvv_vmem_const"),("rvv_vmem_vect"),("rvv_vinteger"),("rvv_vfixed"),
    ("rvv_vfloat"),("rvv_vreduce"),("rvv_vmask"),("rvv_vpermute"),("rvv_vamo"),("rvv_vmem_zvlsseg"),("rvv_vconfig"),("unknown")))
    {
      catstats(cat)=0
      opstats(cat) = new HashMap[String,Int].withDefaultValue(0)
    }
  var instcnt = 0

  def seqs_not_allocated = seqs.filter((x) => !x.allocated)
  def is_seqs_empty = seqs_not_allocated.length == 0
  def is_seqs_active_empty = seqs_active.length == 0

  def are_pools_fully_unallocated = List(xregs, fregs_s, fregs_d, vxregs, vpregs, vsregs, varegs, rvvregs).forall(_.is_fully_unallocated)

  def seqs_find_active(): Unit =
  {
    for (seq <- seqs_not_allocated)
    {
      xregs.backup()
      fregs.backup()
      vregs.backup()
      rvvregs.backup() // RISC-V Vector


      if (seq.allocate_regs())
      {
        seqs_active += seq
      }
      else
      {
        if (are_pools_fully_unallocated)
        {
          seqs -= seq
          killed_seqs += 1
          seqstats(seq.seqname) -= 1
          if (seq.seqname == "vec")
          {
            for ((seqname, seqcnt) <- seq.asInstanceOf[SeqVec].vseqstats)
            {
              vseqstats(seqname) = seqcnt
            }
          }
          if (killed_seqs < (nseqs*5))
            gen_seq()
        }
        xregs.restore()
        fregs.restore()
        vregs.restore()
        rvvregs.restore()  // RISC-V Vector

        return
      }
    }
  }

  var jalr_labels = new ArrayBuffer[Label]

  def update_stats(inst: Inst) =
  { 
    catstats(inst.optype) += 1
    opstats(inst.optype)(inst.opcode) += 1
    for (operand <- inst.operands)
    {
      if (operand.isInstanceOf[Reg])
      {
        regstats(operand.toString) += 1
      }
    }
    instcnt += 1
  }
  def register_stats(use_vec: Boolean): String =
  {
    def register_lt(reg1: (String, Int), reg2: (String, Int)): Boolean =
    {       
      val reghash = HashMap('x'->1,'f'->2,'v'->3,'p'->4,'s'->5,'a'->6)
      val regname1 = reg1._1
      val regname2 = reg2._1

      if (reghash(regname1(0)) == reghash(regname2(0)))
      {
        if (regname1(0) == 'v' && use_vec)
        {
          if (regname1(1) == regname2(1))
          {
            return (regname1.substring(2).toInt < regname2.substring(2).toInt)
          } 
          else 
          {
            return (reghash(regname1(1)) < reghash(regname2(1)))
          }
        } 
        else 
        {
          return (regname1.substring(1).toInt < regname2.substring(1).toInt)
        }
      } 
      else
      {
        return (reghash(regname1(0)) < reghash(regname2(0)))
      }
    }
    val sortedRegs = regstats.toSeq.sortWith(register_lt) //TODO: Better way to sort?
    var s = "------------------------ Register Accesses -------------------------------\n"
    s += "--------------------------------------------------------------------------\n"
    for ((regname, cnt) <- sortedRegs)
    {
      s += "---------- " + regname + ": " + cnt + " ----------\n"
    }
    s
  }

  def sequence_stats(mix: Map[String, Int], vecmix: Map[String, Int], nseqs: Int, vnseq: Int, vfnum: Int): String = 
  {  
    def seq_lt(seq1: (String, Int), seq2: (String, Int)): Boolean =
    {
      val seqhash = HashMap("xmem"->1,"xbranch"->2,"xalu"->3,"vmem"->4,        // Added RISC-V Vector functionality
      "fgen"->5,"fpmem"->6,"fax"->7,"fdiv"->8,"vec"->9,"vonly"->10,"valu"->11,"rvv_vmem_unit"->13,"rvv_vmem_const"->14,"rvv_vmem_vect"->15,"rvv_vinteger"->16,"rvv_vfixed"->17,"rvv_vfloat"->18,"rvv_vreduce"->19,"rvv_vmask"->20,"rvv_vpermute"->21, "rvv_vamo"->22, "rvv_vmem_zvlsseg"->23, "rvv_vconfig"->24,
      "unknown"->25).withDefaultValue(100)
      if (seqhash(seq1._1) == 100 && seqhash(seq2._1) == 100) return (seq1._1 < seq2._1)
      return seqhash(seq1._1) < seqhash(seq2._1)
    }
   
   
    val sortedMix = mix.toSeq.sortWith(seq_lt)
    val sortedVecmix = vecmix.toSeq.sortWith(seq_lt)
    var s = "----- Sequence Types Used:"
    for ((seqtype,percent) <- sortedMix) if (percent > 0) s += " " + seqtype.toUpperCase
    s += " ----\n"
    s += "--------------------------------------------------------------------------\n"
    s += "---------- Configured Sequence Mix ----------\n"
    for ((seqtype, percent) <- sortedMix)
    {
      s += "---------- " + seqtype + ": " + percent + "% ----------\n"
    }
    s += "--------------------------------------------------------------------------\n"
    s += "---------- Configured Hwacha Vector Sequence Mix ----------\n"
    for ((seqtype, percent) <- sortedVecmix)
    {
      s+= "---------- " + seqtype + ": " + percent + "% ----------\n"
    }
    s += "--------------------------------------------------------------------------\n"
    s += "---------- Generated Sequence Mix ----------\n"
    s += "---------- nseqs = " + nseqs + " -------------\n"
    val sortedSeqs = seqstats.toSeq.sortWith(seq_lt)
    for ((seq, seqcnt) <- sortedSeqs)
    {
      s += "---------- " + seq + ": " + seqcnt + " :: %3.3f".format((seqcnt.toDouble/nseqs)*100)
      s += "% ----------\n"
    }
    s += "--------------------------------------------------------------------------\n"
    s += "---------- Generated Hwacha Vector Sequence Mix ----------\n"
    s += "---------- nvseqs = " + vnseq*vfnum*seqstats("vec") + " -------------\n"
    val sortedVSeqs = vseqstats.toSeq.sortWith(seq_lt)
    for ((vseq, vseqcnt) <- sortedVSeqs)
    {
      s += "---------- " + vseq + ": " + vseqcnt
      s += " :: %3.3f".format((vseqcnt.toDouble/(vnseq*vfnum*seqstats("vec")))*100)
      s += "% ----------\n"
    }
    s
  }
  var a		= new ArrayBuffer[String]
  def instruction_stats(): String = 
  {
    def cat_lt(cat1: (String, Int), cat2: (String, Int)): Boolean =
    {
      val cathash = HashMap("alu"->1,"cmp"->2,"branch"->3,"jmp"->4,"jalr"->5,
        "la"->6,"mem"->7,"amo"->8,"misc"->9,"fpalu"->10,"fpcmp"->11,"fpfma"->12,
        "fpmem"->13,"fpcvt"->14,"fpmisc"->15,"Hwacha_vmem"->16,"Hwacha_vamo"->17,"Hwacha_valu"->18,"Hwacha_vfpalu"->19,
        "Hwacha_vfpfma"->20,"Hwacha_vfpcvt"->21,"Hwacha_vsmem"->22,"Hwacha_vshared"->23,"Hwacha_vpred"->24,"Hwacha_vcmp"->25,"Hwacha_vmisc"->26,"rvv_vmem_unit"->27,"rvv_vmem_const"->28,"rvv_vmem_vect"->29,"rvv_vinteger"->30,"rvv_vfixed"->31,"rvv_vfloat"->32,"rvv_vreduce"->33,"rvv_vmask"->34,"rvv_vpermute"->35, "rvv_vamo"->36, "rvv_vmem_zvlsseg"->37,"rvv_vconfig"->38, "unknown"->39) // Added RISC-V Vector functionality
      return cathash(cat1._1) < cathash(cat2._1)
    }
  
	  var  s = "------------------------ ISA Coverage ------------------------------------\n"
	  s += "--------------------------------------------------------------------------\n"
	  val scalar_integer_inst = opstats.toSeq(1)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(13)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(18)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(20)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(22)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(24)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(37)._2.toSeq.toSeq.toSeq.size
    s += "---------- Total Scalar Integer Instructions =  " + 80 + " --\n"
	  s += "---------- Generated Scalar Integer Instructions =  " + scalar_integer_inst + " --\n"
	  s += "---------- Scalar Integer Instructions Coverage =  " + (scalar_integer_inst.toFloat/80.toFloat)*100 + "% --\n"
	  

	  val scalar_float_inst = opstats.toSeq(0)._2.toSeq.toSeq.toSeq.size + opstats.toSeq(2)._2.toSeq.toSeq.toSeq.size +opstats.toSeq(5)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(14)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(17)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(19)._2.toSeq.toSeq.toSeq.size
	  s += "--------------------------------------------------------------------------\n"
	  s += "---------- Total Scalar Floating Instructions =  " + 64 + " --\n"
	  s += "---------- Generated Scalar Floating Instructions =  " + scalar_float_inst + " --\n"
	  s += "---------- Scalar Floating Instructions Coverage =  " + (scalar_float_inst.toFloat/64.toFloat)*100 + "% --\n"	
         
	  val hwacha_inst = opstats.toSeq(4)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(23)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(7)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(8)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(10)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(15)._2.toSeq.toSeq.toSeq.size   +opstats.toSeq(16)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(21)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(30)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(33)._2.toSeq.toSeq.toSeq.size+opstats.toSeq(36)._2.toSeq.toSeq.toSeq.size
	  s += "--------------------------------------------------------------------------\n"
	  s += "---------- Total Hwacha Instructions =  " + 207 + " --\n"
	  s += "---------- Generated Hwacha Instructions =  " + hwacha_inst + " --\n"
	  s += "---------- Hwacha Instructions Coverage =  " + (hwacha_inst.toFloat/207.toFloat)*100 + "% --\n"
	  
	  val rvv_config_inst     = opstats.toSeq(28)._2.toSeq.toSeq.toSeq.size      
	  val rvv_mem_unit_inst     = opstats.toSeq(26)._2.toSeq.toSeq.toSeq.size 
	  val rvv_mem_const_inst    = opstats.toSeq(6)._2.toSeq.toSeq.toSeq.size
	  val rvv_mem_vect_inst     = opstats.toSeq(12)._2.toSeq.toSeq.toSeq.size
	  val rvv_mem_zvlsseg_inst  = opstats.toSeq(9)._2.toSeq.toSeq.toSeq.size
	  val rvv_integer_inst      = opstats.toSeq(3)._2.toSeq.toSeq.toSeq.size
	  val rvv_fixed_inst        = opstats.toSeq(25)._2.toSeq.toSeq.toSeq.size
	  val rvv_float_inst        = opstats.toSeq(11)._2.toSeq.toSeq.toSeq.size
	  val rvv_reduce_inst       = opstats.toSeq(29)._2.toSeq.toSeq.toSeq.size    
	  val rvv_mask_inst         = opstats.toSeq(31)._2.toSeq.toSeq.toSeq.size
	  val rvv_permute_inst      = opstats.toSeq(32)._2.toSeq.toSeq.toSeq.size
	  val rvv_vamo_inst         = opstats.toSeq(27)._2.toSeq.toSeq.toSeq.size
         
	  val rvv_inst = rvv_mem_unit_inst+rvv_mem_const_inst+rvv_mem_vect_inst+rvv_integer_inst+rvv_fixed_inst+rvv_float_inst+rvv_reduce_inst+rvv_mask_inst+rvv_permute_inst + rvv_vamo_inst + rvv_mem_zvlsseg_inst+rvv_config_inst

		s += "--------------------------------------------------------------------------\n"
	  s += "---------- Total RISC-V Vector Instructions =  " + 444 + " --\n"
	  s += "---------- Generated RISC-V Vector Instructions =  " + rvv_inst + " --\n"
	  s += "---------- RISC-V Vector Instructions Coverage =  " + (rvv_inst.toFloat/444.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Configuration Instructions =  " + 2 + " --\n"
	  s += "-------------- Generated RVV Configuration Instructions =  " + rvv_config_inst + " --\n"
	  s += "-------------- RVV Configuration Instructions Coverage =  " + (rvv_config_inst.toFloat/2.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Mem Unit Stride Instructions =  " + 26 + " --\n"
	  s += "-------------- Generated RVV Mem Unit Stride Instructions =  " + rvv_mem_unit_inst + " --\n"
	  s += "-------------- RVV Mem Unit Stride Instructions Coverage =  " + (rvv_mem_unit_inst.toFloat/26.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Mem Constant Stride Instructions =  " + 16 + " --\n"
	  s += "-------------- Generated RVV Mem Constant Stride Instructions =  " + rvv_mem_const_inst + " --\n"
	  s += "-------------- RVV Mem Constant Stride Instructions Coverage =  " + (rvv_mem_const_inst.toFloat/16.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Mem Vector Stride Instructions =  " + 16 + " --\n"
	  s += "-------------- Generated RVV Mem Vector Stride Instructions =  " + rvv_mem_vect_inst + " --\n"
	  s += "-------------- RVV Mem Vector Stride Instructions Coverage =  " + (rvv_mem_vect_inst.toFloat/16.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
    s += "-------------- Total RVV Mem Zvlsseg Instructions =  " + 48 + " --\n"
	  s += "-------------- Generated RVV Mem Zvlsseg Instructions =  " + rvv_mem_zvlsseg_inst + " --\n"
	  s += "-------------- RVV Mem Zvlsseg Instructions Coverage =  " + (rvv_mem_zvlsseg_inst.toFloat/48.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
    s += "-------------- Total RVV Zvamo Instructions =  " + 27 + " --\n"
	  s += "-------------- Generated RVV Zvamo Instructions =  " + rvv_vamo_inst + " --\n"
	  s += "-------------- RVV Zvamo Instructions Coverage =  " + (rvv_vamo_inst.toFloat/27.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Integer Instructions =  " + 139 + " --\n"
	  s += "-------------- Generated RVV Integer Instructions =  " + rvv_integer_inst + " --\n"
	  s += "-------------- RVV Integer Instructions Coverage =  " + (rvv_integer_inst.toFloat/139.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Fixed Instructions =  " + 32 + " --\n"
	  s += "-------------- Generated RVV Fixed Instructions =  " + rvv_fixed_inst + " --\n"
	  s += "-------------- RVV Fixed Instructions Coverage =  " + (rvv_fixed_inst.toFloat/32.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Float Instructions =  " + 89 + " --\n"
	  s += "-------------- Generated RVV Float Instructions =  " + rvv_float_inst + " --\n"
	  s += "-------------- RVV Float Instructions Coverage =  " + (rvv_float_inst.toFloat/89.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Reduce Instructions =  " + 16 + " --\n"
	  s += "-------------- Generated RVV Reduce Instructions =  " + rvv_reduce_inst + " --\n"
	  s += "-------------- RVV Reduce Instructions Coverage =  " + (rvv_reduce_inst.toFloat/16.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Mask Instructions =  " + 15 + " --\n"
	  s += "-------------- Generated RVV Mask Instructions =  " + rvv_mask_inst + " --\n"
	  s += "-------------- RVV Mask Instructions Coverage =  " + (rvv_mask_inst.toFloat/15.toFloat)*100 + "% --\n"

		s += "--------------------------------------------------------------------------\n"
	  s += "-------------- Total RVV Permute Instructions =  " + 20 + " --\n"
	  s += "-------------- Generated RVV Permute Instructions =  " + rvv_permute_inst + " --\n"
	  s += "-------------- RVV Permute Instructions Coverage =  " + (rvv_permute_inst.toFloat/20.toFloat)*100 + "% --\n"

		s += "\n--------------------------------------------------------------------------\n"
    s += "------------------------ Opcode Usage ------------------------------------\n"
 		s += "--------------------------------------------------------------------------\n"
    s += "---------- instcnt = " + instcnt + " -------------\n"
    val sortedCats = catstats.toSeq.sortWith(cat_lt) // TODO: Better way to sort?

    for ((cat, catcnt) <- sortedCats)
    {
      val sortedOps = opstats(cat).toSeq.sortWith(_._1 < _._1)  //TODO: Better way to sort?
      s += "--------------------------------------------------------------------------\n"
      s += "---------- " + cat.toUpperCase() + " Opcodes: " + catcnt + " :: %3.3f".format((catcnt.toDouble/instcnt)*100)
      s +=  "% ----------\n"

      for ((op, opcnt) <- sortedOps)
      {
        a += op

        s += "-------------------- " + op + ": " + opcnt + " :: %3.3f".format((opcnt.toDouble/instcnt)*100)
        s += "% ----------\n"
      }
    }
    s
  }

  def get_time(): String =
  {
    val date = new Date()
    val datestr = DateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.FULL).format(date)
    "----- Test generated on " + datestr + " -----\n"
  }

  def gen_seq(): Unit =
  {
    val nxtseq = InstSeq(prob_tbl)
	  if (nxtseq.seqname == "rvv" && multi_config)
	  {
	    vconfig_counter -= 1
	    gen_config = false
	    if (vconfig_counter == 0) 
	    {
		    vconfig_counter = counter_memory + 1

		    gen_config = true
		    
		    val config = configure(vlen)
		     
		    lmul = config._1
		    sew = config._2
		    nr = config._3
		    nf = config._4

		    if (sew == vlen || lmul == "8") 
		    {
		    	wide = false
		    	narrow = false
		    }
		    else 
		    {
		    	wide = wide_user
		    	narrow = narrow_user
		    }

		    if (sew < 16 || sew > 64) 
		    {
		    	vfloat = false
		    }
		  }
	  }

    seqs += nxtseq
    seqstats(nxtseq.seqname) += 1
    if (nxtseq.seqname == "vec")
    {
      for ((seqname, seqcnt) <- nxtseq.asInstanceOf[SeqVec].vseqstats)
      {
        vseqstats(seqname) += seqcnt
      }
    }
  }

  def add_inst(inst: Inst) =
  {
    if (progsegs.length == 0)
      progsegs += ProgSeg()

    progsegs.last.insts += inst

    val branch_filter = (x: Operand) =>
      x.isInstanceOf[Label] && x.asInstanceOf[Label].label.indexOf("branch_patch") != -1
    val branch_patch = inst.operands.indexWhere(branch_filter)
    if (branch_patch != -1)
    {
      progsegs.last.insts += ILLEGAL(Label("0x%08x" format rand_word))
      progsegs += ProgSeg()
      inst.operands(branch_patch) = Label(progsegs.last.name)
    }

    val jalr_filter = (x: Operand) =>
      x.isInstanceOf[Label] && x.asInstanceOf[Label].label.indexOf("jalr_patch2") != -1
    val jalr_patch = inst.operands.indexWhere(jalr_filter)
    if (jalr_patch != -1)
    {
      progsegs.last.insts += ILLEGAL(Label("0x%08x" format rand_word))
      progsegs += ProgSeg()
      jalr_labels += Label(progsegs.last.name)
      inst.operands(jalr_patch) = Imm(0)
    }
    update_stats(inst)
  }

  def resolve_jalr_las =
  {
    var jalr_count = 0
    val jalr_la_filter = (x: Operand) =>
      x.isInstanceOf[Label] && x.asInstanceOf[Label].label.indexOf("jalr_patch1") != -1
    for (progseg <- progsegs)
    {
      for (inst <- progseg.insts)
      {
        val jalr_la_patch = inst.operands.indexWhere(jalr_la_filter)
        if (jalr_la_patch != -1)
        {
          inst.operands(jalr_la_patch) = jalr_labels(jalr_count)
          jalr_count += 1
        }
      }
    }
  }

  def names = List("xmem","xbranch","xalu","fgen","fpmem","fax","fdiv","vec","rvv")  // Added rvv for RISC-V Vector

//*************** Added variables for RISC-V Vector (v0.9) **********************
	var test_nseqs = 20
	var vconfig_counter = 5
	var counter_memory = vconfig_counter
	var gen_config = false
	var multi_config = false

	var vlen = 64
	var lmul_user = "1"
	var sew_user = 32
	var lmul = "1"
	var sew = 32
	var nr = 1
	var nf = 1
	
	var wide_user = false
	var narrow_user = false
	var wide = false
  var narrow = false
  var vfloat = false
//*****************************************************************************

  def code_body(seqnum: Int, mix: Map[String, Int], veccfg: Map[String, String], use_amo: Boolean, use_mul: Boolean, use_div: Boolean, segment: Boolean, rv_vmem_unit: Boolean,rv_vmem_const: Boolean,rv_vmem_vect: Boolean, rv_vmem_zvlsseg: Boolean,rv_vinteger: Boolean, rv_vfixed: Boolean, rv_vfloat: Boolean, rv_vreduce: Boolean, rv_vmask: Boolean, rv_vpermute: Boolean, rv_vamo: Boolean, rv_wide: Boolean, rv_narrow: Boolean, vlen_import: Int, lmul_import: String, sew_import: Int, nr_import: Int, nf_import: Int, mask:Boolean,  multi_config_import: Boolean) =
  {
  	multi_config = multi_config_import
  	vlen = vlen_import
  	wide_user = rv_wide
  	narrow_user = rv_narrow
	  wide = rv_wide
    narrow = rv_narrow
    vfloat = rv_vfloat
    if(multi_config)
      vfloat = false

    val name_to_seq = Map(
      "xmem" -> (() => new SeqMem(xregs, core_memory, use_amo)),
      "xbranch" -> (() => new SeqBranch(xregs)),
      "xalu" -> (() => new SeqALU(xregs, use_mul, use_div)), //true means use_divider, TODO: make better
      "fgen" -> (() => new SeqFPU(fregs_s, fregs_d)),
      "fpmem" -> (() => new SeqFPMem(xregs, fregs_s, fregs_d, core_memory)),
      "fax" -> (() => new SeqFaX(xregs, fregs_s, fregs_d)),
      "fdiv" -> (() => new SeqFDiv(fregs_s, fregs_d)),
      "vec" -> (() => new SeqVec(xregs, vxregs, vpregs, vsregs, varegs, used_vl, veccfg)),
      "rvv" -> (() => new SeqRVV(rvvregs, xregs, fregs_s, fregs_d, core_memory, rv_vmem_unit, rv_vmem_const, rv_vmem_vect, rv_vmem_zvlsseg, rv_vinteger, rv_vfixed, vfloat, rv_vreduce, rv_vmask, rv_vpermute, rv_vamo, wide, narrow, lmul, sew, nr, nf, mask, gen_config, multi_config)))       // Added RISC-V Vector functionality

    prob_tbl = new ArrayBuffer[(Int, () => InstSeq)]
    nseqs = seqnum
    
	  assert(lmul=="1" ||lmul=="2" ||lmul=="4" ||lmul=="8" || lmul == "f8" || lmul == "f4" || lmul == "f2", "Unsupported LMUL" )
	  assert(sew==8 ||sew==16 ||sew==32 ||sew==64 || sew==128 || sew==256 || sew==512 || sew==1024 , "Unsupported SEW" )

    for ((name, prob) <- mix)
      prob_tbl += ((prob, name_to_seq(name)))

    for (i <- 0 to nseqs-1)
    	gen_seq()

    if (segment)
      progsegs += ProgSeg()

    while (!is_seqs_empty)
    {
      seqs_find_active()

      while (!is_seqs_active_empty)
      {
        val seq = seqs_active(0)
		    if(segment) 
		    {
		      val inst = seq.next_inst()
		      val branch_filter = (x: Operand) =>
		    	  x.isInstanceOf[Label] && x.asInstanceOf[Label].label.indexOf("branch_patch") != -1 
			    val branch_patch = inst.operands.indexWhere(branch_filter)

		      val jalr_filter1 = (x: Operand) =>
      	    x.isInstanceOf[Label] && x.asInstanceOf[Label].label.indexOf("jalr_patch1") != -1
		      val jalr_patch1 = inst.operands.indexWhere(jalr_filter1)
		      val jalr_filter2 = (x: Operand) =>
      	    x.isInstanceOf[Label] && x.asInstanceOf[Label].label.indexOf("jalr_patch2") != -1
		      val jalr_patch2 = inst.operands.indexWhere(jalr_filter2)
		      if (jalr_patch1 == -1 && branch_patch == -1 && jalr_patch2 == -1)
			    {
            if (seq.inst_len==2 && (inst.opcode == "la" || inst.opcode == "li"))
	          {
              progsegs.last.insts += inst
			        update_stats(inst)
			        val inst1 = seq.next_inst()
			        progsegs.last.insts += inst1
			        update_stats(inst1)
			      }
			      else if (seq.inst_len==3 && (inst.opcode == "la" || inst.opcode == "li"))
			      {
			        progsegs.last.insts += inst
			        update_stats(inst)
			        val inst1 = seq.next_inst()
			        progsegs.last.insts += inst1
			        update_stats(inst1)
			        val inst2 = seq.next_inst()
			        progsegs.last.insts += inst2
			        update_stats(inst2)
			      }
			      else
			      {
			        progsegs.last.insts += inst
		       	  update_stats(inst)
			      }
			    }
      	}
      	else
        {
          add_inst(seq.next_inst())
	      }

        if (seq.is_done)
        {
          seq.free_regs()
          seqs_active -= seq
          if (seq.isInstanceOf[SeqVec]) 
            for (vinst <- seq.asInstanceOf[SeqVec].vinsts)
              update_stats(vinst)
        }

        if (rand_range(0,99) < 10) 
          seqs_find_active()
      }
    }

    //Final p_seg
    progsegs.last.insts += J(Label("reg_dump"))

    if(!segment)
    {
      resolve_jalr_las 
    }
    rand_permute(progsegs)

    if (killed_seqs >= (nseqs*5))
    {
      println("Warning: Prog killed an excessive number of sequences. (#X=%d, #Fs=%d, #Fd=%d, #VX=%d, #VP=%d, #VS=%d, #VA=%d, #RVV=%d)" format (xregs.size, fregs_s.size, fregs_d.size, vxregs.size, vpregs.size, vsregs.size, varegs.size, rvvregs.size))
    }

    ("" /: progsegs)(_ + _) + "\n"
  }

  def header(nseqs: Int) =
  {
    "// random assembly code generated by RISC-V torture test generator\n" +
    "// nseqs = " + nseqs + "\n" +
    "// memsize = " + memsize + "\n" +
    "\n" +
    "#include \"riscv_test.h\"\n"
  }

  def code_header(using_fpu: Boolean, using_vec: Boolean, using_rvv: Boolean, fprnd: Int, lmul:String, sew:Int) =
  {    	
    assert(!(using_rvv && using_vec), "RISC-V Vector and Hwacha Vector instructions can not be used simultaneously")
    "\n" +
    (if (using_vec) "RVTEST_RV64UV\n"
     else if (using_rvv) "RVTEST_RV64UV\n"
     else if (using_fpu) "RVTEST_RV64UF\n"
     else "RVTEST_RV64U\n") +
    "RVTEST_CODE_BEGIN\n" +
    "\n" +
    (if (using_vec) init_vector() else "") + 
    (if (using_rvv) init_rvv_vector(lmul_user,sew_user) else "") + // Added RISC-V Vector functionality
    "\n" +
    "\tj test_start\n" +
    "\n" +
    "crash_backward:\n" +
    "\tRVTEST_FAIL\n" +
    "\n" +
    "test_start:\n" +
    "\n" +
    // fregs must be initialized before xregs!
    (if (using_fpu) fregs.init_regs() else "") +
    (if (using_vec) vregs.init_regs() else "") +
    (if (using_rvv) rvvregs.init_regs(lmul) else "") +     // Added RISC-V Vector functionality
    xregs.init_regs() +
    "\tj pseg_0\n" +
    "\n"
  }

  def init_vector() = 
  {
    "\tli x1, " + used_vl + "\n" +
    "\tvsetcfg " + num_vxregs + ", " + num_vpregs + "\n" +
    "\tvsetvl x1,x1\n"
  }
  
  //************************************* Added RISC-V Vector functionality *********************
  def init_rvv_vector(lmul:String, sew: Int) =
  {
    "\tli a0, " + "2048\n" +
    "\tvsetvli t0, a0, e" + sew.toString + ", m" + lmul + ", ta, ma\n"
  }

  def code_footer(using_fpu: Boolean, using_vec: Boolean, using_rvv: Boolean, loop: Boolean,lmul:String, sew:Int) =
  {
    var s = "reg_dump:\n" +
    {
      if(loop)
      {
        "\tla x1, loop_count\n" +
        "\tlw x2, 0(x1)\n" +
        "\taddi x3, x2, -1\n" +
        "\tsw x3, 0(x1)\n" +
        "\tbeqz x2, loop_finish\n" +
        (if (using_rvv && multi_config) init_rvv_vector(lmul_user,sew_user) else "") +
        "\tj pseg_0\n" +
        "\n" +
        "loop_finish:\n"
      }
      else {""}
    } +
    // fregs must be saved after xregs
    xregs.save_regs() +
    (if(using_fpu) fregs.save_regs() else "") +
    (if(using_vec) vregs.save_regs() else "") +
    (if(using_rvv) rvvregs.save_regs(lmul,sew) else "") +    // Added RISC-V Vector functionality
    "\tj test_end\n" +
    "\n" +
    "crash_forward:\n" +
    "\tRVTEST_FAIL\n" +
    "\n" +
    "test_end:\n" +
    "\tRVTEST_PASS\n" +
    "\n" +
    "RVTEST_CODE_END\n" +
    "\n"
    for(seq <- seqs.filter(_.is_done))
    {
      val ns = seq.extra_code.mkString("\n")
      if(ns.nonEmpty) s += "// extra code for " + seq + "\n" +
      "\t.align 3\n" + ns + "\n"
    }
    s += "\n"
    s
  }

  def data_header() =
  {
    "\t.data\n" +
    "\n"
  }

  def output_mem_data(loop_size: Int) =
  {
    var s = "// Memory Blocks\n"
    s += MemDump(core_memory)
    s += "\n"
    s += ".align 8\n"
    s += "loop_count: .word 0x" + Integer.toHexString(loop_size) + "\n\n"
    for(seq <- seqs.filter(_.is_done))
    {
      val ns = seq.extra_visible_data.mkString("\n")
      if(ns.nonEmpty) s += "// output data for " + seq + "\n" + ns + "\n"
    }
    s
  }

  def data_input(using_fpu: Boolean, using_vec: Boolean, using_rvv: Boolean) =   // Added RISC-V Vector functionality
  {
    var s = "hidden_data:\n"
    for(seq <- seqs.filter(_.is_done))
    {
      val ns = seq.extra_hidden_data.mkString("\n")
      if(ns.nonEmpty) s += "// hidden data for " + seq + "\n" + ns + "\n"
    }
    s += xregs.init_regs_data()
    s += (if(using_fpu) fregs.init_regs_data() else "")
    s += (if(using_vec) vregs.init_regs_data() else "")
    s += (if(using_rvv) rvvregs.init_regs_data() else "")    // Added RISC-V Vector functionality
    s
  }

  def data_output(using_fpu: Boolean, using_vec: Boolean, using_rvv: Boolean, loop_size: Int) =    // Added RISC-V Vector functionality
  {
    "RVTEST_DATA_BEGIN\n" +
    "\n" +
    xregs.output_regs_data() +
    (if(using_fpu) fregs.output_regs_data() else "") +
    (if(using_vec) vregs.output_regs_data() else "") +
    (if(using_rvv) rvvregs.output_regs_data() else "") +
    output_mem_data(loop_size) +
    "RVTEST_DATA_END\n"
  }

  def data_footer() = ""

  def generate(nseqs: Int, fprnd: Int, mix: Map[String, Int], veccfg: Map[String, String], use_amo: Boolean, use_mul: Boolean, use_div: Boolean, segment : Boolean, loop: Boolean, loop_size: Int, rv_vmem_unit: Boolean,rv_vmem_const: Boolean,rv_vmem_vect: Boolean, rv_vmem_zvlsseg: Boolean, rv_vinteger: Boolean, rv_vfixed: Boolean, rv_vfloat: Boolean, rv_vreduce: Boolean, rv_vmask: Boolean, rv_vpermute: Boolean, rv_vamo: Boolean, rv_wide: Boolean, rv_narrow: Boolean, vlen: Int, lmul_import: String, sew_import: Int, nr_import: Int, nf_import: Int, mask :Boolean, multi_config: Boolean) =
  {
    // Check if generating any FP operations or Vec unit stuff
    val using_vec = mix.filterKeys(List("vec") contains _).values.reduce(_+_) > 0
    val using_rvv = mix.filterKeys(List("rvv") contains _).values.reduce(_+_) > 0 // Added RISC-V Vector compatibility
    val using_fpu = (mix.filterKeys(List("fgen","fpmem","fax","fdiv") contains _).values.reduce(_+_) > 0) || using_rvv || using_vec

    // TODO: make a config object that is passed around?

  	test_nseqs = nseqs
	  vconfig_counter = rand_range(0, test_nseqs/2)
	  counter_memory = vconfig_counter
	  lmul_user = lmul_import
	  sew_user = sew_import
	  lmul = lmul_import
	  sew = sew_import
	  nr = nr_import
	  nf = nf_import

    header(nseqs) +
    code_header(using_fpu, using_vec, using_rvv, fprnd, lmul, sew) +
    code_body(nseqs, mix, veccfg, use_amo, use_mul, use_div, segment, rv_vmem_unit, rv_vmem_const, rv_vmem_vect, rv_vmem_zvlsseg, rv_vinteger, rv_vfixed, rv_vfloat, rv_vreduce, rv_vmask, rv_vpermute, rv_vamo, rv_wide, rv_narrow, vlen, lmul, sew, nr, nf, mask, multi_config) +
    code_footer(using_fpu, using_vec, using_rvv, loop,lmul, sew) +
    data_header() +
    data_input(using_fpu, using_vec, using_rvv) +
    data_output(using_fpu, using_vec, using_rvv,loop_size) +
    data_footer()
  }

  def statistics(nseqs: Int, fprnd: Int, mix: Map[String, Int], vnseq: Int, vmemsize: Int, vfnum: Int, vecmix: Map[String, Int], use_amo: Boolean, use_mul: Boolean, use_div: Boolean, use_vec: Boolean) =
  {
    "--------------------------------------------------------------------------\n" + 
    "-- Statistics for assembly code created by RISCV torture test generator --\n" +
    get_time() +
    "--------------------------------------------------------------------------\n" +
    "---------- instcnt = " + instcnt + " -------------\n" +
    "---------- nseqs = " + nseqs + " -------------\n" +
    "---------- memsize = " + memsize + " ----------\n" +
    "---------- vnseq = " + vnseq + " ----------\n" +
    "---------- vfnum = " + vfnum + " ----------\n" +
    "---------- vmemsize = " + vmemsize + " ----------\n" +
    "---------- fprnd = " + fprnd + " ----------\n" +
    "---------- use_amo = " + use_amo + " ----------\n" +
    "---------- use_mul = " + use_mul + " ----------\n" +
    "---------- use_div = " + use_div + " ----------\n" + 
    "--------------------------------------------------------------------------\n\n" +
    "--------------------------------------------------------------------------\n" +
    sequence_stats(mix, vecmix, nseqs, vnseq, vfnum) +
    "--------------------------------------------------------------------------\n\n" +
    "--------------------------------------------------------------------------\n" +
    instruction_stats() +
    "--------------------------------------------------------------------------\n\n" +
    "--------------------------------------------------------------------------\n" +
    register_stats(use_vec) +
    "--------------------------------------------------------------------------\n" +
    "--------------------------------------------------------------------------\n"
  }
}
