package torture

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import Rand._

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

class Prog
{
  val xregs = new XRegsPool()
  val fregs = new FRegsMaster()
  val (fregs_s, fregs_d) = fregs.extract_pools()
  
  val seqs = new ArrayBuffer[InstSeq]
  val seqs_active = new ArrayBuffer[InstSeq]
  val progsegs = new ArrayBuffer[ProgSeg]

  def seqs_not_allocated = seqs.filter((x) => !x.allocated)
  def is_seqs_empty = seqs_not_allocated.length == 0
  def is_seqs_active_empty = seqs_active.length == 0

  def seqs_find_active(): Unit =
  {
    for (seq <- seqs_not_allocated)
    {
      xregs.backup()
      fregs.backup()

      if (seq.allocate_regs())
      {
        seqs_active += seq
      }
      else
      {
        xregs.restore()
        fregs.restore()

        return
      }
    }
  }

  var jalr_labels = new ArrayBuffer[Label]

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
        if (jalr_la_patch != -1) {
          inst.operands(jalr_la_patch) = jalr_labels(jalr_count)
          jalr_count += 1
        }
      }
    }
  }

  var far_branches = 0

  def resolve_far_branches =
  {
    var resolved = false

    val labels = new HashMap[String, Int]
    var line = 0

    labels += ("crash_backward" -> line)
    line += 35

    for (progseg <- progsegs)
    {
      labels += (progseg.name -> line)
      line += progseg.insts.length
    }

    labels += ("reg_dump" -> line)
    line += 35
    labels += ("crash_forward" -> line)

    val progsegs_insert = new ArrayBuffer[(ProgSeg, ProgSeg)]

    for (progseg <- progsegs)
    {
      val branches = new ArrayBuffer[Inst]
      line = labels(progseg.name)

      for (inst <- progseg.insts)
      {
        if (inst.is_la) line += 1
        if (inst.is_branch)
        {
          //println("%d: %d %s" format (line, math.abs(line - labels(inst.operands(2).asInstanceOf[Label].label)), inst))
          if (math.abs(line - labels(inst.operands(2).asInstanceOf[Label].label)) > 800)
            branches += inst
        }
        line += 1
      }

      // only split the first far branch in a progseg
      if (branches.length > 0)
      {
        val label = branches(0).operands(2)
        branches(0).operands(2) = Label("far_branch_" + far_branches)
        val idx_split = progseg.insts.indexOf(branches(0)) + 1
        val (insts, insts_split) = progseg.insts.splitAt(idx_split)
        insts += J(Label("far_branch_" + (far_branches+1)))
        progseg.insts = insts

        val progseg_fb0 = new ProgSeg("far_branch_" + far_branches)
        progseg_fb0.insts += J(label)
        progsegs_insert += ((progseg, progseg_fb0))
        val progseg_fb1 = new ProgSeg("far_branch_" + (far_branches+1))
        progseg_fb1.insts = insts_split
        progsegs_insert += ((progseg_fb0, progseg_fb1))

        far_branches += 2
        resolved = true
      }
    }

    for ((progseg, progseg_insert) <- progsegs_insert)
    {
      val idx_insert = progsegs.indexOf(progseg) + 1
      progsegs.insert(idx_insert, progseg_insert)
    }

    resolved
  }

  def names = List("xmem","xbranch","xalu","fgen","fax")

  def code_body(nseqs: Int, memsize: Int, mix: Map[String, Int]) =
  {
    val name_to_seq = Map(
      "xmem" -> (() => new SeqMem(xregs, memsize)),
      "xbranch" -> (() => new SeqBranch(xregs)),
      "xalu" -> (() => new SeqALU(xregs)),
      "fgen" -> (() => new SeqFPU(fregs_s, fregs_d)),
      "fax" -> (() => new SeqFaX(xregs, fregs_s, fregs_d)))

    val prob_tbl = new ArrayBuffer[(Int, () => InstSeq)]

    for ((name, prob) <- mix)
      prob_tbl += ((prob, name_to_seq(name)))

    for (i <- 0 to nseqs-1)
      seqs += InstSeq(memsize, prob_tbl)

    while (!is_seqs_empty)
    {
      seqs_find_active()

      while (!is_seqs_active_empty)
      {
        val seq = rand_pick(seqs_active)
        add_inst(seq.next_inst())

        if (seq.is_done)
        {
          seq.free_regs()
          seqs_active -= seq
        }
      }
    }

    progsegs.last.insts += J(Label("reg_dump"))

    resolve_jalr_las
    rand_permute(progsegs)

    while (resolve_far_branches) {}

    ("" /: progsegs)(_ + _) + "\n"
  }

  def header(nseqs: Int, memsize: Int) =
  {
    "// random assembly code generated by RISC-V torture test generator\n" +
    "// nseqs = " + nseqs + "\n" +
    "// memsize = " + memsize + "\n" +
    "\n" +
    "#include \"test_riscv.h\"\n" +
    "\n" +
    "\tTEST_RISCV\n"
  }

  def code_header(using_fpu: Boolean) =
  {
    "\n" +
    "\tTEST_CODEBEGIN\n" +
    (if(using_fpu) "\tTEST_FP_ENABLE\n" else "") +
    "\n" +
    "\tj test_start\n" +
    "\n" +
    "crash_backward:\n" +
    "\tTEST_FAIL\n" +
    "\n" +
    "test_start:\n" +
    "\n" +
    // fregs must be initialized before xregs!
    (if(using_fpu) fregs.init_regs() else "")  +
    xregs.init_regs() +
    "\tj pseg_0\n" +
    "\n"
  }

  def code_footer(using_fpu: Boolean) =
  {
    "reg_dump:\n" +
    // fregs must be saved after xregs
    xregs.save_regs() +
    (if(using_fpu) fregs.save_regs() else "") +
    "\tj test_end\n" +
    "\n" +
    "crash_forward:\n" +
    "\tTEST_FAIL\n" +
    "\n" +
    "test_end:\n" +
    "\tTEST_PASS\n" +
    "\n" +
    "\tTEST_CODEEND\n" +
    "\n"
  }

  def data_header() =
  {
    "\t.data\n" +
    "\t.align 8\n" +
    "\n"
  }

  def output_mem_data(memsize: Int) =
  {
    var s = "\t.align 8\n"
    s += "test_memory:\n"
    for (i <- 0 to memsize/8/2)
      s += "\t.dword 0x%016x, 0x%016x\n" format (rand_dword, rand_dword)
    s += "\n"
    s
  }

  def data_input(using_fpu: Boolean) =
  {
    xregs.init_regs_data() +
    (if(using_fpu) fregs.init_regs_data() else "")
  }

  def data_output(using_fpu: Boolean, memsize: Int) =
  {
    "\tTEST_DATABEGIN\n" +
    "\n" +
    xregs.output_regs_data() +
    (if(using_fpu) fregs.output_regs_data() else "") +
    output_mem_data(memsize) +
    "\tTEST_DATAEND\n"
  }

  def data_footer() = ""

  def generate(nseqs: Int, memsize: Int, mix: Map[String, Int]) =
  {
    // Check if generating any FP operations
    val using_fpu = mix.filterKeys(List("fgen","fax") contains _).values.reduce(_+_) > 0

    header(nseqs, memsize) +
    code_header(using_fpu) +
    code_body(nseqs, memsize, mix) +
    code_footer(using_fpu) +
    data_header() +
    data_input(using_fpu) +
    data_output(using_fpu, memsize) +
    data_footer()
  }
}
