package torture

class Inst(opcode: String, val operands: Array[Operand])
{
  def optype(): String = {
    if (is_alu) return "alu"
    if (is_cmp) return "cmp"
    if (is_branch) return "branch"
    if (is_jalr) return "jalr"
    if (is_jmp) return "jmp"
    if (is_la) return "la"
    if (is_mem) return "mem"
    if (is_amo) return "amo"
    if (is_misc) return "misc"
    if (is_fpalu) return "fpalu"
    if (is_fpcmp) return "fpcmp"
    if (is_fpfma) return "fpfma"
    if (is_fpmem) return "fpmem"
    if (is_fpcvt) return "fpcvt"
    if (is_fpmisc) return "fpmisc"
    if (is_vshared) return "vshared"
    if (is_valu) return "valu"
    if (is_vfpalu) return "vfpalu"
    if (is_vfpfma) return "vfpfma"
    if (is_vfpcvt) return "vfpcvt"
    if (is_vsmem) return "vsmem"
    if (is_vshared) return "vshared"
    if (is_vcmp) return "vcmp"
    if (is_vpred) return "vpred"
    if (is_vmem) return "vmem"
    if (is_vamo) return "vamo"
    if (is_vmisc) return "vmisc"
    // ***** RISC-V Vector Ext (v0.9) --- Optypes ******************************************
    if (is_rvv_vconfig) return "rvv_vconfig"
    if (is_rvv_vmem_unit) return "rvv_vmem_unit"
    if (is_rvv_vmem_const) return "rvv_vmem_const"
    if (is_rvv_vmem_vect) return "rvv_vmem_vect"
    if (is_rvv_vmem_zvlsseg) return "rvv_vmem_zvlsseg"
    if (is_rvv_vamo) return "rvv_vamo"
    if (is_rvv_vinteger) return "rvv_vinteger"
    if (is_rvv_vfixed) return "rvv_vfixed"
    if (is_rvv_vfloat) return "rvv_vfloat"
    if (is_rvv_vreduce) return "rvv_vreduce"
    if (is_rvv_vmask) return "rvv_vmask"
    if (is_rvv_vpermute) return "rvv_vpermute"
    // ************************************************************************************
    return "unknown" //Shouldn't return this.
  
  }
  def opcode(): String = { return opcode }

  def is_branch = List("beq", "bne", "blt", "bge", "bltu", "bgeu").contains(opcode)

  def is_jalr = List("jalr").contains(opcode)

  def is_jmp = List("jal").contains(opcode)

  def is_la = opcode == "la"

  def is_mem = List("lb", "lh", "lw", "ld", "lbu", "lhu", "lwu", "sb", "sh", "sw", "sd").contains(opcode)

  def is_amo = List("amoadd.w", "amoswap.w", "amoand.w", "amoor.w", "amomin.w", "amominu.w",
    "amomax.w", "amomaxu.w", "amoxor.w", "amoadd.d", "amoswap.d", "amoand.d", "amoor.d",
    "amomin.d", "amominu.d", "amomax.d", "amomaxu.d", "amoxor.d").contains(opcode)

  def is_cmp = List("slti", "sltiu", "slt", "sltu").contains(opcode)

  def is_alu = List("addi", "slli", "xori", "srli", "srai", "ori", "andi",
    "add", "sub", "sll", "xor", "srl", "sra", "or", "and", "mul", "mulh",
    "mulhsu", "mulhu", "div", "divu", "rem", "remu", "lui", "addiw", "slliw", "srliw",
    "sraiw", "addw", "subw", "sllw", "srlw", "sraw", "mulw", "divw", "divuw", "remw",
    "remuw").contains(opcode)

  def is_fpmem = List("flw", "fld", "fsw", "fsd").contains(opcode)

  def is_fpalu = List("fadd.s", "fsub.s", "fmul.s", "fdiv.s", "fsqrt.s", "fmin.s", "fmax.s",
    "fadd.d", "fsub.d", "fmul.d", "fdiv.d", "fsqrt.d", "fmin.d", "fmax.d",
    "fsgnj.s", "fsgnjn.s", "fsgnjx.s", "fsgnj.d", "fsgnjn.d", "fsgnjx.d").contains(opcode)

  def is_fpfma = List("fmadd.s", "fmsub.s", "fnmsub.s", "fnmadd.s",
    "fmadd.d", "fmsub.d", "fnmsub.d", "fnmadd.d").contains(opcode)

  def is_fpcvt = List("fcvt.s.d", "fcvt.d.s", "fcvt.s.l", "fcvt.s.lu", "fcvt.s.w",
    "fcvt.s.wu", "fcvt.d.l", "fcvt.d.lu", "fcvt.d.w", "fcvt.d.wu", "fcvt.l.s",
    "fcvt.lu.s", "fcvt.w.s", "fcvt.wu.s", "fcvt.l.d", "fcvt.lu.d",
    "fcvt.w.d", "fcvt.wu.d").contains(opcode)

  def is_fpmisc = List("fmovz", "fmovn", "frsr", "fssr", "fmv.s.x", "fmv.x.s",
    "fmv.d.x", "fmv.x.d").contains(opcode)

  def is_fpcmp = List("feq.s", "flt.s", "fle.s", "feq.d", "flt.d", "fle.d").contains(opcode)

  def is_misc = List("syscall", "break", "rdcycle", "rdtime", "rdinstret",
    "nop", "li", "mfpcr", "mtpcr", "auipc", "movz", "movn", "fence.i", "fence").contains(opcode)

  def is_vshared = List("vaddi", "vslli", "vxori", "vsrli", "vsrai", "vori", "vandi", "vlui",
    "vaddiw", "vslliw", "vsrliw", "vsraiw").contains(opcode)

  def is_valu = List("vadd", "vsub", "vsll", "vxor", "vsrl", "vsra", "vor", "vand", "vmul", "vmulh",
    "vmulhsu", "vmulhu", "vdiv", "vdivu", "vrem", "vremu", "vaddw", "vsubw", "vsllw",
    "vsrlw", "vsraw", "vmulw", "vdivw", "vdivuw", "vremw", "vremuw").contains(opcode)

  def is_vpred = List("vpop", "vpset", "vpclear").contains(opcode)

  def is_vcmp = List("vcmpeq", "vcmplt", "vcmpltu", "vcmpfeq", "vcmpflt", "vcmfle").contains(opcode)

  def is_vfpalu = List("vfadd.s", "vfsub.s", "vfmul.s", "vfdiv.s", "vfsqrt.s", "vfmin.s", "vfmax.s",
    "vfadd.d", "vfsub.d", "vfmul.d", "vfdiv.d", "vfsqrt.d", "vfmin.d", "vfmax.d",
    "vfsgnj.s", "vfsgnjn.s", "vfsgnjx.s", "vfsgnj.d", "vfsgnjn.d", "vfsgnjx.d").contains(opcode)

  def is_vfpfma = List("vfmadd.s", "vfmsub.s", "vfnmsub.s", "vfnmadd.s",
    "vfmadd.d", "vfmsub.d", "vfnmsub.d", "vfnmadd.d").contains(opcode)

  def is_vfpcvt = List("vfcvt.s.d", "vfcvt.d.s", "vfcvt.s.l", "vfcvt.s.lu", "vfcvt.s.w",
    "vfcvt.s.wu", "vfcvt.d.l", "vfcvt.d.lu", "vfcvt.d.w", "vfcvt.d.wu", "vfcvt.l.s",
    "vfcvt.lu.s", "vfcvt.w.s", "vfcvt.wu.s", "vfcvt.l.d", "vfcvt.lu.d",
    "vfcvt.w.d", "vfcvt.wu.d").contains(opcode)

  def is_vsmem = List("vlsb", "vlsh", "vlsw", "vlsd", "vlsbu", "vlshu", "vlswu", "vssb", "vssh", "vssw", "vssd",
    "vlab", "vlah", "vlaw", "vlad", "vlabu", "vlahu", "vlawu", "vsab", "vsah", "vsaw", "vsad").contains(opcode)

  def is_vmem = List("vlb", "vlh", "vlw", "vld", "vlbu", "vlhu", "vlwu", "vsb", "vsh", "vsw", "vsd",
    "vlsegb", "vlsegh", "vlsegw", "vlsegd", "vlsegbu", "vlseghu", "vlsegwu", "vssegb", "vssegh", "vssegw", "vssegd",
    "vlstb", "vlsth", "vlstw", "vlstd", "vlstbu", "vlsthu", "vlstwu", "vsstb", "vssth", "vsstw", "vsstd",
    "vlsegstb", "vlsegsth", "vlsegstw", "vlsegstd", "vlsegstbu", "vlsegsthu", "vlsegstwu", "vssegstb", "vssegsth",
    "vssegstw", "vssegstd", "vlxb", "vlxh", "vlxw", "vlxd", "vlxbu", "vlxhu", "vlxwu", "vsxb", "vsxh", "vsxw", "vsxd",
    "vlsegxb", "vlsegxh", "vlsegxw", "vlsegxd", "vlsegxbu", "vlsegxhu", "vlsegxwu", "vssegxb", "vssegxh", "vssegxw",
    "vssegxd").contains(opcode)

  def is_vamo = List("vamoadd.w", "vamoswap.w", "vamoand.w", "vamoor.w", "vamomin.w", "vamominu.w",
    "vamomax.w", "vamomaxu.w", "vamoxor.w", "vamoadd.d", "vamoswap.d", "vamoand.d", "vamoor.d",
    "vamomin.d", "vamominu.d", "vamomax.d", "vamomaxu.d", "vamoxor.d").contains(opcode)

  def is_vmisc = List("vsetcfg", "vstop", "vsetvl", "veidx", "vf",
    "vmcs", "vmca", "fence").contains(opcode)

  // ***** RISC-V Vector Ext (v0.9) --- Opcode Lists ******************************************

  def is_rvv_vconfig =
    List("vsetvli", "vsetvl").contains(opcode)

  def is_rvv_vmem_unit =
    List("vle8.v", "vle16.v", "vle32.v", "vle64.v", "vle128.v", "vle256.v", "vle512.v", "vle1024.v",
         "vse8.v", "vse16.v", "vse32.v", "vse64.v", "vse128.v", "vse256.v", "vse512.v", "vse1024.v",
         "vl1r.v", "vs1r.v",
         "vle8ff.v", "vle16ff.v", "vle32ff.v", "vle64ff.v", "vle128ff.v", "vle256ff.v", "vle512ff.v", "vle1024ff.v"
         ).contains(opcode)

  def is_rvv_vmem_const =
    List("vlse8.v", "vlse16.v", "vlse32.v", "vlse64.v", "vlse128.v", "vlse256.v", "vlse512.v", "vlse1024.v",
         "vsse8.v", "vsse16.v", "vsse32.v", "vsse64.v", "vsse128.v", "vsse256.v", "vsse512.v", "vsse1024.v"
         ).contains(opcode)

  def is_rvv_vmem_vect =
    List("vlxei8.v", "vlxei16.v", "vlxei32.v", "vlxei64.v", "vlxei128.v", "vlxei256.v", "vlxei512.v", "vlxei1024.v",
         "vsxei8.v", "vsxei16.v", "vsxei32.v", "vsxei64.v", "vsxei128.v", "vsxei256.v", "vsxei512.v", "vsxei1024.v",
         "vsuxei8.v", "vsuxei16.v", "vsuxei32.v", "vsuxei64.v", "vsuxei128.v", "vsuxei256.v", "vsuxei512.v", "vsuxei1024.v"
         ).contains(opcode)

  def is_rvv_vmem_zvlsseg =
    List("vlseg2e8.v", "vlseg2e16.v", "vlseg2e32.v", "vlseg2e64.v", "vlseg2e128.v", "vlseg2e256.v", "vlseg2e512.v", "vlseg2e1024.v",
         "vsseg2e8.v", "vsseg2e16.v", "vsseg2e32.v", "vsseg2e64.v", "vsseg2e128.v", "vsseg2e256.v", "vsseg2e512.v", "vsseg2e1024.v",
         "vlseg3e8.v", "vlseg3e16.v", "vlseg3e32.v", "vlseg3e64.v", "vlseg3e128.v", "vlseg3e256.v", "vlseg3e512.v", "vlseg3e1024.v",
         "vsseg3e8.v", "vsseg3e16.v", "vsseg3e32.v", "vsseg3e64.v", "vsseg3e128.v", "vsseg3e256.v", "vsseg3e512.v", "vsseg3e1024.v",
         "vlseg4e8.v", "vlseg4e16.v", "vlseg4e32.v", "vlseg4e64.v", "vlseg4e128.v", "vlseg4e256.v", "vlseg4e512.v", "vlseg4e1024.v",
         "vsseg4e8.v", "vsseg4e16.v", "vsseg4e32.v", "vsseg4e64.v", "vsseg4e128.v", "vsseg4e256.v", "vsseg4e512.v", "vsseg4e1024.v",
         "vlseg5e8.v", "vlseg5e16.v", "vlseg5e32.v", "vlseg5e64.v", "vlseg5e128.v", "vlseg5e256.v", "vlseg5e512.v", "vlseg5e1024.v",
         "vsseg5e8.v", "vsseg5e16.v", "vsseg5e32.v", "vsseg5e64.v", "vsseg5e128.v", "vsseg5e256.v", "vsseg5e512.v", "vsseg5e1024.v",
         "vlseg6e8.v", "vlseg6e16.v", "vlseg6e32.v", "vlseg6e64.v", "vlseg6e128.v", "vlseg6e256.v", "vlseg6e512.v", "vlseg6e1024.v",
         "vsseg6e8.v", "vsseg6e16.v", "vsseg6e32.v", "vsseg6e64.v", "vsseg6e128.v", "vsseg6e256.v", "vsseg6e512.v", "vsseg6e1024.v",
         "vlseg7e8.v", "vlseg7e16.v", "vlseg7e32.v", "vlseg7e64.v", "vlseg7e128.v", "vlseg7e256.v", "vlseg7e512.v", "vlseg7e1024.v",
         "vsseg7e8.v", "vsseg7e16.v", "vsseg7e32.v", "vsseg7e64.v", "vsseg7e128.v", "vsseg7e256.v", "vsseg7e512.v", "vsseg7e1024.v",
         "vlseg8e8.v", "vlseg8e16.v", "vlseg8e32.v", "vlseg8e64.v", "vlseg8e128.v", "vlseg8e256.v", "vlseg8e512.v", "vlseg8e1024.v",
         "vsseg8e8.v", "vsseg8e16.v", "vsseg8e32.v", "vsseg8e64.v", "vsseg8e128.v", "vsseg8e256.v", "vsseg8e512.v", "vsseg8e1024.v",
         "vlsseg2e8.v", "vlsseg2e16.v", "vlsseg2e32.v", "vlsseg2e64.v", "vlsseg2e128.v", "vlsseg2e256.v", "vlsseg2e512.v", "vlsseg2e1024.v",
         "vssseg2e8.v", "vssseg2e16.v", "vssseg2e32.v", "vssseg2e64.v", "vssseg2e128.v", "vssseg2e256.v", "vssseg2e512.v", "vssseg2e1024.v",
         "vlsseg3e8.v", "vlsseg3e16.v", "vlsseg3e32.v", "vlsseg3e64.v", "vlsseg3e128.v", "vlsseg3e256.v", "vlsseg3e512.v", "vlsseg3e1024.v",
         "vssseg3e8.v", "vssseg3e16.v", "vssseg3e32.v", "vssseg3e64.v", "vssseg3e128.v", "vssseg3e256.v", "vssseg3e512.v", "vssseg3e1024.v",
         "vlsseg4e8.v", "vlsseg4e16.v", "vlsseg4e32.v", "vlsseg4e64.v", "vlsseg4e128.v", "vlsseg4e256.v", "vlsseg4e512.v", "vlsseg4e1024.v",
         "vssseg4e8.v", "vssseg4e16.v", "vssseg4e32.v", "vssseg4e64.v", "vssseg4e128.v", "vssseg4e256.v", "vssseg4e512.v", "vssseg4e1024.v",
         "vlsseg5e8.v", "vlsseg5e16.v", "vlsseg5e32.v", "vlsseg5e64.v", "vlsseg5e128.v", "vlsseg5e256.v", "vlsseg5e512.v", "vlsseg5e1024.v",
         "vssseg5e8.v", "vssseg5e16.v", "vssseg5e32.v", "vssseg5e64.v", "vssseg5e128.v", "vssseg5e256.v", "vssseg5e512.v", "vssseg5e1024.v",
         "vlsseg6e8.v", "vlsseg6e16.v", "vlsseg6e32.v", "vlsseg6e64.v", "vlsseg6e128.v", "vlsseg6e256.v", "vlsseg6e512.v", "vlsseg6e1024.v",
         "vssseg6e8.v", "vssseg6e16.v", "vssseg6e32.v", "vssseg6e64.v", "vssseg6e128.v", "vssseg6e256.v", "vssseg6e512.v", "vssseg6e1024.v",
         "vlsseg7e8.v", "vlsseg7e16.v", "vlsseg7e32.v", "vlsseg7e64.v", "vlsseg7e128.v", "vlsseg7e256.v", "vlsseg7e512.v", "vlsseg7e1024.v",
         "vssseg7e8.v", "vssseg7e16.v", "vssseg7e32.v", "vssseg7e64.v", "vssseg7e128.v", "vssseg7e256.v", "vssseg7e512.v", "vssseg7e1024.v",
         "vlsseg8e8.v", "vlsseg8e16.v", "vlsseg8e32.v", "vlsseg8e64.v", "vlsseg8e128.v", "vlsseg8e256.v", "vlsseg8e512.v", "vlsseg8e1024.v",
         "vssseg8e8.v", "vssseg8e16.v", "vssseg8e32.v", "vssseg8e64.v", "vssseg8e128.v", "vssseg8e256.v", "vssseg8e512.v", "vssseg8e1024.v",
         "vlxseg2ei8.v", "vlxseg2ei16.v", "vlxseg2ei32.v", "vlxseg2ei64.v", "vlxseg2ei128.v", "vlxseg2ei256.v", "vlxseg2ei512.v", "vlxseg2ei1024.v",
         "vsxseg2ei8.v", "vsxseg2ei16.v", "vsxseg2ei32.v", "vsxseg2ei64.v", "vsxseg2ei128.v", "vsxseg2ei256.v", "vsxseg2ei512.v", "vsxseg2ei 1024.v",
         "vlxseg3ei8.v", "vlxseg3ei16.v", "vlxseg3ei32.v", "vlxseg3ei64.v", "vlxseg3ei128.v", "vlxseg3ei256.v", "vlxseg3ei512.v", "vlxseg3ei1024.v",
         "vsxseg3ei8.v", "vsxseg3ei16.v", "vsxseg3ei32.v", "vsxseg3ei64.v", "vsxseg3ei128.v", "vsxseg3ei256.v", "vsxseg3ei512.v", "vsxseg3ei1024.v",
         "vlxseg4ei8.v", "vlxseg4ei16.v", "vlxseg4ei32.v", "vlxseg4ei64.v", "vlxseg4ei128.v", "vlxseg4ei256.v", "vlxseg4ei512.v", "vlxseg4ei1024.v",
         "vsxseg4ei8.v", "vsxseg4ei16.v", "vsxseg4ei32.v", "vsxseg4ei64.v", "vsxseg4ei128.v", "vsxseg4ei256.v", "vsxseg4ei512.v", "vsxseg4ei1024.v",
         "vlxseg5ei8.v", "vlxseg5ei16.v", "vlxseg5ei32.v", "vlxseg5ei64.v", "vlxseg5ei128.v", "vlxseg5ei256.v", "vlxseg5ei512.v", "vlxseg5ei1024.v",
         "vsxseg5ei8.v", "vsxseg5ei16.v", "vsxseg5ei32.v", "vsxseg5ei64.v", "vsxseg5ei128.v", "vsxseg5ei256.v", "vsxseg5ei512.v", "vsxseg5ei1024.v",
         "vlxseg6ei8.v", "vlxseg6ei16.v", "vlxseg6ei32.v", "vlxseg6ei64.v", "vlxseg6ei128.v", "vlxseg6ei256.v", "vlxseg6ei512.v", "vlxseg6ei1024.v",
         "vsxseg6ei8.v", "vsxseg6ei16.v", "vsxseg6ei32.v", "vsxseg6ei64.v", "vsxseg6ei128.v", "vsxseg6ei256.v", "vsxseg6ei512.v", "vsxseg6ei1024.v",
         "vlxseg7ei8.v", "vlxseg7ei16.v", "vlxseg7ei32.v", "vlxseg7ei64.v", "vlxseg7ei128.v", "vlxseg7ei256.v", "vlxseg7ei512.v", "vlxseg7ei1024.v",
         "vsxseg7ei8.v", "vsxseg7ei16.v", "vsxseg7ei32.v", "vsxseg7ei64.v", "vsxseg7ei128.v", "vsxseg7ei256.v", "vsxseg7ei512.v", "vsxseg7ei1024.v",
         "vlxseg8ei8.v", "vlxseg8ei16.v", "vlxseg8ei32.v", "vlxseg8ei64.v", "vlxseg8ei128.v", "vlxseg8ei256.v", "vlxseg8ei512.v", "vlxseg8ei1024.v",
         "vsxseg8ei8.v", "vsxseg8ei16.v", "vsxseg8ei32.v", "vsxseg8ei64.v", "vsxseg8ei128.v", "vsxseg8ei256.v", "vsxseg8ei512.v", "vsxseg8ei1024.v"
         ).contains(opcode)

  def is_rvv_vamo =
    List("vamoswapei8.v", "vamoaddei8.v", "vamoxorei8.v", "vamoandei8.v", "vamoorei8.v",
         "vamominei8.v", "vamomaxei8.v", "vamominuei8.v", "vamomaxuei8.v", "vamoswapei16.v", "vamoaddei16.v", "vamoxorei16.v", "vamoandei16.v", 
         "vamoorei16.v", "vamominei16.v", "vamomaxei16.v", "vamominuei16.v", "vamomaxuei16.v", "vamoswapei32.v", "vamoaddei32.v", "vamoxorei32.v", 
         "vamoandei32.v", "vamoorei32.v", "vamominei32.v", "vamomaxei32.v", "vamominuei32.v", "vamomaxuei32.v", "vamoswapei64.v", "vamoaddei64.v",
         "vamoxorei64.v", "vamoandei64.v", "vamoorei64.v", "vamominei64.v", "vamomaxei64.v", "vamominuei64.v", "vamomaxuei64.v",
         "vamoswapei128.v", "vamoaddei128.v", "vamoxorei128.v", "vamoandei128.v", "vamoorei128.v", "vamominei128.v", "vamomaxei128.v",
         "vamominuei128.v", "vamomaxuei128.v").contains(opcode)
  
  def is_rvv_vinteger =
    List("vadd.vv", "vadd.vx", "vadd.vi", "vsub.vv", "vsub.vx", "vrsub.vx", "vrsub.vi",
         "vwaddu.vv", "vwaddu.vx", "vwsubu.vv", "vwsubu.vx", "vwadd.vv", "vwadd.vx", "vwsub.vv", "vwsub.vx",
         "vwaddu.wv", "vwaddu.wx", "vwsubu.wv", "vwsubu.wx", "vwadd.wv", "vwadd.wx", "vwsub.wv", "vwsub.wx",
         "vzext.vf2", "vsext.vf2", "vzext.vf4", "vsext.vf4", "vzext.vf8", "vsext.vf8", "vadc.vvm", "vadc.vxm",
         "vadc.vim", "vmadc.vvm", "vmadc.vxm", "vmadc.vim", "vmadc.vv", "vmadc.vx", "vmadc.vi", "vsbc.vvm",
         "vsbc.vxm", "vmsbc.vvm", "vmsbc.vxm", "vmsbc.vv", "vmsbc.vx", "vand.vv", "vand.vx", "vand.vi",
         "vor.vv", "vor.vx", "vor.vi", "vxor.vv", "vxor.vx", "vxor.vi", "vsll.vv", "vsll.vx", "vsll.vi",
         "vsrl.vv", "vsrl.vx", "vsrl.vi", "vsra.vv", "vsra.vx", "vsra.vi", "vnsrl.wv", "vnsrl.wx", "vnsrl.wi",
         "vnsra.wv", "vnsra.wx", "vnsra.wi", "vmseq.vv", "vmseq.vx", "vmseq.vi", "vmsne.vv", "vmsne.vx",
         "vmsne.vi", "vmsltu.vv", "vmsltu.vx", "vmslt.vv", "vmslt.vx", "vmsleu.vv", "vmsleu.vx", "vmsleu.vi",
         "vmsle.vv", "vmsle.vx", "vmsle.vi", "vmsgtu.vx", "vmsgtu.vi", "vmsgt.vx", "vmsgt.vi", "vminu.vv",
         "vminu.vx", "vmin.vv", "vmin.vx", "vmaxu.vv", "vmaxu.vx", "vmax.vv", "vmax.vx", "vmul.vv", "vmul.vx",
  	     "vmulh.vv", "vmulh.vx", "vmulhu.vv", "vmulhu.vx", "vmulhsu.vv", "vmulhsu.vx", "vdivu.vv", "vdivu.vx",
	       "vdiv.vv", "vdiv.vx", "vremu.vv", "vremu.vx", "vrem.vv", "vrem.vx", "vwmul.vv", "vwmul.vx", "vwmulu.vv",
	       "vwmulu.vx", "vwmulsu.vv", "vwmulsu.vx", "vmacc.vv", "vmacc.vx", "vnmsac.vv", "vnmsac.vx", "vmadd.vv",
	       "vmadd.vx", "vnmsub.vv", "vnmsub.vx", "vwmaccu.vv", "vwmaccu.vx", "vwmacc.vv", "vwmacc.vx",
	       "vwmaccsu.vv", "vwmaccsu.vx", "vwmaccus.vx", "vmerge.vvm", "vmerge.vxm", "vmerge.vim", "vmv.v.v",
	       "vmv.v.x", "vmv.v.i").contains(opcode)

  def is_rvv_vfixed =
    List("vsaddu.vv", "vsaddu.vx", "vsaddu.vi", "vsadd.vv", "vsadd.vx", "vsadd.vi", "vssubu.vv",
	       "vssubu.vx", "vssub.vv", "vssub.vx", "vaaddu.vv", "vaaddu.vx", "vaadd.vv", "vaadd.vx", "vasubu.vv",
	       "vasubu.vx", "vasub.vv", "vasub.vx", "vsmul.vv", "vsmul.vx", "vssrl.vv", "vssrl.vx", "vssrl.vi",
	       "vssra.vv", "vssra.vx", "vssra.vi", "vnclipu.wv", "vnclipu.wx", "vnclipu.wi", "vnclip.wv",
	       "vnclip.wx", "vnclip.wi").contains(opcode)

  def is_rvv_vfloat =
    List("vfadd.vv", "vfadd.vf", "vfsub.vv", "vfsub.vf", "vfrsub.vf", "vfwadd.vv", "vfwadd.vf",
	       "vfwsub.vv", "vfwsub.vf", "vfwadd.wv", "vfwadd.wf", "vfwsub.wv", "vfwsub.wf", "vfmul.vv", "vfmul.vf",
	       "vfdiv.vv", "vfdiv.vf", "vfrdiv.vf", "vfwmul.vv", "vfwmul.vf", "vfmacc.vv", "vfmacc.vf", "vfnmacc.vv",
	       "vfnmacc.vf", "vfmsac.vv", "vfmsac.vf", "vfnmsac.vv", "vfnmsac.vf", "vfmadd.vv", "vfmadd.vf",
	       "vfnmadd.vv", "vfnmadd.vf", "vfmsub.vv", "vfmsub.vf", "vfnmsub.vv", "vfnmsub.vf", "vfwmacc.vv",
	       "vfwmacc.vf", "vfwnmacc.vv", "vfwnmacc.vf", "vfwmsac.vv", "vfwmsac.vf", "vfwnmsac.vv", "vfwnmsac.vf",
	       "vfsqrt.v", "vfmin.vv", "vfmin.vf", "vfmax.vv", "vfmax.vf", "vfsgnj.vv", "vfsgnj.vf", "vfsgnjn.vv",
	       "vfsgnjn.vf", "vfsgnjx.vv", "vfsgnjx.vf", "vmfeq.vv", "vmfeq.vf", "vmfne.vv", "vmfne.vf", "vmflt.vv",
	       "vmflt.vf", "vmfle.vv", "vmfle.vf", "vmfgt.vf", "vmfge.vf", "vfclass.v", "vfmerge.vfm", "vfmv.v.f",
	       "vfcvt.xu.f.v", "vfcvt.x.f.v", "vfcvt.rtz.xu.f.v", "vfcvt.rtz.x.f.v", "vfcvt.f.xu.v", "vfcvt.f.x.v",
	       "vfwcvt.xu.f.v", "vfwcvt.x.f.v", "vfwcvt.rtz.xu.f.v", "vfwcvt.rtz.x.f.v", "vfwcvt.f.xu.v", "vfwcvt.f.x.v",
	       "vfwcvt.f.f.v", "vfncvt.xu.f.w", "vfncvt.x.f.w", "vfncvt.rtz.xu.f.w", "vfncvt.rtz.x.f.w", "vfncvt.f.xu.w",
	       "vfncvt.f.x.w", "vfncvt.f.f.w", "vfncvt.rod.f.f.w").contains(opcode)

  def is_rvv_vreduce =
    List("vredsum.vs", "vredmaxu.vs", "vredmax.vs", "vredminu.vs", "vredmin.vs", "vredand.vs",
	       "vredor.vs", "vredxor.vs", "vwredsumu.vs", "vwredsum.vs", "vfredosum.vs", "vfredsum.vs", "vfredmax.vs",
	       "vfredmin.vs", "vfwredosum.vs", "vfwredsum.vs").contains(opcode)

  def is_rvv_vmask =
    List("vmand.mm", "vmnand.mm", "vmandnot.mm", "vmxor.mm", "vmor.mm", "vmnor.mm", "vmornot.mm",
	       "vmxnor.mm", "vpopc.m", "vfirst.m", "vmsbf.m", "vmsif.m", "vmsof.m", "viota.m", "vid.v").contains(opcode)

  def is_rvv_vpermute =
    List("vmv.x.s", "vmv.s.x", "vfmv.f.s", "vfmv.s.f", "vslideup.vx", "vslideup.vi",
	       "vslidedown.vx", "vslidedown.vi", "vslide1up.vx", "vfslide1up.vf", "vslide1down.vx", "vfslide1down.vf",
	       "vrgather.vv", "vrgather.vx", "vrgather.vi", "vcompress.vm", "vmv1r.v", "vmv2r.v", "vmv4r.v", "vmv8r.v").contains(opcode)

  // ******************************************************************************************

  override def toString =
  {
    operands.find(op => op.isInstanceOf[PredReg]) match
    {
      case Some(pred) => pred + " " + opcode +
        operands.filterNot(op => op.isInstanceOf[PredReg]).mkString(" ", ", ", "")
      case None => opcode + operands.mkString(" ", ", ", "")
    }
  }
}

class Opcode(val name: String)
{
  def apply(opnds: Operand*) = new Inst(name, opnds.toArray)
}

object J extends Opcode("j")
object JAL extends Opcode("jal")
object JALR extends Opcode("jalr")
object BEQ extends Opcode("beq")
object BNE extends Opcode("bne")
object BLT extends Opcode("blt")
object BGE extends Opcode("bge")
object BLTU extends Opcode("bltu")
object BGEU extends Opcode("bgeu")

object LA extends Opcode("la")
object LB extends Opcode("lb")
object LH extends Opcode("lh")
object LW extends Opcode("lw")
object LD extends Opcode("ld")
object LBU extends Opcode("lbu")
object LHU extends Opcode("lhu")
object LWU extends Opcode("lwu")
object SB extends Opcode("sb")
object SH extends Opcode("sh")
object SW extends Opcode("sw")
object SD extends Opcode("sd")

object AMOADD_W extends Opcode("amoadd.w")
object AMOSWAP_W extends Opcode("amoswap.w")
object AMOAND_W extends Opcode("amoand.w")
object AMOOR_W extends Opcode("amoor.w")
object AMOMIN_W extends Opcode("amomin.w")
object AMOMINU_W extends Opcode("amominu.w")
object AMOMAX_W extends Opcode("amomax.w")
object AMOMAXU_W extends Opcode("amomaxu.w")
object AMOXOR_W extends Opcode("amoxor.w")
object AMOADD_D extends Opcode("amoadd.d")
object AMOSWAP_D extends Opcode("amoswap.d")
object AMOAND_D extends Opcode("amoand.d")
object AMOOR_D extends Opcode("amoor.d")
object AMOMIN_D extends Opcode("amomin.d")
object AMOMINU_D extends Opcode("amominu.d")
object AMOMAX_D extends Opcode("amomax.d")
object AMOMAXU_D extends Opcode("amomaxu.d")
object AMOXOR_D extends Opcode("amoxor.d")

object ADDI extends Opcode("addi")
object SLLI extends Opcode("slli")
object SLTI extends Opcode("slti")
object SLTIU extends Opcode("sltiu")
object XORI extends Opcode("xori")
object SRLI extends Opcode("srli")
object SRAI extends Opcode("srai")
object ORI extends Opcode("ori")
object ANDI extends Opcode("andi")
object ADD extends Opcode("add")
object SUB extends Opcode("sub")
object SLL extends Opcode("sll")
object SLT extends Opcode("slt")
object SLTU extends Opcode("sltu")
object XOR extends Opcode("xor")
object SRL extends Opcode("srl")
object SRA extends Opcode("sra")
object OR extends Opcode("or")
object AND extends Opcode("and")
object MUL extends Opcode("mul")
object MULH extends Opcode("mulh")
object MULHSU extends Opcode("mulhsu")
object MULHU extends Opcode("mulhu")
object DIV extends Opcode("div")
object DIVU extends Opcode("divu")
object REM extends Opcode("rem")
object REMU extends Opcode("remu")
object LUI extends Opcode("lui")

object ADDIW extends Opcode("addiw")
object SLLIW extends Opcode("slliw")
object SRLIW extends Opcode("srliw")
object SRAIW extends Opcode("sraiw")
object ADDW extends Opcode("addw")
object SUBW extends Opcode("subw")
object SLLW extends Opcode("sllw")
object SRLW extends Opcode("srlw")
object SRAW extends Opcode("sraw")
object MULW extends Opcode("mulw")
object DIVW extends Opcode("divw")
object DIVUW extends Opcode("divuw")
object REMW extends Opcode("remw")
object REMUW extends Opcode("remuw")

object FLW extends Opcode("flw")
object FLD extends Opcode("fld")
object FSW extends Opcode("fsw")
object FSD extends Opcode("fsd")

object FADD_S extends Opcode("fadd.s")
object FSUB_S extends Opcode("fsub.s")
object FMUL_S extends Opcode("fmul.s")
object FDIV_S extends Opcode("fdiv.s")
object FSQRT_S extends Opcode("fsqrt.s")
object FMIN_S extends Opcode("fmin.s")
object FMAX_S extends Opcode("fmax.s")
object FADD_D extends Opcode("fadd.d")
object FSUB_D extends Opcode("fsub.d")
object FMUL_D extends Opcode("fmul.d")
object FDIV_D extends Opcode("fdiv.d")
object FSQRT_D extends Opcode("fsqrt.d")
object FMIN_D extends Opcode("fmin.d")
object FMAX_D extends Opcode("fmax.d")
object FSGNJ_S extends Opcode("fsgnj.s")
object FSGNJN_S extends Opcode("fsgnjn.s")
object FSGNJX_S extends Opcode("fsgnjx.s")
object FSGNJ_D extends Opcode("fsgnj.d")
object FSGNJN_D extends Opcode("fsgnjn.d")
object FSGNJX_D extends Opcode("fsgnjx.d")

object FMADD_S extends Opcode("fmadd.s")
object FMSUB_S extends Opcode("fmsub.s")
object FNMSUB_S extends Opcode("fnmsub.s")
object FNMADD_S extends Opcode("fnmadd.s")
object FMADD_D extends Opcode("fmadd.d")
object FMSUB_D extends Opcode("fmsub.d")
object FNMSUB_D extends Opcode("fnmsub.d")
object FNMADD_D extends Opcode("fnmadd.d")

object FCVT_S_D extends Opcode("fcvt.s.d")
object FCVT_D_S extends Opcode("fcvt.d.s")
object FCVT_S_L extends Opcode("fcvt.s.l")
object FCVT_S_LU extends Opcode("fcvt.s.lu")
object FCVT_S_W extends Opcode("fcvt.s.w")
object FCVT_S_WU extends Opcode("fcvt.s.wu")
object FCVT_D_L extends Opcode("fcvt.d.l")
object FCVT_D_LU extends Opcode("fcvt.d.lu")
object FCVT_D_W extends Opcode("fcvt.d.w")
object FCVT_D_WU extends Opcode("fcvt.d.wu")
object FCVT_L_S extends Opcode("fcvt.l.s")
object FCVT_LU_S extends Opcode("fcvt.lu.s")
object FCVT_W_S extends Opcode("fcvt.w.s")
object FCVT_WU_S extends Opcode("fcvt.wu.s")
object FCVT_L_D extends Opcode("fcvt.l.d")
object FCVT_LU_D extends Opcode("fcvt.lu.d")
object FCVT_W_D extends Opcode("fcvt.w.d")
object FCVT_WU_D extends Opcode("fcvt.wu.d")

object FMV_X_S extends Opcode("fmv.x.s")
object FMV_S_X extends Opcode("fmv.s.x")
object FMV_X_D extends Opcode("fmv.x.d")
object FMV_D_X extends Opcode("fmv.d.x")

object FRSR extends Opcode("frsr")
object FSSR extends Opcode("fssr")

object FEQ_S extends Opcode("feq.s")
object FLT_S extends Opcode("flt.s")
object FLE_S extends Opcode("fle.s")
object FEQ_D extends Opcode("feq.d")
object FLT_D extends Opcode("flt.d")
object FLE_D extends Opcode("fle.d")

object FENCE_I extends Opcode("fence.i")
object FENCE extends Opcode("fence")

object SYSCALL extends Opcode("syscall")
object BREAK extends Opcode("break")
object RDCYCLE extends Opcode("rdcycle")
object RDTIME extends Opcode("rdtime")
object RDINSTRET extends Opcode("rdinstret")
object NOP extends Opcode("nop")
object LI extends Opcode("li")
object MFPCR extends Opcode("mfpcr")
object MTPCR extends Opcode("mtpcr")
object AUIPC extends Opcode("auipc")

object VVCFGIVL extends Opcode("vvcfgivl")
object VSTOP extends Opcode("vstop")
object VSETVL extends Opcode("vsetvl")
object VEIDX extends Opcode("veidx")
object VF extends Opcode("vf")
object VMCS extends Opcode("vmcs")
object VMCA extends Opcode("vmca")

object VADDI extends Opcode("vaddi")
object VSLLI extends Opcode("vslli")
object VXORI extends Opcode("vxori")
object VSRLI extends Opcode("vsrli")
object VSRAI extends Opcode("vsrai")
object VORI extends Opcode("vori")
object VANDI extends Opcode("vandi")
object VLUI extends Opcode("vlui")
object VADDIW extends Opcode("vaddiw")
object VSLLIW extends Opcode("vslliw")
object VSRLIW extends Opcode("vsrliw")
object VSRAIW extends Opcode("vsraiw")

object VADD extends Opcode("vadd")
object VSUB extends Opcode("vsub")
object VSLL extends Opcode("vsll")
object VXOR extends Opcode("vxor")
object VSRL extends Opcode("vsrl")
object VSRA extends Opcode("vsra")
object VOR extends Opcode("vor")
object VAND extends Opcode("vand")
object VMUL extends Opcode("vmul")
object VMULH extends Opcode("vmulh")
object VMULHSU extends Opcode("vmulhsu")
object VMULHU extends Opcode("vmulhu")
object VDIV extends Opcode("vdiv")
object VDIVU extends Opcode("vdivu")
object VREM extends Opcode("vrem")
object VREMU extends Opcode("vremu")
object VADDW extends Opcode("vaddw")
object VSUBW extends Opcode("vsubw")
object VSLLW extends Opcode("vsllw")
object VSRLW extends Opcode("vsrlw")
object VSRAW extends Opcode("vsraw")
object VMULW extends Opcode("vmulw")
object VDIVW extends Opcode("vdivw")
object VDIVUW extends Opcode("vdivuw")
object VREMW extends Opcode("vremw")
object VREMUW extends Opcode("vremuw")

object VCMPEQ extends Opcode("vcmpeq")
object VCMPLT extends Opcode("vcmplt")
object VCMPLTU extends Opcode("vcmpltu")
object VCMPFEQ extends Opcode("vcmpfeq")
object VCMPFLT extends Opcode("vcmpflt")
object VCMPFLE extends Opcode("vcmpfle")

object VPOP extends Opcode("vpop")
object VPSET extends Opcode("vpset")
object VPCLEAR extends Opcode("vpclear")

object VFADD_S extends Opcode("vfadd.s")
object VFSUB_S extends Opcode("vfsub.s")
object VFMUL_S extends Opcode("vfmul.s")
object VFDIV_S extends Opcode("vfdiv.s")
object VFSQRT_S extends Opcode("vfsqrt.s")
object VFMIN_S extends Opcode("vfmin.s")
object VFMAX_S extends Opcode("vfmax.s")
object VFADD_D extends Opcode("vfadd.d")
object VFSUB_D extends Opcode("vfsub.d")
object VFMUL_D extends Opcode("vfmul.d")
object VFDIV_D extends Opcode("vfdiv.d")
object VFSQRT_D extends Opcode("vfsqrt.d")
object VFMIN_D extends Opcode("vfmin.d")
object VFMAX_D extends Opcode("vfmax.d")
object VFSGNJ_S extends Opcode("vfsgnj.s")
object VFSGNJN_S extends Opcode("vfsgnjn.s")
object VFSGNJX_S extends Opcode("vfsgnjx.s")
object VFSGNJ_D extends Opcode("vfsgnj.d")
object VFSGNJN_D extends Opcode("vfsgnjn.d")
object VFSGNJX_D extends Opcode("vfsgnjx.d")

object VFMADD_S extends Opcode("vfmadd.s")
object VFMSUB_S extends Opcode("vfmsub.s")
object VFNMSUB_S extends Opcode("vfnmsub.s")
object VFNMADD_S extends Opcode("vfnmadd.s")
object VFMADD_D extends Opcode("vfmadd.d")
object VFMSUB_D extends Opcode("vfmsub.d")
object VFNMSUB_D extends Opcode("vfnmsub.d")
object VFNMADD_D extends Opcode("vfnmadd.d")

object VFCVT_S_D extends Opcode("vfcvt.s.d")
object VFCVT_D_S extends Opcode("vfcvt.d.s")
object VFCVT_S_L extends Opcode("vfcvt.s.l")
object VFCVT_S_LU extends Opcode("vfcvt.s.lu")
object VFCVT_S_W extends Opcode("vfcvt.s.w")
object VFCVT_S_WU extends Opcode("vfcvt.s.wu")
object VFCVT_D_L extends Opcode("vfcvt.d.l")
object VFCVT_D_LU extends Opcode("vfcvt.d.lu")
object VFCVT_D_W extends Opcode("vfcvt.d.w")
object VFCVT_D_WU extends Opcode("vfcvt.d.wu")
object VFCVT_L_S extends Opcode("vfcvt.l.s")
object VFCVT_LU_S extends Opcode("vfcvt.lu.s")
object VFCVT_W_S extends Opcode("vfcvt.w.s")
object VFCVT_WU_S extends Opcode("vfcvt.wu.s")
object VFCVT_L_D extends Opcode("vfcvt.l.d")
object VFCVT_LU_D extends Opcode("vfcvt.lu.d")
object VFCVT_W_D extends Opcode("vfcvt.w.d")
object VFCVT_WU_D extends Opcode("vfcvt.wu.d")

object VLSB extends Opcode("vlsb")
object VLSH extends Opcode("vlsh")
object VLSW extends Opcode("vlsw")
object VLSD extends Opcode("vlsd")
object VLSBU extends Opcode("vlsbu")
object VLSHU extends Opcode("vlshu")
object VLSWU extends Opcode("vlswu")
object VSSB extends Opcode("vssb")
object VSSH extends Opcode("vssh")
object VSSW extends Opcode("vssw")
object VSSD extends Opcode("vssd")
object VLAB extends Opcode("vlab")
object VLAH extends Opcode("vlah")
object VLAW extends Opcode("vlaw")
object VLAD extends Opcode("vlad")
object VLABU extends Opcode("vlabu")
object VLAHU extends Opcode("vlahu")
object VLAWU extends Opcode("vlawu")
object VSAB extends Opcode("vsab")
object VSAH extends Opcode("vsah")
object VSAW extends Opcode("vsaw")
object VSAD extends Opcode("vsad")

object VLB extends Opcode("vlb")
object VLH extends Opcode("vlh")
object VLW extends Opcode("vlw")
object VLD extends Opcode("vld")
object VLBU extends Opcode("vlbu")
object VLHU extends Opcode("vlhu")
object VLWU extends Opcode("vlwu")
object VSB extends Opcode("vsb")
object VSH extends Opcode("vsh")
object VSW extends Opcode("vsw")
object VSD extends Opcode("vsd")

object VLSEGB extends Opcode("vlsegb")
object VLSEGH extends Opcode("vlsegh")
object VLSEGW extends Opcode("vlsegw")
object VLSEGD extends Opcode("vlsegd")
object VLSEGBU extends Opcode("vlsegbu")
object VLSEGHU extends Opcode("vlseghu")
object VLSEGWU extends Opcode("vlsegwu")
object VSSEGB extends Opcode("vssegb")
object VSSEGH extends Opcode("vssegh")
object VSSEGW extends Opcode("vssegw")
object VSSEGD extends Opcode("vssegd")

object VLSTB extends Opcode("vlstb")
object VLSTH extends Opcode("vlsth")
object VLSTW extends Opcode("vlstw")
object VLSTD extends Opcode("vlstd")
object VLSTBU extends Opcode("vlstbu")
object VLSTHU extends Opcode("vlsthu")
object VLSTWU extends Opcode("vlstwu")
object VSSTB extends Opcode("vsstb")
object VSSTH extends Opcode("vssth")
object VSSTW extends Opcode("vsstw")
object VSSTD extends Opcode("vsstd")

object VLSEGSTB extends Opcode("vlsegstb")
object VLSEGSTH extends Opcode("vlsegsth")
object VLSEGSTW extends Opcode("vlsegstw")
object VLSEGSTD extends Opcode("vlsegstd")
object VLSEGSTBU extends Opcode("vlsegstbu")
object VLSEGSTHU extends Opcode("vlsegsthu")
object VLSEGSTWU extends Opcode("vlsegstwu")
object VSSEGSTB extends Opcode("vssegstb")
object VSSEGSTH extends Opcode("vssegsth")
object VSSEGSTW extends Opcode("vssegstw")
object VSSEGSTD extends Opcode("vssegstd")

object VLXB extends Opcode("vlxb")
object VLXH extends Opcode("vlxh")
object VLXW extends Opcode("vlxw")
object VLXD extends Opcode("vlxd")
object VLXBU extends Opcode("vlxbu")
object VLXHU extends Opcode("vlxhu")
object VLXWU extends Opcode("vlxwu")
object VSXB extends Opcode("vsxb")
object VSXH extends Opcode("vsxh")
object VSXW extends Opcode("vsxw")
object VSXD extends Opcode("vsxd")

object VLSEGXB extends Opcode("vlsegxb")
object VLSEGXH extends Opcode("vlsegxh")
object VLSEGXW extends Opcode("vlsegxw")
object VLSEGXD extends Opcode("vlsegxd")
object VLSEGXBU extends Opcode("vlsegxbu")
object VLSEGXHU extends Opcode("vlsegxhu")
object VLSEGXWU extends Opcode("vlsegxwu")
object VSSEGXB extends Opcode("vssegxb")
object VSSEGXH extends Opcode("vssegxh")
object VSSEGXW extends Opcode("vssegxw")
object VSSEGXD extends Opcode("vssegxd")

object VAMOADD_W extends Opcode("vamoadd.w")
object VAMOSWAP_W extends Opcode("vamoswap.w")
object VAMOAND_W extends Opcode("vamoand.w")
object VAMOOR_W extends Opcode("vamoor.w")
object VAMOMIN_W extends Opcode("vamomin.w")
object VAMOMINU_W extends Opcode("vamominu.w")
object VAMOMAX_W extends Opcode("vamomax.w")
object VAMOMAXU_W extends Opcode("vamomaxu.w")
object VAMOXOR_W extends Opcode("vamoxor.w")
object VAMOADD_D extends Opcode("vamoadd.d")
object VAMOSWAP_D extends Opcode("vamoswap.d")
object VAMOAND_D extends Opcode("vamoand.d")
object VAMOOR_D extends Opcode("vamoor.d")
object VAMOMIN_D extends Opcode("vamomin.d")
object VAMOMINU_D extends Opcode("vamominu.d")
object VAMOMAX_D extends Opcode("vamomax.d")
object VAMOMAXU_D extends Opcode("vamomaxu.d")
object VAMOXOR_D extends Opcode("vamoxor.d")

object MOVZ extends Opcode("movz")
object MOVN extends Opcode("movn")
object FMOVZ extends Opcode("fmovz")
object FMOVN extends Opcode("fmovn")

object FENCE_V extends Opcode("fence")

object ILLEGAL extends Opcode(".word")

// ***** RISC-V Vector Ext (v0.9) --- Opcode Objects ******************************************

//is_rvv_vconfig
object RVV_VSETVLI extends Opcode("vsetvli")		// prefix added as a hwacha object exists with same name
object RVV_VSETVL extends Opcode("vsetvl")			// prefix added as a hwacha object exists with same name

//is_rvv_vmem

//----------------------------------------
object VLE8_V extends Opcode("vle8.v")
object VLE16_V extends Opcode("vle16.v")
object VLE32_V extends Opcode("vle32.v")
object VLE64_V extends Opcode("vle64.v")
object VLE128_V extends Opcode("vle128.v")
object VLE256_V extends Opcode("vle256.v")
object VLE512_V extends Opcode("vle512.v")
object VLE1024_V extends Opcode("vle1024.v")

//----------------------------------------
object VSE8_V extends Opcode("vse8.v")
object VSE16_V extends Opcode("vse16.v")
object VSE32_V extends Opcode("vse32.v")
object VSE64_V extends Opcode("vse64.v")
object VSE128_V extends Opcode("vse128.v")
object VSE256_V extends Opcode("vse256.v")
object VSE512_V extends Opcode("vse512.v")
object VSE1024_V extends Opcode("vse1024.v")

//////////////    EXTENDED Zvlsseg Unit_Strided    //////////////

//load nf=2
object VLSEG2E8_V    extends Opcode("vlseg2e8.v")
object VLSEG2E16_V   extends Opcode("vlseg2e16.v")
object VLSEG2E32_V   extends Opcode("vlseg2e32.v")
object VLSEG2E64_V   extends Opcode("vlseg2e64.v")
object VLSEG2E128_V  extends Opcode("vlseg2e128.v")
object VLSEG2E256_V  extends Opcode("vlseg2e256.v")
object VLSEG2E512_V  extends Opcode("vlseg2e512.v")
object VLSEG2E1024_V extends Opcode("vlseg2e1024.v")
//store
object VSSEG2E8_V    extends Opcode("vsseg2e8.v")
object VSSEG2E16_V   extends Opcode("vsseg2e16.v")
object VSSEG2E32_V   extends Opcode("vsseg2e32.v")
object VSSEG2E64_V   extends Opcode("vsseg2e64.v")
object VSSEG2E128_V  extends Opcode("vsseg2e128.v")
object VSSEG2E256_V  extends Opcode("vsseg2e256.v")
object VSSEG2E512_V  extends Opcode("vsseg2e512.v")
object VSSEG2E1024_V extends Opcode("vsseg2e1024.v")

//load nf=3
object VLSEG3E8_V    extends Opcode("vlseg3e8.v")
object VLSEG3E16_V   extends Opcode("vlseg3e16.v")
object VLSEG3E32_V   extends Opcode("vlseg3e32.v")
object VLSEG3E64_V   extends Opcode("vlseg3e64.v")
object VLSEG3E128_V  extends Opcode("vlseg3e128.v")
object VLSEG3E256_V  extends Opcode("vlseg3e256.v")
object VLSEG3E512_V  extends Opcode("vlseg3e512.v")
object VLSEG3E1024_V extends Opcode("vlseg3e1024.v")
//store
object VSSEG3E8_V    extends Opcode("vsseg3e8.v")
object VSSEG3E16_V   extends Opcode("vsseg3e16.v")
object VSSEG3E32_V   extends Opcode("vsseg3e32.v")
object VSSEG3E64_V   extends Opcode("vsseg3e64.v")
object VSSEG3E128_V  extends Opcode("vsseg3e128.v")
object VSSEG3E256_V  extends Opcode("vsseg3e256.v")
object VSSEG3E512_V  extends Opcode("vsseg3e512.v")
object VSSEG3E1024_V extends Opcode("vsseg3e1024.v")

//load nf=4
object VLSEG4E8_V    extends Opcode("vlseg4e8.v")
object VLSEG4E16_V   extends Opcode("vlseg4e16.v")
object VLSEG4E32_V   extends Opcode("vlseg4e32.v")
object VLSEG4E64_V   extends Opcode("vlseg4e64.v")
object VLSEG4E128_V  extends Opcode("vlseg4e128.v")
object VLSEG4E256_V  extends Opcode("vlseg4e256.v")
object VLSEG4E512_V  extends Opcode("vlseg4e512.v")
object VLSEG4E1024_V extends Opcode("vlseg4e1024.v")
//store
object VSSEG4E8_V    extends Opcode("vsseg4e8.v")
object VSSEG4E16_V   extends Opcode("vsseg4e16.v")
object VSSEG4E32_V   extends Opcode("vsseg4e32.v")
object VSSEG4E64_V   extends Opcode("vsseg4e64.v")
object VSSEG4E128_V  extends Opcode("vsseg4e128.v")
object VSSEG4E256_V  extends Opcode("vsseg4e256.v")
object VSSEG4E512_V  extends Opcode("vsseg4e512.v")
object VSSEG4E1024_V extends Opcode("vsseg4e1024.v")

//load nf=5
object VLSEG5E8_V    extends Opcode("vlseg5e8.v")
object VLSEG5E16_V   extends Opcode("vlseg5e16.v")
object VLSEG5E32_V   extends Opcode("vlseg5e32.v")
object VLSEG5E64_V   extends Opcode("vlseg5e64.v")
object VLSEG5E128_V  extends Opcode("vlseg5e128.v")
object VLSEG5E256_V  extends Opcode("vlseg5e256.v")
object VLSEG5E512_V  extends Opcode("vlseg5e512.v")
object VLSEG5E1024_V extends Opcode("vlseg5e1024.v")
//store
object VSSEG5E8_V    extends Opcode("vsseg5e8.v")
object VSSEG5E16_V   extends Opcode("vsseg5e16.v")
object VSSEG5E32_V   extends Opcode("vsseg5e32.v")
object VSSEG5E64_V   extends Opcode("vsseg5e64.v")
object VSSEG5E128_V  extends Opcode("vsseg5e128.v")
object VSSEG5E256_V  extends Opcode("vsseg5e256.v")
object VSSEG5E512_V  extends Opcode("vsseg5e512.v")
object VSSEG5E1024_V extends Opcode("vsseg5e1024.v")

//load nf=6
object VLSEG6E8_V    extends Opcode("vlseg6e8.v")
object VLSEG6E16_V   extends Opcode("vlseg6e16.v")
object VLSEG6E32_V   extends Opcode("vlseg6e32.v")
object VLSEG6E64_V   extends Opcode("vlseg6e64.v")
object VLSEG6E128_V  extends Opcode("vlseg6e128.v")
object VLSEG6E256_V  extends Opcode("vlseg6e256.v")
object VLSEG6E512_V  extends Opcode("vlseg6e512.v")
object VLSEG6E1024_V extends Opcode("vlseg6e1024.v")
//store
object VSSEG6E8_V    extends Opcode("vsseg6e8.v")
object VSSEG6E16_V   extends Opcode("vsseg6e16.v")
object VSSEG6E32_V   extends Opcode("vsseg6e32.v")
object VSSEG6E64_V   extends Opcode("vsseg6e64.v")
object VSSEG6E128_V  extends Opcode("vsseg6e128.v")
object VSSEG6E256_V  extends Opcode("vsseg6e256.v")
object VSSEG6E512_V  extends Opcode("vsseg6e512.v")
object VSSEG6E1024_V extends Opcode("vsseg6e1024.v")

//load nf=7
object VLSEG7E8_V    extends Opcode("vlseg7e8.v")
object VLSEG7E16_V   extends Opcode("vlseg7e16.v")
object VLSEG7E32_V   extends Opcode("vlseg7e32.v")
object VLSEG7E64_V   extends Opcode("vlseg7e64.v")
object VLSEG7E128_V  extends Opcode("vlseg7e128.v")
object VLSEG7E256_V  extends Opcode("vlseg7e256.v")
object VLSEG7E512_V  extends Opcode("vlseg7e512.v")
object VLSEG7E1024_V extends Opcode("vlseg7e1024.v")
//store
object VSSEG7E8_V    extends Opcode("vsseg7e8.v")
object VSSEG7E16_V   extends Opcode("vsseg7e16.v")
object VSSEG7E32_V   extends Opcode("vsseg7e32.v")
object VSSEG7E64_V   extends Opcode("vsseg7e64.v")
object VSSEG7E128_V  extends Opcode("vsseg7e128.v")
object VSSEG7E256_V  extends Opcode("vsseg7e256.v")
object VSSEG7E512_V  extends Opcode("vsseg7e512.v")
object VSSEG7E1024_V extends Opcode("vsseg7e1024.v")

//load nf=8
object VLSEG8E8_V    extends Opcode("vlseg8e8.v")
object VLSEG8E16_V   extends Opcode("vlseg8e16.v")
object VLSEG8E32_V   extends Opcode("vlseg8e32.v")
object VLSEG8E64_V   extends Opcode("vlseg8e64.v")
object VLSEG8E128_V  extends Opcode("vlseg8e128.v")
object VLSEG8E256_V  extends Opcode("vlseg8e256.v")
object VLSEG8E512_V  extends Opcode("vlseg8e512.v")
object VLSEG8E1024_V extends Opcode("vlseg8e1024.v")
//store
object VSSEG8E8_V    extends Opcode("vsseg8e8.v")
object VSSEG8E16_V   extends Opcode("vsseg8e16.v")
object VSSEG8E32_V   extends Opcode("vsseg8e32.v")
object VSSEG8E64_V   extends Opcode("vsseg8e64.v")
object VSSEG8E128_V  extends Opcode("vsseg8e128.v")
object VSSEG8E256_V  extends Opcode("vsseg8e256.v")
object VSSEG8E512_V  extends Opcode("vsseg8e512.v")
object VSSEG8E1024_V extends Opcode("vsseg8e1024.v")

//----------------------------------------
object VLSE8_V extends Opcode("vlse8.v")
object VLSE16_V extends Opcode("vlse16.v")
object VLSE32_V extends Opcode("vlse32.v")
object VLSE64_V extends Opcode("vlse64.v")
object VLSE128_V extends Opcode("vlse128.v")
object VLSE256_V extends Opcode("vlse256.v")
object VLSE512_V extends Opcode("vlse512.v")
object VLSE1024_V extends Opcode("vlse1024.v")

//----------------------------------------
object VSSE8_V extends Opcode("vsse8.v")
object VSSE16_V extends Opcode("vsse16.v")
object VSSE32_V extends Opcode("vsse32.v")
object VSSE64_V extends Opcode("vsse64.v")
object VSSE128_V extends Opcode("vsse128.v")
object VSSE256_V extends Opcode("vsse256.v")
object VSSE512_V extends Opcode("vsse512.v")
object VSSE1024_V extends Opcode("vsse1024.v")

//////////////    EXTENDED Zvlsseg Constant_Strided    //////////////

//load nf=2
object VLSSEG2E8_V    extends Opcode("vlsseg2e8.v")
object VLSSEG2E16_V   extends Opcode("vlsseg2e16.v")
object VLSSEG2E32_V   extends Opcode("vlsseg2e32.v")
object VLSSEG2E64_V   extends Opcode("vlsseg2e64.v")
object VLSSEG2E128_V  extends Opcode("vlsseg2e128.v")
object VLSSEG2E256_V  extends Opcode("vlsseg2e256.v")
object VLSSEG2E512_V  extends Opcode("vlsseg2e512.v")
object VLSSEG2E1024_V extends Opcode("vlsseg2e1024.v")
//store
object VSSSEG2E8_V    extends Opcode("vssseg2e8.v")
object VSSSEG2E16_V   extends Opcode("vssseg2e16.v")
object VSSSEG2E32_V   extends Opcode("vssseg2e32.v")
object VSSSEG2E64_V   extends Opcode("vssseg2e64.v")
object VSSSEG2E128_V  extends Opcode("vssseg2e128.v")
object VSSSEG2E256_V  extends Opcode("vssseg2e256.v")
object VSSSEG2E512_V  extends Opcode("vssseg2e512.v")
object VSSSEG2E1024_V extends Opcode("vssseg2e1024.v")

//load nf=3
object VLSSEG3E8_V    extends Opcode("vlsseg3e8.v")
object VLSSEG3E16_V   extends Opcode("vlsseg3e16.v")
object VLSSEG3E32_V   extends Opcode("vlsseg3e32.v")
object VLSSEG3E64_V   extends Opcode("vlsseg3e64.v")
object VLSSEG3E128_V  extends Opcode("vlsseg3e128.v")
object VLSSEG3E256_V  extends Opcode("vlsseg3e256.v")
object VLSSEG3E512_V  extends Opcode("vlsseg3e512.v")
object VLSSEG3E1024_V extends Opcode("vlsseg3e1024.v")
//store
object VSSSEG3E8_V    extends Opcode("vssseg3e8.v")
object VSSSEG3E16_V   extends Opcode("vssseg3e16.v")
object VSSSEG3E32_V   extends Opcode("vssseg3e32.v")
object VSSSEG3E64_V   extends Opcode("vssseg3e64.v")
object VSSSEG3E128_V  extends Opcode("vssseg3e128.v")
object VSSSEG3E256_V  extends Opcode("vssseg3e256.v")
object VSSSEG3E512_V  extends Opcode("vssseg3e512.v")
object VSSSEG3E1024_V extends Opcode("vssseg3e1024.v")

//load nf=4
object VLSSEG4E8_V    extends Opcode("vlsseg4e8.v")
object VLSSEG4E16_V   extends Opcode("vlsseg4e16.v")
object VLSSEG4E32_V   extends Opcode("vlsseg4e32.v")
object VLSSEG4E64_V   extends Opcode("vlsseg4e64.v")
object VLSSEG4E128_V  extends Opcode("vlsseg4e128.v")
object VLSSEG4E256_V  extends Opcode("vlsseg4e256.v")
object VLSSEG4E512_V  extends Opcode("vlsseg4e512.v")
object VLSSEG4E1024_V extends Opcode("vlsseg4e1024.v")
//store
object VSSSEG4E8_V    extends Opcode("vssseg4e8.v")
object VSSSEG4E16_V   extends Opcode("vssseg4e16.v")
object VSSSEG4E32_V   extends Opcode("vssseg4e32.v")
object VSSSEG4E64_V   extends Opcode("vssseg4e64.v")
object VSSSEG4E128_V  extends Opcode("vssseg4e128.v")
object VSSSEG4E256_V  extends Opcode("vssseg4e256.v")
object VSSSEG4E512_V  extends Opcode("vssseg4e512.v")
object VSSSEG4E1024_V extends Opcode("vssseg4e1024.v")

//load nf=5
object VLSSEG5E8_V    extends Opcode("vlsseg5e8.v")
object VLSSEG5E16_V   extends Opcode("vlsseg5e16.v")
object VLSSEG5E32_V   extends Opcode("vlsseg5e32.v")
object VLSSEG5E64_V   extends Opcode("vlsseg5e64.v")
object VLSSEG5E128_V  extends Opcode("vlsseg5e128.v")
object VLSSEG5E256_V  extends Opcode("vlsseg5e256.v")
object VLSSEG5E512_V  extends Opcode("vlsseg5e512.v")
object VLSSEG5E1024_V extends Opcode("vlsseg5e1024.v")
//store
object VSSSEG5E8_V    extends Opcode("vssseg5e8.v")
object VSSSEG5E16_V   extends Opcode("vssseg5e16.v")
object VSSSEG5E32_V   extends Opcode("vssseg5e32.v")
object VSSSEG5E64_V   extends Opcode("vssseg5e64.v")
object VSSSEG5E128_V  extends Opcode("vssseg5e128.v")
object VSSSEG5E256_V  extends Opcode("vssseg5e256.v")
object VSSSEG5E512_V  extends Opcode("vssseg5e512.v")
object VSSSEG5E1024_V extends Opcode("vssseg5e1024.v")

//load nf=6
object VLSSEG6E8_V    extends Opcode("vlsseg6e8.v")
object VLSSEG6E16_V   extends Opcode("vlsseg6e16.v")
object VLSSEG6E32_V   extends Opcode("vlsseg6e32.v")
object VLSSEG6E64_V   extends Opcode("vlsseg6e64.v")
object VLSSEG6E128_V  extends Opcode("vlsseg6e128.v")
object VLSSEG6E256_V  extends Opcode("vlsseg6e256.v")
object VLSSEG6E512_V  extends Opcode("vlsseg6e512.v")
object VLSSEG6E1024_V extends Opcode("vlsseg6e1024.v")
//store
object VSSSEG6E8_V    extends Opcode("vssseg6e8.v")
object VSSSEG6E16_V   extends Opcode("vssseg6e16.v")
object VSSSEG6E32_V   extends Opcode("vssseg6e32.v")
object VSSSEG6E64_V   extends Opcode("vssseg6e64.v")
object VSSSEG6E128_V  extends Opcode("vssseg6e128.v")
object VSSSEG6E256_V  extends Opcode("vssseg6e256.v")
object VSSSEG6E512_V  extends Opcode("vssseg6e512.v")
object VSSSEG6E1024_V extends Opcode("vssseg6e1024.v")

//load nf=7
object VLSSEG7E8_V    extends Opcode("vlsseg7e8.v")
object VLSSEG7E16_V   extends Opcode("vlsseg7e16.v")
object VLSSEG7E32_V   extends Opcode("vlsseg7e32.v")
object VLSSEG7E64_V   extends Opcode("vlsseg7e64.v")
object VLSSEG7E128_V  extends Opcode("vlsseg7e128.v")
object VLSSEG7E256_V  extends Opcode("vlsseg7e256.v")
object VLSSEG7E512_V  extends Opcode("vlsseg7e512.v")
object VLSSEG7E1024_V extends Opcode("vlsseg7e1024.v")
//store
object VSSSEG7E8_V    extends Opcode("vssseg7e8.v")
object VSSSEG7E16_V   extends Opcode("vssseg7e16.v")
object VSSSEG7E32_V   extends Opcode("vssseg7e32.v")
object VSSSEG7E64_V   extends Opcode("vssseg7e64.v")
object VSSSEG7E128_V  extends Opcode("vssseg7e128.v")
object VSSSEG7E256_V  extends Opcode("vssseg7e256.v")
object VSSSEG7E512_V  extends Opcode("vssseg7e512.v")
object VSSSEG7E1024_V extends Opcode("vssseg7e1024.v")

//load nf=8
object VLSSEG8E8_V    extends Opcode("vlsseg8e8.v")
object VLSSEG8E16_V   extends Opcode("vlsseg8e16.v")
object VLSSEG8E32_V   extends Opcode("vlsseg8e32.v")
object VLSSEG8E64_V   extends Opcode("vlsseg8e64.v")
object VLSSEG8E128_V  extends Opcode("vlsseg8e128.v")
object VLSSEG8E256_V  extends Opcode("vlsseg8e256.v")
object VLSSEG8E512_V  extends Opcode("vlsseg8e512.v")
object VLSSEG8E1024_V extends Opcode("vlsseg8e1024.v")
//store
object VSSSEG8E8_V    extends Opcode("vssseg8e8.v")
object VSSSEG8E16_V   extends Opcode("vssseg8e16.v")
object VSSSEG8E32_V   extends Opcode("vssseg8e32.v")
object VSSSEG8E64_V   extends Opcode("vssseg8e64.v")
object VSSSEG8E128_V  extends Opcode("vssseg8e128.v")
object VSSSEG8E256_V  extends Opcode("vssseg8e256.v")
object VSSSEG8E512_V  extends Opcode("vssseg8e512.v")
object VSSSEG8E1024_V extends Opcode("vssseg8e1024.v")

//----------------------------------------
object VLXEI8_V extends Opcode("vlxei8.v")
object VLXEI16_V extends Opcode("vlxei16.v")
object VLXEI32_V extends Opcode("vlxei32.v")
object VLXEI64_V extends Opcode("vlxei64.v")
object VLXEI128_V extends Opcode("vlxei128.v")
object VLXEI256_V extends Opcode("vlxei256.v")
object VLXEI512_V extends Opcode("vlxei512.v")
object VLXEI1024_V extends Opcode("vlxei1024.v")

//----------------------------------------
object VSXEI8_V extends Opcode("vsxei8.v")
object VSXEI16_V extends Opcode("vsxei16.v")
object VSXEI32_V extends Opcode("vsxei32.v")
object VSXEI64_V extends Opcode("vsxei64.v")
object VSXEI128_V extends Opcode("vsxei128.v")
object VSXEI256_V extends Opcode("vsxei256.v")
object VSXEI512_V extends Opcode("vsxei512.v")
object VSXEI1024_V extends Opcode("vsxei1024.v")

//----------------------------------------
object VSUXEI8_V extends Opcode("vsuxei8.v")
object VSUXEI16_V extends Opcode("vsuxei16.v")
object VSUXEI32_V extends Opcode("vsuxei32.v")
object VSUXEI64_V extends Opcode("vsuxei64.v")
object VSUXEI128_V extends Opcode("vsuxei128.v")
object VSUXEI256_V extends Opcode("vsuxei256.v")
object VSUXEI512_V extends Opcode("vsuxei512.v")
object VSUXEI1024_V extends Opcode("vsuxei1024.v")

//////////////    EXTENDED Zvlsseg Vector Strided    //////////////

//load nf=2
object VLXSEG2E8_V    extends Opcode("vlxseg2ei8.v")
object VLXSEG2E16_V   extends Opcode("vlxseg2ei16.v")
object VLXSEG2E32_V   extends Opcode("vlxseg2ei32.v")
object VLXSEG2E64_V   extends Opcode("vlxseg2ei64.v")
object VLXSEG2E128_V  extends Opcode("vlxseg2ei128.v")
object VLXSEG2E256_V  extends Opcode("vlxseg2ei256.v")
object VLXSEG2E512_V  extends Opcode("vlxseg2ei512.v")
object VLXSEG2E1024_V extends Opcode("vlxseg2ei1024.v")
//store
object VSXSEG2E8_V    extends Opcode("vsxseg2ei8.v")
object VSXSEG2E16_V   extends Opcode("vsxseg2ei16.v")
object VSXSEG2E32_V   extends Opcode("vsxseg2ei32.v")
object VSXSEG2E64_V   extends Opcode("vsxseg2ei64.v")
object VSXSEG2E128_V  extends Opcode("vsxseg2ei128.v")
object VSXSEG2E256_V  extends Opcode("vsxseg2ei256.v")
object VSXSEG2E512_V  extends Opcode("vsxseg2ei512.v")
object VSXSEG2E1024_V extends Opcode("vsxseg2ei1024.v")

//load nf=3
object VLXSEG3E8_V    extends Opcode("vlxseg3ei8.v")
object VLXSEG3E16_V   extends Opcode("vlxseg3ei16.v")
object VLXSEG3E32_V   extends Opcode("vlxseg3ei32.v")
object VLXSEG3E64_V   extends Opcode("vlxseg3ei64.v")
object VLXSEG3E128_V  extends Opcode("vlxseg3ei128.v")
object VLXSEG3E256_V  extends Opcode("vlxseg3ei256.v")
object VLXSEG3E512_V  extends Opcode("vlxseg3ei512.v")
object VLXSEG3E1024_V extends Opcode("vlxseg3ei1024.v")
//store
object VSXSEG3E8_V    extends Opcode("vsxseg3ei8.v")
object VSXSEG3E16_V   extends Opcode("vsxseg3ei16.v")
object VSXSEG3E32_V   extends Opcode("vsxseg3ei32.v")
object VSXSEG3E64_V   extends Opcode("vsxseg3ei64.v")
object VSXSEG3E128_V  extends Opcode("vsxseg3ei128.v")
object VSXSEG3E256_V  extends Opcode("vsxseg3ei256.v")
object VSXSEG3E512_V  extends Opcode("vsxseg3ei512.v")
object VSXSEG3E1024_V extends Opcode("vsxseg3ei1024.v")

//load nf=4
object VLXSEG4E8_V    extends Opcode("vlxseg4ei8.v")
object VLXSEG4E16_V   extends Opcode("vlxseg4ei16.v")
object VLXSEG4E32_V   extends Opcode("vlxseg4ei32.v")
object VLXSEG4E64_V   extends Opcode("vlxseg4ei64.v")
object VLXSEG4E128_V  extends Opcode("vlxseg4ei128.v")
object VLXSEG4E256_V  extends Opcode("vlxseg4ei256.v")
object VLXSEG4E512_V  extends Opcode("vlxseg4ei512.v")
object VLXSEG4E1024_V extends Opcode("vlxseg4ei1024.v")
//store
object VSXSEG4E8_V    extends Opcode("vsxseg4ei8.v")
object VSXSEG4E16_V   extends Opcode("vsxseg4ei16.v")
object VSXSEG4E32_V   extends Opcode("vsxseg4ei32.v")
object VSXSEG4E64_V   extends Opcode("vsxseg4ei64.v")
object VSXSEG4E128_V  extends Opcode("vsxseg4ei128.v")
object VSXSEG4E256_V  extends Opcode("vsxseg4ei256.v")
object VSXSEG4E512_V  extends Opcode("vsxseg4ei512.v")
object VSXSEG4E1024_V extends Opcode("vsxseg4ei1024.v")

//load nf=5
object VLXSEG5E8_V    extends Opcode("vlxseg5ei8.v")
object VLXSEG5E16_V   extends Opcode("vlxseg5ei16.v")
object VLXSEG5E32_V   extends Opcode("vlxseg5ei32.v")
object VLXSEG5E64_V   extends Opcode("vlxseg5ei64.v")
object VLXSEG5E128_V  extends Opcode("vlxseg5ei128.v")
object VLXSEG5E256_V  extends Opcode("vlxseg5ei256.v")
object VLXSEG5E512_V  extends Opcode("vlxseg5ei512.v")
object VLXSEG5E1024_V extends Opcode("vlxseg5ei1024.v")
//store
object VSXSEG5E8_V    extends Opcode("vsxseg5ei8.v")
object VSXSEG5E16_V   extends Opcode("vsxseg5ei16.v")
object VSXSEG5E32_V   extends Opcode("vsxseg5ei32.v")
object VSXSEG5E64_V   extends Opcode("vsxseg5ei64.v")
object VSXSEG5E128_V  extends Opcode("vsxseg5ei128.v")
object VSXSEG5E256_V  extends Opcode("vsxseg5ei256.v")
object VSXSEG5E512_V  extends Opcode("vsxseg5ei512.v")
object VSXSEG5E1024_V extends Opcode("vsxseg5ei1024.v")

//load nf=6
object VLXSEG6E8_V    extends Opcode("vlxseg6ei8.v")
object VLXSEG6E16_V   extends Opcode("vlxseg6ei16.v")
object VLXSEG6E32_V   extends Opcode("vlxseg6ei32.v")
object VLXSEG6E64_V   extends Opcode("vlxseg6ei64.v")
object VLXSEG6E128_V  extends Opcode("vlxseg6ei128.v")
object VLXSEG6E256_V  extends Opcode("vlxseg6ei256.v")
object VLXSEG6E512_V  extends Opcode("vlxseg6ei512.v")
object VLXSEG6E1024_V extends Opcode("vlxseg6ei1024.v")
//store
object VSXSEG6E8_V    extends Opcode("vsxseg6ei8.v")
object VSXSEG6E16_V   extends Opcode("vsxseg6ei16.v")
object VSXSEG6E32_V   extends Opcode("vsxseg6ei32.v")
object VSXSEG6E64_V   extends Opcode("vsxseg6ei64.v")
object VSXSEG6E128_V  extends Opcode("vsxseg6ei128.v")
object VSXSEG6E256_V  extends Opcode("vsxseg6ei256.v")
object VSXSEG6E512_V  extends Opcode("vsxseg6ei512.v")
object VSXSEG6E1024_V extends Opcode("vsxseg6ei1024.v")

//load nf=7
object VLXSEG7E8_V    extends Opcode("vlxseg7ei8.v")
object VLXSEG7E16_V   extends Opcode("vlxseg7ei16.v")
object VLXSEG7E32_V   extends Opcode("vlxseg7ei32.v")
object VLXSEG7E64_V   extends Opcode("vlxseg7ei64.v")
object VLXSEG7E128_V  extends Opcode("vlxseg7ei128.v")
object VLXSEG7E256_V  extends Opcode("vlxseg7ei256.v")
object VLXSEG7E512_V  extends Opcode("vlxseg7ei512.v")
object VLXSEG7E1024_V extends Opcode("vlxseg7ei1024.v")
//store
object VSXSEG7E8_V    extends Opcode("vsxseg7ei8.v")
object VSXSEG7E16_V   extends Opcode("vsxseg7ei16.v")
object VSXSEG7E32_V   extends Opcode("vsxseg7ei32.v")
object VSXSEG7E64_V   extends Opcode("vsxseg7ei64.v")
object VSXSEG7E128_V  extends Opcode("vsxseg7ei128.v")
object VSXSEG7E256_V  extends Opcode("vsxseg7ei256.v")
object VSXSEG7E512_V  extends Opcode("vsxseg7ei512.v")
object VSXSEG7E1024_V extends Opcode("vsxseg7ei1024.v")

//load nf=8
object VLXSEG8E8_V    extends Opcode("vlxseg8ei8.v")
object VLXSEG8E16_V   extends Opcode("vlxseg8ei16.v")
object VLXSEG8E32_V   extends Opcode("vlxseg8ei32.v")
object VLXSEG8E64_V   extends Opcode("vlxseg8ei64.v")
object VLXSEG8E128_V  extends Opcode("vlxseg8ei128.v")
object VLXSEG8E256_V  extends Opcode("vlxseg8ei256.v")
object VLXSEG8E512_V  extends Opcode("vlxseg8ei512.v")
object VLXSEG8E1024_V extends Opcode("vlxseg8ei1024.v")
//store
object VSXSEG8E8_V    extends Opcode("vsxseg8ei8.v")
object VSXSEG8E16_V   extends Opcode("vsxseg8ei16.v")
object VSXSEG8E32_V   extends Opcode("vsxseg8ei32.v")
object VSXSEG8E64_V   extends Opcode("vsxseg8ei64.v")
object VSXSEG8E128_V  extends Opcode("vsxseg8ei128.v")
object VSXSEG8E256_V  extends Opcode("vsxseg8ei256.v")
object VSXSEG8E512_V  extends Opcode("vsxseg8ei512.v")
object VSXSEG8E1024_V extends Opcode("vsxseg8ei1024.v")

//----------------------------------------
object VLE8FF_V extends Opcode("vle8ff.v")
object VLE16FF_V extends Opcode("vle16ff.v")
object VLE32FF_V extends Opcode("vle32ff.v")
object VLE64FF_V extends Opcode("vle64ff.v")
object VLE128FF_V extends Opcode("vle128ff.v")
object VLE256FF_V extends Opcode("vle256ff.v")
object VLE512FF_V extends Opcode("vle512ff.v")
object VLE1024FF_V extends Opcode("vle1024ff.v")

//----------------------------------------
object VL1R_V extends Opcode("vl1r.v")
//----------------------------------------
object VS1R_V extends Opcode("vs1r.v")

//is_rvv_vamo
object VAMOSWAPEI8_V extends Opcode("vamoswapei8.v")
object VAMOADDEI8_V extends Opcode("vamoaddei8.v")
object VAMOXOREI8_V extends Opcode("vamoxorei8.v")
object VAMOANDEI8_V extends Opcode("vamoandei8.v")
object VAMOOREI8_V extends Opcode("vamoorei8.v")
object VAMOMINEI8_V extends Opcode("vamominei8.v")
object VAMOMAXEI8_V extends Opcode("vamomaxei8.v")
object VAMOMINUEI8_V extends Opcode("vamominuei8.v")
object VAMOMAXUEI8_V extends Opcode("vamomaxuei8.v")

object VAMOSWAPEI16_V extends Opcode("vamoswapei16.v")
object VAMOADDEI16_V extends Opcode("vamoaddei16.v")
object VAMOXOREI16_V extends Opcode("vamoxorei16.v")
object VAMOANDEI16_V extends Opcode("vamoandei16.v")
object VAMOOREI16_V extends Opcode("vamoorei16.v")
object VAMOMINEI16_V extends Opcode("vamominei16.v")
object VAMOMAXEI16_V extends Opcode("vamomaxei16.v")
object VAMOMINUEI16_V extends Opcode("vamominuei16.v")
object VAMOMAXUEI16_V extends Opcode("vamomaxuei16.v")

object VAMOSWAPEI32_V extends Opcode("vamoswapei32.v")
object VAMOADDEI32_V extends Opcode("vamoaddei32.v")
object VAMOXOREI32_V extends Opcode("vamoxorei32.v")
object VAMOANDEI32_V extends Opcode("vamoandei32.v")
object VAMOOREI32_V extends Opcode("vamoorei32.v")
object VAMOMINEI32_V extends Opcode("vamominei32.v")
object VAMOMAXEI32_V extends Opcode("vamomaxei32.v")
object VAMOMINUEI32_V extends Opcode("vamominuei32.v")
object VAMOMAXUEI32_V extends Opcode("vamomaxuei32.v")

object VAMOSWAPEI64_V extends Opcode("vamoswapei64.v")
object VAMOADDEI64_V extends Opcode("vamoaddei64.v")
object VAMOXOREI64_V extends Opcode("vamoxorei64.v")
object VAMOANDEI64_V extends Opcode("vamoandei64.v")
object VAMOOREI64_V extends Opcode("vamoorei64.v")
object VAMOMINEI64_V extends Opcode("vamominei64.v")
object VAMOMAXEI64_V extends Opcode("vamomaxei64.v")
object VAMOMINUEI64_V extends Opcode("vamominuei64.v")
object VAMOMAXUEI64_V extends Opcode("vamomaxuei64.v")

object VAMOSWAPEI128_V extends Opcode("vamoswapei128.v")
object VAMOADDEI128_V extends Opcode("vamoaddei128.v")
object VAMOXOREI128_V extends Opcode("vamoxorei128.v")
object VAMOANDEI128_V extends Opcode("vamoandei128.v")
object VAMOOREI128_V extends Opcode("vamoorei128.v")
object VAMOMINEI128_V extends Opcode("vamominei128.v")
object VAMOMAXEI128_V extends Opcode("vamomaxei128.v")
object VAMOMINUEI128_V extends Opcode("vamominuei128.v")
object VAMOMAXUEI128_V extends Opcode("vamomaxuei128.v")

//is_rvv_vinteger
object VADD_VV extends Opcode("vadd.vv")
object VADD_VX extends Opcode("vadd.vx")
object VADD_VI extends Opcode("vadd.vi")
object VSUB_VV extends Opcode("vsub.vv")
object VSUB_VX extends Opcode("vsub.vx")
object VRSUB_VX extends Opcode("vrsub.vx")
object VRSUB_VI extends Opcode("vrsub.vi")
object VWADDU_VV extends Opcode("vwaddu.vv")
object VWADDU_VX extends Opcode("vwaddu.vx")
object VWSUBU_VV extends Opcode("vwsubu.vv")
object VWSUBU_VX extends Opcode("vwsubu.vx")
object VWADD_VV extends Opcode("vwadd.vv")
object VWADD_VX extends Opcode("vwadd.vx")
object VWSUB_VV extends Opcode("vwsub.vv")
object VWSUB_VX extends Opcode("vwsub.vx")
object VWADDU_WV extends Opcode("vwaddu.wv")
object VWADDU_WX extends Opcode("vwaddu.wx")
object VWSUBU_WV extends Opcode("vwsubu.wv")
object VWSUBU_WX extends Opcode("vwsubu.wx")
object VWADD_WV extends Opcode("vwadd.wv")
object VWADD_WX extends Opcode("vwadd.wx")
object VWSUB_WV extends Opcode("vwsub.wv")
object VWSUB_WX extends Opcode("vwsub.wx")
object VZEXT_VF2 extends Opcode("vzext.vf2")
object VSEXT_VF2 extends Opcode("vsext.vf2")
object VZEXT_VF4 extends Opcode("vzext.vf4")
object VSEXT_VF4 extends Opcode("vsext.vf4")
object VZEXT_VF8 extends Opcode("vzext.vf8")
object VSEXT_VF8 extends Opcode("vsext.vf8")
object VADC_VVM extends Opcode("vadc.vvm")
object VADC_VXM extends Opcode("vadc.vxm")
object VADC_VIM extends Opcode("vadc.vim")
object VMADC_VVM extends Opcode("vmadc.vvm")
object VMADC_VXM extends Opcode("vmadc.vxm")
object VMADC_VIM extends Opcode("vmadc.vim")
object VMADC_VV extends Opcode("vmadc.vv")
object VMADC_VX extends Opcode("vmadc.vx")
object VMADC_VI extends Opcode("vmadc.vi")
object VSBC_VVM extends Opcode("vsbc.vvm")
object VSBC_VXM extends Opcode("vsbc.vxm")
object VMSBC_VVM extends Opcode("vmsbc.vvm")
object VMSBC_VXM extends Opcode("vmsbc.vxm")
object VMSBC_VV extends Opcode("vmsbc.vv")
object VMSBC_VX extends Opcode("vmsbc.vx")
object VAND_VV extends Opcode("vand.vv")
object VAND_VX extends Opcode("vand.vx")
object VAND_VI extends Opcode("vand.vi")
object VOR_VV extends Opcode("vor.vv")
object VOR_VX extends Opcode("vor.vx")
object VOR_VI extends Opcode("vor.vi")
object VXOR_VV extends Opcode("vxor.vv")
object VXOR_VX extends Opcode("vxor.vx")
object VXOR_VI extends Opcode("vxor.vi")
object VSLL_VV extends Opcode("vsll.vv")
object VSLL_VX extends Opcode("vsll.vx")
object VSLL_VI extends Opcode("vsll.vi")
object VSRL_VV extends Opcode("vsrl.vv")
object VSRL_VX extends Opcode("vsrl.vx")
object VSRL_VI extends Opcode("vsrl.vi")
object VSRA_VV extends Opcode("vsra.vv")
object VSRA_VX extends Opcode("vsra.vx")
object VSRA_VI extends Opcode("vsra.vi")
object VNSRL_WV extends Opcode("vnsrl.wv")
object VNSRL_WX extends Opcode("vnsrl.wx")
object VNSRL_WI extends Opcode("vnsrl.wi")
object VNSRA_WV extends Opcode("vnsra.wv")
object VNSRA_WX extends Opcode("vnsra.wx")
object VNSRA_WI extends Opcode("vnsra.wi")
object VMSEQ_VV extends Opcode("vmseq.vv")
object VMSEQ_VX extends Opcode("vmseq.vx")
object VMSEQ_VI extends Opcode("vmseq.vi")
object VMSNE_VV extends Opcode("vmsne.vv")
object VMSNE_VX extends Opcode("vmsne.vx")
object VMSNE_VI extends Opcode("vmsne.vi")
object VMSLTU_VV extends Opcode("vmsltu.vv")
object VMSLTU_VX extends Opcode("vmsltu.vx")
object VMSLT_VV extends Opcode("vmslt.vv")
object VMSLT_VX extends Opcode("vmslt.vx")
object VMSLEU_VV extends Opcode("vmsleu.vv")
object VMSLEU_VX extends Opcode("vmsleu.vx")
object VMSLEU_VI extends Opcode("vmsleu.vi")
object VMSLE_VV extends Opcode("vmsle.vv")
object VMSLE_VX extends Opcode("vmsle.vx")
object VMSLE_VI extends Opcode("vmsle.vi")
object VMSGTU_VX extends Opcode("vmsgtu.vx")
object VMSGTU_VI extends Opcode("vmsgtu.vi")
object VMSGT_VX extends Opcode("vmsgt.vx")
object VMSGT_VI extends Opcode("vmsgt.vi")
object VMINU_VV extends Opcode("vminu.vv")
object VMINU_VX extends Opcode("vminu.vx")
object VMIN_VV extends Opcode("vmin.vv")
object VMIN_VX extends Opcode("vmin.vx")
object VMAXU_VV extends Opcode("vmaxu.vv")
object VMAXU_VX extends Opcode("vmaxu.vx")
object VMAX_VV extends Opcode("vmax.vv")
object VMAX_VX extends Opcode("vmax.vx")
object VMUL_VV extends Opcode("vmul.vv")
object VMUL_VX extends Opcode("vmul.vx")
object VMULH_VV extends Opcode("vmulh.vv")
object VMULH_VX extends Opcode("vmulh.vx")
object VMULHU_VV extends Opcode("vmulhu.vv")
object VMULHU_VX extends Opcode("vmulhu.vx")
object VMULHSU_VV extends Opcode("vmulhsu.vv")
object VMULHSU_VX extends Opcode("vmulhsu.vx")
object VDIVU_VV extends Opcode("vdivu.vv")
object VDIVU_VX extends Opcode("vdivu.vx")
object VDIV_VV extends Opcode("vdiv.vv")
object VDIV_VX extends Opcode("vdiv.vx")
object VREMU_VV extends Opcode("vremu.vv")
object VREMU_VX extends Opcode("vremu.vx")
object VREM_VV extends Opcode("vrem.vv")
object VREM_VX extends Opcode("vrem.vx")
object VWMUL_VV extends Opcode("vwmul.vv")
object VWMUL_VX extends Opcode("vwmul.vx")
object VWMULU_VV extends Opcode("vwmulu.vv")
object VWMULU_VX extends Opcode("vwmulu.vx")
object VWMULSU_VV extends Opcode("vwmulsu.vv")
object VWMULSU_VX extends Opcode("vwmulsu.vx")
object VMACC_VV extends Opcode("vmacc.vv")
object VMACC_VX extends Opcode("vmacc.vx")
object VNMSAC_VV extends Opcode("vnmsac.vv")
object VNMSAC_VX extends Opcode("vnmsac.vx")
object VMADD_VV extends Opcode("vmadd.vv")
object VMADD_VX extends Opcode("vmadd.vx")
object VNMSUB_VV extends Opcode("vnmsub.vv")
object VNMSUB_VX extends Opcode("vnmsub.vx")
object VWMACCU_VV extends Opcode("vwmaccu.vv")
object VWMACCU_VX extends Opcode("vwmaccu.vx")
object VWMACC_VV extends Opcode("vwmacc.vv")
object VWMACC_VX extends Opcode("vwmacc.vx")
object VWMACCSU_VV extends Opcode("vwmaccsu.vv")
object VWMACCSU_VX extends Opcode("vwmaccsu.vx")
object VWMACCUS_VX extends Opcode("vwmaccus.vx")
object VMERGE_VVM extends Opcode("vmerge.vvm")
object VMERGE_VXM extends Opcode("vmerge.vxm")
object VMERGE_VIM extends Opcode("vmerge.vim")
object VMV_V_V extends Opcode("vmv.v.v")
object VMV_V_X extends Opcode("vmv.v.x")
object VMV_V_I extends Opcode("vmv.v.i")

//is_rvv_vfixed
object VSADDU_VV extends Opcode("vsaddu.vv")
object VSADDU_VX extends Opcode("vsaddu.vx")
object VSADDU_VI extends Opcode("vsaddu.vi")
object VSADD_VV extends Opcode("vsadd.vv")
object VSADD_VX extends Opcode("vsadd.vx")
object VSADD_VI extends Opcode("vsadd.vi")
object VSSUBU_VV extends Opcode("vssubu.vv")
object VSSUBU_VX extends Opcode("vssubu.vx")
object VSSUB_VV extends Opcode("vssub.vv")
object VSSUB_VX extends Opcode("vssub.vx")
object VAADDU_VV extends Opcode("vaaddu.vv")
object VAADDU_VX extends Opcode("vaaddu.vx")
object VAADD_VV extends Opcode("vaadd.vv")
object VAADD_VX extends Opcode("vaadd.vx")
object VASUBU_VV extends Opcode("vasubu.vv")
object VASUBU_VX extends Opcode("vasubu.vx")
object VASUB_VV extends Opcode("vasub.vv")
object VASUB_VX extends Opcode("vasub.vx")
object VSMUL_VV extends Opcode("vsmul.vv")
object VSMUL_VX extends Opcode("vsmul.vx")
object VSSRL_VV extends Opcode("vssrl.vv")
object VSSRL_VX extends Opcode("vssrl.vx")
object VSSRL_VI extends Opcode("vssrl.vi")
object VSSRA_VV extends Opcode("vssra.vv")
object VSSRA_VX extends Opcode("vssra.vx")
object VSSRA_VI extends Opcode("vssra.vi")
object VNCLIPU_WV extends Opcode("vnclipu.wv")
object VNCLIPU_WX extends Opcode("vnclipu.wx")
object VNCLIPU_WI extends Opcode("vnclipu.wi")
object VNCLIP_WV extends Opcode("vnclip.wv")
object VNCLIP_WX extends Opcode("vnclip.wx")
object VNCLIP_WI extends Opcode("vnclip.wi")

//is_rvv_vfloat
object VFADD_VV extends Opcode("vfadd.vv")
object VFADD_VF extends Opcode("vfadd.vf")
object VFSUB_VV extends Opcode("vfsub.vv")
object VFSUB_VF extends Opcode("vfsub.vf")
object VFRSUB_VF extends Opcode("vfrsub.vf")
object VFWADD_VV extends Opcode("vfwadd.vv")
object VFWADD_VF extends Opcode("vfwadd.vf")
object VFWSUB_VV extends Opcode("vfwsub.vv")
object VFWSUB_VF extends Opcode("vfwsub.vf")
object VFWADD_WV extends Opcode("vfwadd.wv")
object VFWADD_WF extends Opcode("vfwadd.wf")
object VFWSUB_WV extends Opcode("vfwsub.wv")
object VFWSUB_WF extends Opcode("vfwsub.wf")
object VFMUL_VV extends Opcode("vfmul.vv")
object VFMUL_VF extends Opcode("vfmul.vf")
object VFDIV_VV extends Opcode("vfdiv.vv")
object VFDIV_VF extends Opcode("vfdiv.vf")
object VFRDIV_VF extends Opcode("vfrdiv.vf")
object VFWMUL_VV extends Opcode("vfwmul.vv")
object VFWMUL_VF extends Opcode("vfwmul.vf")
object VFMACC_VV extends Opcode("vfmacc.vv")
object VFMACC_VF extends Opcode("vfmacc.vf")
object VFNMACC_VV extends Opcode("vfnmacc.vv")
object VFNMACC_VF extends Opcode("vfnmacc.vf")
object VFMSAC_VV extends Opcode("vfmsac.vv")
object VFMSAC_VF extends Opcode("vfmsac.vf")
object VFNMSAC_VV extends Opcode("vfnmsac.vv")
object VFNMSAC_VF extends Opcode("vfnmsac.vf")
object VFMADD_VV extends Opcode("vfmadd.vv")
object VFMADD_VF extends Opcode("vfmadd.vf")
object VFNMADD_VV extends Opcode("vfnmadd.vv")
object VFNMADD_VF extends Opcode("vfnmadd.vf")
object VFMSUB_VV extends Opcode("vfmsub.vv")
object VFMSUB_VF extends Opcode("vfmsub.vf")
object VFNMSUB_VV extends Opcode("vfnmsub.vv")
object VFNMSUB_VF extends Opcode("vfnmsub.vf")
object VFWMACC_VV extends Opcode("vfwmacc.vv")
object VFWMACC_VF extends Opcode("vfwmacc.vf")
object VFWNMACC_VV extends Opcode("vfwnmacc.vv")
object VFWNMACC_VF extends Opcode("vfwnmacc.vf")
object VFWMSAC_VV extends Opcode("vfwmsac.vv")
object VFWMSAC_VF extends Opcode("vfwmsac.vf")
object VFWNMSAC_VV extends Opcode("vfwnmsac.vv")
object VFWNMSAC_VF extends Opcode("vfwnmsac.vf")
object VFSQRT_V extends Opcode("vfsqrt.v")
object VFMIN_VV extends Opcode("vfmin.vv")
object VFMIN_VF extends Opcode("vfmin.vf")
object VFMAX_VV extends Opcode("vfmax.vv")
object VFMAX_VF extends Opcode("vfmax.vf")
object VFSGNJ_VV extends Opcode("vfsgnj.vv")
object VFSGNJ_VF extends Opcode("vfsgnj.vf")
object VFSGNJN_VV extends Opcode("vfsgnjn.vv")
object VFSGNJN_VF extends Opcode("vfsgnjn.vf")
object VFSGNJX_VV extends Opcode("vfsgnjx.vv")
object VFSGNJX_VF extends Opcode("vfsgnjx.vf")
object VMFEQ_VV extends Opcode("vmfeq.vv")
object VMFEQ_VF extends Opcode("vmfeq.vf")
object VMFNE_VV extends Opcode("vmfne.vv")
object VMFNE_VF extends Opcode("vmfne.vf")
object VMFLT_VV extends Opcode("vmflt.vv")
object VMFLT_VF extends Opcode("vmflt.vf")
object VMFLE_VV extends Opcode("vmfle.vv")
object VMFLE_VF extends Opcode("vmfle.vf")
object VMFGT_VF extends Opcode("vmfgt.vf")
object VMFGE_VF extends Opcode("vmfge.vf")
object VFCLASS_V extends Opcode("vfclass.v")
object VFMERGE_VFM extends Opcode("vfmerge.vfm")
object VFMV_V_F extends Opcode("vfmv.v.f")
object VFCVT_XU_F_V extends Opcode("vfcvt.xu.f.v")
object VFCVT_X_F_V extends Opcode("vfcvt.x.f.v")
object VFCVT_RTZ_XU_F_V extends Opcode("vfcvt.rtz.xu.f.v")
object VFCVT_RTZ_X_F_V extends Opcode("vfcvt.rtz.x.f.v")
object VFCVT_F_XU_V extends Opcode("vfcvt.f.xu.v")
object VFCVT_F_X_V extends Opcode("vfcvt.f.x.v")
object VFWCVT_XU_F_V extends Opcode("vfwcvt.xu.f.v")
object VFWCVT_X_F_V extends Opcode("vfwcvt.x.f.v")
object VFWCVT_RTZ_XU_F_V extends Opcode("vfwcvt.rtz.xu.f.v")
object VFWCVT_RTZ_X_F_V extends Opcode("vfwcvt.rtz.x.f.v")
object VFWCVT_F_XU_V extends Opcode("vfwcvt.f.xu.v")
object VFWCVT_F_X_V extends Opcode("vfwcvt.f.x.v")
object VFWCVT_F_F_V extends Opcode("vfwcvt.f.f.v")
object VFNCVT_XU_F_W extends Opcode("vfncvt.xu.f.w")
object VFNCVT_X_F_W extends Opcode("vfncvt.x.f.w")
object VFNCVT_RTZ_XU_F_W extends Opcode("vfncvt.rtz.xu.f.w")
object VFNCVT_RTZ_X_F_W extends Opcode("vfncvt.rtz.x.f.w")
object VFNCVT_F_XU_W extends Opcode("vfncvt.f.xu.w")
object VFNCVT_F_X_W extends Opcode("vfncvt.f.x.w")
object VFNCVT_F_F_W extends Opcode("vfncvt.f.f.w")
object VFNCVT_ROD_F_F_W extends Opcode("vfncvt.rod.f.f.w")

//is_rvv_vreduce
object VREDSUM_VS extends Opcode("vredsum.vs")
object VREDMAXU_VS extends Opcode("vredmaxu.vs")
object VREDMAX_VS extends Opcode("vredmax.vs")
object VREDMINU_VS extends Opcode("vredminu.vs")
object VREDMIN_VS extends Opcode("vredmin.vs")
object VREDAND_VS extends Opcode("vredand.vs")
object VREDOR_VS extends Opcode("vredor.vs")
object VREDXOR_VS extends Opcode("vredxor.vs")
object VWREDSUMU_VS extends Opcode("vwredsumu.vs")
object VWREDSUM_VS extends Opcode("vwredsum.vs")
object VFREDOSUM_VS extends Opcode("vfredosum.vs")
object VFREDSUM_VS extends Opcode("vfredsum.vs")
object VFREDMAX_VS extends Opcode("vfredmax.vs")
object VFREDMIN_VS extends Opcode("vfredmin.vs")
object VFWREDOSUM_VS extends Opcode("vfwredosum.vs")
object VFWREDSUM_VS extends Opcode("vfwredsum.vs")

//is_rvv_vmask
object VMAND_MM extends Opcode("vmand.mm")
object VMNAND_MM extends Opcode("vmnand.mm")
object VMANDNOT_MM extends Opcode("vmandnot.mm")
object VMXOR_MM extends Opcode("vmxor.mm")
object VMOR_MM extends Opcode("vmor.mm")
object VMNOR_MM extends Opcode("vmnor.mm")
object VMORNOT_MM extends Opcode("vmornot.mm")
object VMXNOR_MM extends Opcode("vmxnor.mm")
object VPOPC_M extends Opcode("vpopc.m")
object VFIRST_M extends Opcode("vfirst.m")
object VMSBF_M extends Opcode("vmsbf.m")
object VMSIF_M extends Opcode("vmsif.m")
object VMSOF_M extends Opcode("vmsof.m")
object VIOTA_M extends Opcode("viota.m")
object VID_V extends Opcode("vid.v")

//is_rvv_permute
object VMV_X_S extends Opcode("vmv.x.s")
object VMV_S_X extends Opcode("vmv.s.x")
object VFMV_F_S extends Opcode("vfmv.f.s")
object VFMV_S_F extends Opcode("vfmv.s.f")
object VSLIDEUP_VX extends Opcode("vslideup.vx")
object VSLIDEUP_VI extends Opcode("vslideup.vi")
object VSLIDEDOWN_VX extends Opcode("vslidedown.vx")
object VSLIDEDOWN_VI extends Opcode("vslidedown.vi")
object VSLIDE1UP_VX extends Opcode("vslide1up.vx")
object VFSLIDE1UP_VF extends Opcode("vfslide1up.vf")
object VSLIDE1DOWN_VX extends Opcode("vslide1down.vx")
object VFSLIDE1DOWN_VF extends Opcode("vfslide1down.vf")
object VRGATHER_VV extends Opcode("vrgather.vv")
object VRGATHER_VX extends Opcode("vrgather.vx")
object VRGATHER_VI extends Opcode("vrgather.vi")
object VCOMPRESS_VM extends Opcode("vcompress.vm")
object VMV1R_V extends Opcode("vmv1r.v")
object VMV2R_V extends Opcode("vmv2r.v")
object VMV4R_V extends Opcode("vmv4r.v")
object VMV8R_V extends Opcode("vmv8r.v")

// ********************************************************************************************
