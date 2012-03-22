package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

import HWRegState._
class HWRegPool
{
  val hwregs = new ArrayBuffer[HWReg]

  def backup() = { hwregs.map((x) => x.backup()) }
  def restore() = { hwregs.map((x) => x.restore()) }

  def is_fully_unallocated = hwregs.forall(_.is_unallocated)
  def size = hwregs.length
}

class XRegsPool extends HWRegPool
{
  hwregs += new HWReg("x0", true, false)
  for (i <- 1 to 31)
    hwregs += new HWReg("x" + i.toString(), true, true)

  def init_regs() =
  {
    var s = "xreg_init:\n"
    s += "\tla x31, xreg_init_data\n"
    for (i <- 0 to 31)
      s += "\tld " + hwregs(i) + ", " + 8*i + "(x31)\n"
    s += "\n"
    s
  }

  def save_regs() =
  {
    val r = rand_range(1, 31)
    var s = "\tla x" + r + ", xreg_output_data\n"
    hwregs(r).state = HID
    for (i <- 0 to 31)
      if (hwregs(i).is_visible)
        s += "\tsd " + hwregs(i) + ", " + 8*i + "(" + hwregs(r) + ")\n"
    s += "\n"
    s
  }

  def init_regs_data() =
  {
    var s = "\t.align 8\n"
    s += "xreg_init_data:\n"
    for (i <- 0 to 31)
      s += ("reg_x" + i + "_init:\t.dword " + "0x%016x\n" format rand_biased)
    s += "\n"
    s
  }

  def output_regs_data() =
  {  
    var s = "\t.align 8\n"
    s += "xreg_output_data:\n"
    for (i <- 0 to 31)
      s += "reg_x" + i + "_output:\t.dword 0x%016x\n" format rand_dword
    s += "\n"
    s
  }
}

class FRegsMaster()
{
  val s_reg_num = new ArrayBuffer[Int]
  val d_reg_num = new ArrayBuffer[Int]

  for (n <- 0 to 31)
    if(rand_range(0, 1) == 0) s_reg_num += n
    else d_reg_num += n

  // Ensure each pool has at least 5 members
  while(s_reg_num.length < 5)
  {
    val mv_n = rand_pick(d_reg_num)
    d_reg_num -= mv_n
    s_reg_num += mv_n
  }
  
  while(d_reg_num.length < 5)
  {
    val mv_n = rand_pick(s_reg_num)
    s_reg_num -= mv_n
    d_reg_num += mv_n
  }
  
  val s_regpool = new FRegsPool(s_reg_num.toArray, "freg_s", "flw", "fsw")
  val d_regpool = new FRegsPool(d_reg_num.toArray, "freg_d", "fld", "fsd")
  
  def extract_pools() =
  {
    (s_regpool,d_regpool)
  }
  def backup() = // Wrapper function
  {
    s_regpool.backup()
    d_regpool.backup()
  }
  def restore() = // Wrapper function
  {
    s_regpool.restore()
    d_regpool.restore()
  }

  // NOTE: This must be called BEFORE scalar core init since x1 needed for memory addressing
  def init_regs() = // Wrapper function
  {
    var s = "freg_init:\n"
    s += s_regpool.init_regs()
    s += d_regpool.init_regs()
    s += "\n"
    s
  }
  
  // NOTE: This must be called AFTER scalar core init since x1 needed for memory addressing
  def save_regs() = // Wrapper function
  {
    var s = "freg_save:\n"
    s += s_regpool.save_regs()
    s += d_regpool.save_regs()
    s += "\n"
    s
  }
  
  def init_regs_data() =
  {
    var s = "\t.align 8\n"
    s += "freg_init_data:\n"
    for (i <- 0 to 31)
      s += ("reg_f" + i + "_init:\t.dword " + "0x%016x\n" format rand_biased) // TODO CHANGE RANDOMIZATION
    s += "\n"
    s
  }

  def output_regs_data() =
  {
    var s = "\t.align 8\n"
    s += "freg_output_data:\n"
    for (i <- 0 to 31)
      s += ("reg_f" + i + "_output:\t.dword 0x%016x\n" format rand_dword)
    s += "\n"
    s
  }
}

class FRegsPool(reg_nums: Array[Int] = (0 to 31).toArray, name: String = "freg_d", inst_ld: String = "fld", inst_sd: String = "fsd") extends HWRegPool
{
  for (i <- reg_nums)
    hwregs += new HWReg("f" + i.toString(), true, true)
  
  // NOTE: This must be called BEFORE scalar core init since x1 needed for memory addressing
  def init_regs() =
  {
    var s = name + "_init:\n"
    s += "\tla x1, freg_init_data\n"
    for ((i, curreg) <- reg_nums.zip(hwregs))
      s += "\t" + inst_ld + " " + curreg + ", " + 8*i + "(x1)\n"
    s += "\n"
    s
  }

  // NOTE: This must be called AFTER scalar core init since x1 needed for memory addressing
  def save_regs() =
  {
    var s = "\tla x1, freg_output_data\n"
    // NOTE: x31 in XRegsPool 'should' be declared hidden here; however, it is assumed XRegs has already saved.
    //        Thus, the issue is moot.
    for ((i, curreg) <- reg_nums.zip(hwregs))
      if (curreg.is_visible)
        s += "\t" + inst_sd + " " + curreg + ", " + 8*i + "(x1)\n"
    s += "\n"
    s
  }
}

class VRegsMaster(num_xregs: Int, num_fregs: Int)
{
  assert(num_xregs >= 5, "For VRegMaster, num_xregs >=5 enforced")
  assert(num_fregs >= 8, "For VRegMaster, num_fregs >=8 enforced")

  // Randomly segregate the fregs
  val fs_reg_num = new ArrayBuffer[Int]
  val fd_reg_num = new ArrayBuffer[Int]

  for (n <- 0 to num_fregs)
    if(rand_range(0, 1) == 0) fs_reg_num += n
    else fd_reg_num += n

  // Ensure each pool has at least 4 members
  while(fs_reg_num.length < 4)
  {
    val mv_n = rand_pick(fd_reg_num)
    fd_reg_num -= mv_n
    fs_reg_num += mv_n
  }
  
  while(fd_reg_num.length < 4)
  {
    val mv_n = rand_pick(fs_reg_num)
    fs_reg_num -= mv_n
    fd_reg_num += mv_n
  }

  val x_reg_num = (1 to (num_xregs-1)) // reg 0 will always be setup since special
  
  val x_regpool  = new VXRegsPool(x_reg_num.toArray)
  val fs_regpool = new VFRegsPool(fs_reg_num.toArray)
  val fd_regpool = new VFRegsPool(fd_reg_num.toArray)
  
  def extract_pools() =
  {
    (x_regpool, fs_regpool, fd_regpool)
  }
  def backup() = // Wrapper function
  {
    x_regpool.backup()
    fs_regpool.backup()
    fd_regpool.backup()
  }
  def restore() = // Wrapper function
  {
    x_regpool.restore()
    fs_regpool.restore()
    fd_regpool.restore()
  }
}

class VXRegsPool(reg_nums: Array[Int] = (1 to 31).toArray) extends HWRegPool
{
  hwregs += new HWReg("vx0", true, false)
  for (i <- reg_nums)
    hwregs += new HWReg("vx" + i.toString(), true, true)
}

class VFRegsPool(reg_nums: Array[Int] = (0 to 31).toArray) extends HWRegPool
{
  for (i <- reg_nums)
    hwregs += new HWReg("vf" + i.toString(), true, true)
}
