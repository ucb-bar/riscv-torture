package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

object HWRegState extends Enumeration
{
  type HWRegState = Value
  val VIS, HID, HID2HID, HID2VIS, VIS2HID, VIS2VIS = Value
}

import HWRegState._
// TODO: This file is getting kind of large
class HWReg(val name: String, val readable: Boolean, val writable: Boolean)
{
  var state = VIS
  var readers = 0
  var backup_state = VIS
  var backup_readers = 0

  def is_state(states: HWRegState*) = states.toList.contains(state)

  def is_visible() = is_state(VIS, VIS2VIS)

  override def toString = name

  def backup() =
  {
    backup_state = state
    backup_readers = readers
  }

  def restore() =
  {
    state = backup_state
    readers = backup_readers
  }
}

object HWReg
{
  // These filters are for allocation purposes
  def filter_read_zero = (hwreg: HWReg) => (hwreg.name == "x0" || hwreg.name == "vx0" || hwreg.name == "x0_shadow")
  def filter_read_any = (hwreg: HWReg) => hwreg.readable
  def filter_read_any_other(other: Reg)(hwreg: HWReg) = (hwreg.readable && hwreg.name != other.hwreg.name)
  def filter_read_visible = (hwreg: HWReg) => hwreg.readable && hwreg.is_state(VIS,VIS2VIS)
  def filter_write_ra = (hwreg: HWReg) => hwreg.name == "x1" && filter_write_visible(hwreg)
  def filter_write_visible = (hwreg: HWReg) => hwreg.writable && hwreg.is_state(VIS,HID)
  def filter_write_hidden = (hwreg: HWReg) => hwreg.writable && (hwreg.is_state(HID) || hwreg.is_state(VIS) && hwreg.readers == 0)
  def filter_write_visible_other(other: Reg)(hwreg: HWReg) = (hwreg.name != other.hwreg.name && hwreg.writable && hwreg.is_state(VIS,HID))
  def filter_write_hidden_other(other: Reg)(hwreg: HWReg) = (hwreg.name != other.hwreg.name && hwreg.writable && (hwreg.is_state(HID) || hwreg.is_state(VIS) && hwreg.readers == 0))
  def filter_write_dep(regs: List[Reg]) =
  {
    if (regs.forall(_.hwreg.is_visible)) filter_write_visible
    else filter_write_hidden
  }
  def filter_write_dep_other(other: Reg, regs: List[Reg]) =
  {
    if (regs.forall(_.hwreg.is_visible)) filter_write_visible_other(other) _
    else filter_write_hidden_other(other) _
  }

  def alloc_read = (hwreg: HWReg) => hwreg.readers += 1
  def alloc_write(visible: Boolean)(hwreg: HWReg) =
  {
    if (hwreg.state == VIS)
    {
      if (visible) hwreg.state = VIS2VIS
      else hwreg.state = VIS2HID
    }
    else if (hwreg.state == HID)
    {
      if (visible) hwreg.state = HID2VIS
      else hwreg.state = HID2HID
    }
    else println("bug in do_write")
  }
  def alloc_write_dep(regs: List[Reg]) = alloc_write(regs.forall(_.hwreg.is_visible)) _

  def free_read = (hwreg: HWReg) => hwreg.readers -= 1
  def free_write = (hwreg: HWReg) =>
  {
    if (hwreg.state == VIS2VIS || hwreg.state == HID2VIS) hwreg.state = VIS
    else if (hwreg.state == VIS2HID || hwreg.state == HID2HID) hwreg.state = HID
    else println("bug in free_write")
  }
}

class HWRegPool
{
  val hwregs = new ArrayBuffer[HWReg]

  def backup() = { hwregs.map((x) => x.backup()) }
  def restore() = { hwregs.map((x) => x.restore()) }
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
    var s = "xreg_init_data:\n"
    for (i <- 0 to 31)
      s += ("reg_x" + i + "_init:\t.dword " + "0x%016x\n" format rand_biased)
    s += "\n"
    s
  }

  def output_regs_data() =
  {
    var s = "xreg_output_data:\n"
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
    var s = "freg_init_data:\n"
    for (i <- 0 to 31)
      s += ("reg_f" + i + "_init:\t.dword " + "0x%016x\n" format rand_biased) // TODO CHANGE RANDOMIZATION
    s += "\n"
    s
  }

  def output_regs_data() =
  {
    var s = "freg_output_data:\n"
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

import HWReg._

class HWRegAllocator
{
  val regs = new ArrayBuffer[Reg]
  var allocated = false

  def reg_fn(hwrp: HWRegPool, filter: (HWReg) => Boolean, alloc: (HWReg) => Unit, free: (HWReg) => Unit) =
  {
    val reg = new RegNeedsAlloc(hwrp, filter, alloc, free)
    regs += reg
    reg
  }

  def reg_read_zero(hwrp: HWRegPool) = { reg_fn(hwrp, filter_read_zero, alloc_read, free_read) }
  def reg_read_any(hwrp: HWRegPool) = { reg_fn(hwrp, filter_read_any, alloc_read, free_read) }
  def reg_read_any_other(hwrp: HWRegPool, other: Reg) = { reg_fn(hwrp, filter_read_any_other(other), alloc_read, free_read) }
  def reg_read_visible(hwrp: HWRegPool) = { reg_fn(hwrp, filter_read_visible, alloc_read, free_read) }
  def reg_write_ra(hwrp: HWRegPool) = { reg_fn(hwrp, filter_write_ra, alloc_write(false), free_write) }
  def reg_write_visible(hwrp: HWRegPool) = { reg_fn(hwrp, filter_write_visible, alloc_write(true), free_write) }
  def reg_write_hidden(hwrp: HWRegPool) = { reg_fn(hwrp, filter_write_hidden, alloc_write(false), free_write) }
  def reg_write(hwrp: HWRegPool, regs: Reg*) = { reg_fn(hwrp, filter_write_dep(regs.toList), alloc_write_dep(regs.toList), free_write) }
  def reg_write_other(hwrp: HWRegPool, other: Reg, regs: Reg*) = { reg_fn(hwrp, filter_write_dep_other(other, regs.toList), alloc_write_dep(regs.toList), free_write) }

  def allocate_regs(): Boolean =
  {
    for (reg <- regs)
    {
      val regna = reg.asInstanceOf[RegNeedsAlloc]
      val candidates = regna.hwrp.hwregs.filter(regna.filter)

      if (candidates.length == 0)
        return false

      val hwreg = rand_pick(candidates)
      regna.alloc(hwreg)
      regna.hwreg = hwreg
    }

    allocated = true

    return true
  }

  def free_regs() =
  {
    for (reg <- regs)
    {
      val regna = reg.asInstanceOf[RegNeedsAlloc]
      regna.free(regna.hwreg)
    }
  }
}
