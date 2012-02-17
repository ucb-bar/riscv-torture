package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

object HWRegState extends Enumeration
{
  type HWRegState = Value
  val VIS, HID, HID2HID, HID2VIS, VIS2HID, VIS2VIS = Value
}

import HWRegState._

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
  def filter_read_zero = (hwreg: HWReg) => hwreg.name == "x0"
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

class FRegsPool extends HWRegPool
{
  for (i <- 0 to 31)
    hwregs += new HWReg("f" + i.toString(), true, true)
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
