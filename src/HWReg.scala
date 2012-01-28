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

  def is_state(states: HWRegState*) =
    states.toList.map(x => {state == x}).reduceLeft(_ || _)

  def is_visible() = is_state(VIS, VIS2VIS)

  override def toString = name
  override def clone =
  {
    val res = new HWReg(name, readable, writable)
    res.state = state
    res.readers = readers
    res
  }
}

object HWReg
{
  def filter_read_zero = (hwreg: HWReg) => hwreg.name == "x0"
  def filter_read_any = (hwreg: HWReg) => hwreg.readable
  def filter_read_visible = (hwreg: HWReg) => hwreg.readable && hwreg.is_state(VIS,VIS2VIS)
  def filter_write_ra = (hwreg: HWReg) => hwreg.name == "x1" && filter_write_visible(hwreg)
  def filter_write_visible = (hwreg: HWReg) => hwreg.writable && hwreg.is_state(VIS,HID)
  def filter_write_hidden = (hwreg: HWReg) => hwreg.writable && (hwreg.is_state(HID) || hwreg.is_state(VIS) && hwreg.readers == 0)
  def filter_write_dep(regs: List[Reg]) =
  {
    if (regs.map(x => {x.hwreg.is_visible()}).reduceLeft(_ && _)) filter_write_visible
    else filter_write_hidden
  }

  def alloc_read = (hwreg: HWReg) => hwreg.readers += 1
  def alloc_write(visible: Boolean) = (hwreg: HWReg) =>
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
  def alloc_write_dep(regs: List[Reg]) = alloc_write(regs.map(x => {x.hwreg.is_visible()}).reduceLeft(_ && _))

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

  def init() =
  {
    hwregs += new HWReg("x0", true, false)
    for (i <- 1 to 31)
      hwregs += new HWReg("x" + i.toString(), true, true)
  }

  override def clone =
  {
    val res = new HWRegPool

    for (hwreg <- hwregs)
      res.hwregs += hwreg.clone

    res
  }
}

import HWReg._

class HWRegAllocator
{
  val regs = new ArrayBuffer[Reg]
  var allocated = false

  def reg_fn(filter: (HWReg) => Boolean, alloc: (HWReg) => Unit, free: (HWReg) => Unit) =
  {
    val reg = new RegNeedsAlloc(filter, alloc, free)
    regs += reg
    reg
  }

  def reg_read_zero() = { reg_fn(filter_read_zero, alloc_read, free_read) }
  def reg_read_any() = { reg_fn(filter_read_any, alloc_read, free_read) }
  def reg_read_visible() = { reg_fn(filter_read_visible, alloc_read, free_read) }
  def reg_write_ra() = { reg_fn(filter_write_ra, alloc_write(false), free_write) }
  def reg_write_visible() = { reg_fn(filter_write_visible, alloc_write(true), free_write) }
  def reg_write_hidden() = { reg_fn(filter_write_hidden, alloc_write(false), free_write) }
  def reg_write(regs: Reg*) = { reg_fn(filter_write_dep(regs.toList), alloc_write_dep(regs.toList), free_write) }

  def allocate_regs(hwrp: HWRegPool, real: Boolean): Boolean =
  {
    var _hwrp = hwrp

    if (!real)
      _hwrp = hwrp.clone

    for (reg <- regs)
    {
      val regna = reg.asInstanceOf[RegNeedsAlloc]
      val candidates = _hwrp.hwregs.filter(regna.filter)

      if (candidates.length == 0)
        return false

      val hwreg = rand_pick(candidates)
      regna.alloc(hwreg)
      regna.hwreg = hwreg
    }

    if (real)
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
