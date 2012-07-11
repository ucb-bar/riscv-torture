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

trait ScalarRegPool extends HWRegPool
{
  val name: String
  val regname: String
  val ldinst: String
  val stinst: String
  
  def init_regs() =
  {
    var s = name + "_init:\n"
    s += "\tla x31, " + name + "_init_data\n"
    for (i <- 0 to hwregs.length-1)
      s += "\t" + ldinst + " " + hwregs(i) + ", " + 8*i + "(x31)\n"
    s += "\n"
    s
  }
  
  def save_regs() =
  {
    var s = "\tla x1, " + name + "_output_data\n"
    for (i <- 0 to hwregs.length-1)
      if (hwregs(i).is_visible)
        s += "\t" + stinst + " " + hwregs(i) + ", " + 8*i + "(x1)\n"
    s += "\n"
    s
  }
  
  def init_regs_data() = 
  {
    var s = "\t.align 8\n"
    s += name + "_init_data:\n"
    for (i <- 0 to hwregs.length-1)
      s += (regname + i + "_init:\t.dword " + "0x%016x\n" format rand_biased) //Change randomization for FRegs
    s += "\n"
    s
  }
  
  def output_regs_data() =
  {
    var s = "\t.align 8\n"
    s += name + "_output_data:\n"
    for (i <- 0 to hwregs.length-1)
      s += (regname + i + "_output:\t.dword 0x%016x\n" format rand_dword)
    s += "\n"
    s
  }
}

trait PoolsMaster extends HWRegPool
{
  val regpools: ArrayBuffer[HWRegPool]
  override val hwregs = new ArrayBuffer[HWReg] //Override this in subclasses
  override def is_fully_unallocated = regpools.forall(_.is_fully_unallocated)
  override def size = regpools.map(_.size).sum
  def extract_pools() =
  {
    regpools
  }
  override def backup() =
  {
    regpools.map(_.backup()).flatten
  }
  override def restore() =
  {
    regpools.map(_.restore()).flatten
  }
}

class XRegsPool extends ScalarRegPool
{
  val (name, regname, ldinst, stinst) = ("xreg", "reg_x", "ld", "sd")
  
  hwregs += new HWReg("x0", true, false)
  for (i <- 1 to 31)
    hwregs += new HWReg("x" + i.toString(), true, true)
    
  override def save_regs() =
  {
    hwregs(1).state = HID
    super.save_regs()
  }
}

class FRegsMaster extends ScalarRegPool with PoolsMaster
{
  val (name,regname,ldinst,stinst) = ("freg","reg_f","fld","fsd") // and flw and fsw
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
  
  val s_regpool = new FRegsPool(s_reg_num.toArray)
  val d_regpool = new FRegsPool(d_reg_num.toArray)
  val regpools = ArrayBuffer(s_regpool.asInstanceOf[HWRegPool],
                 d_regpool.asInstanceOf[HWRegPool])
  override val hwregs = regpools.map(_.hwregs).flatten
  
  override def init_regs() = //Wrapper function
  {
    var s = "freg_init:\n"+"freg_s_init:\n"+"\tla x1, freg_init_data\n"
    for ((i, curreg) <- s_reg_num.zip(s_regpool.hwregs))
      s += "\tflw" + " " + curreg + ", " + 8*i + "(x1)\n"
    s += "\n"+"freg_d_init:\n"+"\tla x1, freg_init_data\n"
    for ((i, curreg) <- d_reg_num.zip(d_regpool.hwregs))
      s += "\tfld" + " " + curreg + ", " + 8*i + "(x1)\n"
    s += "\n\n"
    s
  } 
  override def save_regs() = //Wrapper function
  {
    var s = "freg_save:\n"+"\tla x1, freg_output_data\n"
    for ((i, curreg) <- s_reg_num.zip(s_regpool.hwregs))
      if (curreg.is_visible)
        s += "\tfsw" + " " + curreg + ", " + 8*i + "(x1)\n"
    s += "\n"+"\tla x1, freg_output_data\n"
    for ((i, curreg) <- d_reg_num.zip(d_regpool.hwregs))
      if (curreg.is_visible)
        s += "\tfsd" + " " + curreg + ", " + 8*i + "(x1)\n"
    s += "\n\n"
    s
  }
}

class FRegsPool(reg_nums: Array[Int] = (0 to 31).toArray) extends HWRegPool
{
  for (i <- reg_nums)
    hwregs += new HWReg("f" + i.toString(), true, true)
}

class VRegsMaster(num_xregs: Int, num_fregs: Int) extends PoolsMaster
{
  assert(num_xregs >= 5, "For VRegMaster, num_xregs >=5 enforced")
  assert(num_fregs >= 8, "For VRegMaster, num_fregs >=8 enforced")

  // Randomly segregate the fregs
  val fs_reg_num = new ArrayBuffer[Int]
  val fd_reg_num = new ArrayBuffer[Int]

  for (n <- 0 to num_fregs-1)
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
  val regpools = ArrayBuffer(x_regpool.asInstanceOf[HWRegPool], 
      fs_regpool.asInstanceOf[HWRegPool], fd_regpool.asInstanceOf[HWRegPool])  
  override val hwregs = regpools.map(_.hwregs).flatten
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
