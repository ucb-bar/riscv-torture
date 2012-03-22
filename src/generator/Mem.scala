package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class Mem(name: Array[Operand], val  size: Int) extends Operand
{
  def this(namelabel: String, size: Int) = this(Array[Operand](Label(namelabel)), size)

  assert(size % 4 == 0, "Memory size must be multiple of 4")

  override def toString = name.mkString("")

  def dumpdata = 
  {
    var s = this.toString + ":\n"
    if(size % 16 == 0)
    {
      for (i <- 0 to (size/8/2 - 1))
        s += "\t.dword 0x%016x, 0x%016x\n" format (rand_dword, rand_dword)
    } else if(size % 8 == 0)
    {
      for (i <- 0 to (size/8 - 1))
        s += "\t.dword 0x%016x\n" format (rand_dword)
    }
    else
    {
      for (i <- 0 to (size/4 - 1))
        s += "\t.word 0x%08x\n" format (rand_word)
    }
    s
  }
}

