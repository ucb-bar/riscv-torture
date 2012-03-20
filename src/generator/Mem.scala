package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class Mem(name: Array[Operand], newsize: Int) extends Operand
{
  def this(namelabel: String, newsize: Int) = this(Array[Operand](Label(namelabel)), newsize)

  val size = newsize

  override def toString = name.mkString("")

  def dumpdata = 
  {
    var s = this.toString + ":\n"
    for (i <- 0 to (size/8/2 - 1))
      s += "\t.dword 0x%016x, 0x%016x\n" format (rand_dword, rand_dword)
    s
  }
}

