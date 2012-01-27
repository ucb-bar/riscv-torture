package torture

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object Rand
{
  def rand_range(low: Int, high: Int): Int =
  {
    var span = high - low + 1
    if (low > high) span = low - high + 1
    low + Random.nextInt(span)
  }

  def rand_shamt() = rand_range(0, 63)
  def rand_shamtw() = rand_range(0, 31)
  def rand_imm() = rand_range(-2048, 2047)
  def rand_bigimm() = rand_range(0, 1048575)

  def rand_addr_b(memsize: Int) = rand_range(0, memsize-1)
  def rand_addr_h(memsize: Int) = rand_range(0, memsize-1) & ~1
  def rand_addr_w(memsize: Int) = rand_range(0, memsize-1) & ~3
  def rand_addr_d(memsize: Int) = rand_range(0, memsize-1) & ~7

  def rand_filter(rand: () => Int, filter: (Int) => Boolean) =
  {
    var res = rand()
    while (filter(res)) res = rand()
    res
  }

  def rand_array[T](array: ArrayBuffer[T]) =
  {
    array(rand_range(0, array.length-1))
  }
}
