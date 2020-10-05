package torture

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.util.Calendar
object Rand
{
  def rand_word: Int = Random.nextInt
  def rand_dword: Long = Random.nextLong

  def rand_range(low: Int, high: Int): Int =
  {
    var span = high - low + 1
    if (low > high) span = low - high + 1
    low + Random.nextInt(span)
  }

  def rand_range_multiple8(low: Int, high: Int): Int =
  {
    var span = high - low + 1
    if (low > high) 
      span = low - high + 1
    
    low + ((Random.nextInt(span) >> 5) << 5)
  }
 
//============================================Riscv Functionality=================================================


def dest_n(lmul : String): Int =
  {  
  var temp = 0
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2") {temp = Random.nextInt(32)}
  else if(lmul=="2") {temp = Random.nextInt(16) * 2}
  else if(lmul=="4") {temp = Random.nextInt(8) * 4}
  else if(lmul=="8") {temp = Random.nextInt(4) * 8}
  return temp  
  }

def src1_n(lmul : String, dest_n : Int): Int =
  {  
  var temp = 0
  
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2") {temp = Random.nextInt(32)}
  else if(lmul=="2") {temp = Random.nextInt(16) * 2}
  else if(lmul=="4") {temp = Random.nextInt(8) * 4}
  else if(lmul=="8") {temp = Random.nextInt(4) * 8}
  
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2"){ while ((temp == dest_n)){temp = Random.nextInt(32)}}  
  else if(lmul=="2"){ while ((temp >= dest_n && temp <= dest_n + 1)){temp = (Random.nextInt(16)*2)}}
  else if(lmul=="4"){ while ((temp >= dest_n && temp <= dest_n + 3)){temp = (Random.nextInt(8)*4)}}
  else if(lmul=="8"){ while ((temp >= dest_n && temp <= dest_n + 7)){temp = (Random.nextInt(4)*8)}}  
  return temp
  
  }

def src2_n(lmul : String, dest_n : Int, src1 : Int): Int =
  {  
  var temp = 0
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2") {temp = Random.nextInt(32)}
  else if(lmul=="2") {temp = Random.nextInt(16) * 2}
  else if(lmul=="4") {temp = Random.nextInt(8) * 4}
  else if(lmul=="8") {temp = Random.nextInt(4) * 8}
  
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2"){ while ((temp == dest_n) || temp == src1){temp = Random.nextInt(32)}}  
  else if(lmul=="2"){ while ((temp >= dest_n && temp <= dest_n + 1) || temp == src1){temp = (Random.nextInt(16)*2)}}
  else if(lmul=="4"){ while ((temp >= dest_n && temp <= dest_n + 3) || temp == src1){temp = (Random.nextInt(8)*4)}}
  else if(lmul=="8"){ while ((temp >= dest_n && temp <= dest_n + 7) || temp == src1){temp = (Random.nextInt(4)*8)}}  
  return temp
  
  }

def dest_w(lmul : String): Int =
  {  
  var temp = 0
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2") {temp = Random.nextInt(16) * 2}
  else if(lmul=="2") {temp = Random.nextInt(8) * 4}
  else if(lmul=="4") {temp = Random.nextInt(4) * 8}
  else if(lmul=="8") {temp = Random.nextInt(2) * 16}
  return temp  
  }

def src1_w(lmul : String, dest_w : Int): Int =
  {  
  var temp = 0
  
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2") {temp = Random.nextInt(32)}
  else if(lmul=="2") {temp = Random.nextInt(16) * 2}
  else if(lmul=="4") {temp = Random.nextInt(8) * 4}
  else if(lmul=="8") {temp = Random.nextInt(4) * 8}
     
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2"){ while ((temp >= dest_w && temp <= dest_w + 1)){temp = Random.nextInt(32)}}  
  else if(lmul=="2"){ while ((temp >= dest_w && temp <= dest_w + 3)){temp = (Random.nextInt(16)*2)}}
  else if(lmul=="4"){ while ((temp >= dest_w && temp <= dest_w + 7)){temp = (Random.nextInt(8)*4)}}
  else if(lmul=="8"){ while ((temp >= dest_w && temp <= dest_w + 15)){temp = (Random.nextInt(4)*8)}}  
  return temp
  }

def src2_w(lmul : String, dest_w : Int, src1 : Int): Int =
  {  
  var temp = 0
  
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2") {temp = Random.nextInt(32)}
  else if(lmul=="2") {temp = Random.nextInt(16) * 2}
  else if(lmul=="4") {temp = Random.nextInt(8) * 4}
  else if(lmul=="8") {temp = Random.nextInt(4) * 8}
    
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2"){ while ((temp >= dest_w && temp <= dest_w + 1) || temp == src1){temp = Random.nextInt(32)}}  
  else if(lmul=="2"){ while ((temp >= dest_w && temp <= dest_w + 3) || temp == src1){temp = (Random.nextInt(16)*2)}}
  else if(lmul=="4"){ while ((temp >= dest_w && temp <= dest_w + 7) || temp == src1){temp = (Random.nextInt(8)*4)}}
  else if(lmul=="8"){ while ((temp >= dest_w && temp <= dest_w + 15) || temp == src1){temp = (Random.nextInt(4)*8)}}  
  return temp
  }
  
  def dest_wo(lmul : String, dest_w : Int, src1 : Int): Int =
  {  
    var temp = 0
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2") {temp = Random.nextInt(32)}
  else if(lmul=="2") {temp = Random.nextInt(16) * 2}
  else if(lmul=="4") {temp = Random.nextInt(8) * 4}
  else if(lmul=="8") {temp = Random.nextInt(4) * 8}
       
       if(lmul == "1" || lmul == "f8" || lmul == "f4" || lmul == "f2"){ while ((temp >= dest_w && temp <= dest_w + 1) || (temp >= src1 && temp <= src1 +1)){temp = (Random.nextInt(16)*2)}}  
  else if(lmul=="2"){ while ((temp >= dest_w && temp <= dest_w + 3) || (temp >= src1 && temp <= src1 +3)){temp = (Random.nextInt(8)*4)}}
  else if(lmul=="4"){ while ((temp >= dest_w && temp <= dest_w + 7) || (temp >= src1 && temp <= src1 +7)){temp = (Random.nextInt(4)*8)}}
  else if(lmul=="8"){ while ((temp >= dest_w && temp <= dest_w + 15) || (temp >= src1 && temp <= src1 +15)){temp = (Random.nextInt(2)*16)}}  
  return temp
  }

//=================================================================================================================
  
  def rand_shamt() = rand_range(0, 63)
  def rand_rvv15() = rand_range(-16, 15)
  def rand_rvv31() = rand_range(0, 31)
  def rand_rvv() = rand_range(0, 15)
  def rand_shamtw() = rand_range(0, 31)
  def rand_seglen() = rand_range(0, 7)
  def rand_imm() = rand_range(-2048, 2047)
  def rand_bigimm() = rand_range(0, 1048575)
  def rand_addr_b(memsize: Int) = rand_range(0, memsize-1)
  def rand_addr_h(memsize: Int) = rand_range(0, memsize-1) & ~1
  def rand_addr_w(memsize: Int) = rand_range(0, memsize-1) & ~3
  def rand_addr_d(memsize: Int) = rand_range(0, memsize-1) & ~7

  def rand_addr_b_rvv(memsize: Int) = rand_range_multiple8(0, memsize-1)
  def rand_addr_h_rvv(memsize: Int) = rand_range_multiple8(0, memsize-1) & ~1
  def rand_addr_w_rvv(memsize: Int) = rand_range_multiple8(0, memsize-1) & ~3
  def rand_addr_d_rvv(memsize: Int) = rand_range_multiple8(0, memsize-1) & ~7

  def rand_filter(rand: () => Int, filter: (Int) => Boolean) =
  {
    var res = rand()
    while (!filter(res)) res = rand()
    res
  }

 
 def rand_pick[T](array: ArrayBuffer[T]) =
  {
    array(rand_range(0, array.length-1))
  }

  def rand_permute[T](array: ArrayBuffer[T]) =
  {
    for (i <- 0 to array.length-1)
    {
      val j = rand_range(0, array.length-1)
      val t = array(i)
      array(i) = array(j)
      array(j) = t
    }
  }

  def rand_biased: Long =
  {
    val value = rand_dword
    val s = rand_range(0, 17)

    if (s < 9)
    {
      val small = rand_range(0, 9).toLong

      s match
      {
        // return a value with a single bit set
        case 0 => (1 << value & 63)
        case 1 => (1 << value & 63)
        // return a valueue with a single bit clear
        case 2 => ~(1 << value & 63)
        case 3 => ~(1 << value & 63)
        // return a small integer around zero
        case 4 => small
        // return a very large/very small 8b signed number
        case 5 => ((0x80L + small) << 56) >> 56
        // return a very large/very small 16b signed number
        case 6 => ((0x8000L + small) << 48) >> 48
        // return a very large/very small 32b signed number
        case 7 => ((0x80000000L + small) << 32) >> 32
        // return a very large/very small 64b signed number
        case 8 => 0x800000000000000L + small
      }
    }
    else
    {
      value
    }
  }
}
