package torture

import scala.collection.mutable.ArrayBuffer
import Rand._

class SeqVec(xregs: HWRegPool, vxregs: HWRegPool, vfregs_s: HWRegPool, vfregs_d: HWRegPool) extends InstSeq
{
  insts += FENCE_V_L()
}
