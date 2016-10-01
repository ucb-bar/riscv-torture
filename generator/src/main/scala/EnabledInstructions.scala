package torture

/** Track which kind of instructions we should be emitting or compiling */
case class EnabledInstructions(
    xlen: Int,
    amo:  Boolean,
    mul:  Boolean,
    div:  Boolean,
    fps:  Boolean,
    fpd:  Boolean,
    comp: Boolean,
    vec:  Boolean) {
  def fpu = fps || fpd
}
