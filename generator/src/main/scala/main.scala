package torture
package generator

import scopt.OptionParser
import java.io.FileWriter
import java.io.FileInputStream
import java.util.Properties
import scala.collection.JavaConversions._

case class Options(
  var outFileName: String = "test",
  var confFileName: String = "config/default.config",
  var isaName: String = "RV64IMAFD",
  var extName: Option[String] = Some("hwacha"))

object Generator extends App
{
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser[Options]("generator/run") {
      opt[String]('C', "config") valueName("<file>") text("config file") action {(s: String, c) => c.copy(confFileName = s)}
      opt[String]('o', "output") valueName("<filename>") text("output filename") action {(s: String, c) => c.copy(outFileName = s)}
      opt[String]('I', "isa") valueName("<isa>") text("RISC-V ISA string") action {(s: String, c) => c.copy(isaName = s)}
      opt[String]('E', "extension") valueName("<extension>") text("RoCC Extension name") action {(s: String, c) => c.copy(extName = Some(s))}
    }
    parser.parse(args, Options()) match {
      case Some(opts) =>
        generate(opts.confFileName, opts.outFileName, opts.isaName, opts.extName)
      case None =>
        System.exit(1) //error message printed by parser
    }
  }

  def generate(confFile: String, outFileName: String, isaName: String, extName: Option[String]): String = {
    val config = new Properties()
    val in = new FileInputStream(confFile)
    config.load(in)
    in.close()
    
    val nseqs   = config.getProperty("torture.generator.nseqs", "1000").toInt
    val memsize = config.getProperty("torture.generator.memsize", "1024").toInt
    val fprnd   = config.getProperty("torture.generator.fprnd", "0").toInt
    val mix     = config.filterKeys(_ contains "torture.generator.mix").map { case (k,v) => (k.split('.')(3), v.toInt) }.asInstanceOf[Map[String,Int]]
    val vec     = config.filterKeys(_ contains "torture.generator.vec").map { case (k,v) => (k.split('.').drop(3).reduce(_+"."+_), v) }.asInstanceOf[Map[String,String]]
    val segment = (config.getProperty("torture.generator.segment", "true").toLowerCase == "true")
    val loop    = (config.getProperty("torture.generator.loop", "true").toLowerCase == "true")
    val loop_size = config.getProperty("torture.generator.loop_size", "256").toInt

    val useVec = mix.filterKeys(List("vec") contains _).values.reduce(_+_) > 0 && extName.exists(_ == "hwacha")
    val useFPU = (mix.filterKeys(List("fgen","fpmem","fax","fdiv") contains _).values.reduce(_+_) > 0) || useVec

    val parseIsa = """RV(\d*)([A-Z]+)""".r
    val parseIsa(xlen, standards) = isaName
    val useAmo  = (standards.contains("A") || standards.contains("G")) &&
                    (config.getProperty("torture.generator.amo", "true").toLowerCase == "true")
    val useMul  = (standards.contains("M") || standards.contains("G")) &&
                    (config.getProperty("torture.generator.mul", "true").toLowerCase == "true")
    val useDiv  = (standards.contains("M") || standards.contains("G")) &&
                    (config.getProperty("torture.generator.divider", "true").toLowerCase == "true")
    val useFPS  = (standards.contains("F") || standards.contains("G")) && useFPU
    val useFPD  = (standards.contains("D") || standards.contains("G")) && useFPU
    val useComp = (standards.contains("C"))

    val use = EnabledInstructions(xlen.toInt, useAmo, useMul, useDiv, useFPS, useFPD, useComp, useVec)
    generate(nseqs, memsize, fprnd, mix, vec, outFileName, segment, loop, loop_size, use)
  }

  def generate(
      nseqs: Int,
      memsize: Int,
      fprnd: Int,
      mix: Map[String,Int],
      veccfg: Map[String,String],
      outFileName: String,
      segment: Boolean,
      loop: Boolean,
      loop_size: Int,
      use: EnabledInstructions): String = {
    assert (mix.values.sum == 100, println("The instruction mix specified in config does not add up to 100%"))
    assert (mix.keys.forall(List("xmem","xbranch","xalu","fgen","fpmem","fax","fdiv","vec") contains _), println("The instruction mix specified in config contains an unknown sequence type name"))
    
    println("Instructions enabled for generation: " + use)

    val vmemsize = veccfg.getOrElse("memsize", "32").toInt
    val vnseq = veccfg.getOrElse("seq", "100").toInt
    val vfnum = veccfg.getOrElse("vf", "10").toInt
    val vecmix = veccfg.filterKeys(_ contains "mix.").map { case (k,v) => (k.split('.')(1), v.toInt) }.asInstanceOf[Map[String,Int]]
    assert (vecmix.values.sum == 100, println("The vector instruction mix specified in config does not add up to 100%"))
    assert (vecmix.keys.forall(List("vmem","valu","vpop","vonly") contains _), println("The vector instruction mix specified in config contains an unknown sequence type name"))

    val prog = new Prog(use, memsize, veccfg, loop)
    ProgSeg.cnt = 0
    SeqVec.cnt = 0
    val s = prog.generate(nseqs, fprnd, mix, veccfg, segment, loop, loop_size)

    val oname = "output/" + outFileName + ".S"
    val fw = new FileWriter(oname)
    fw.write(s)
    fw.close()
    val stats = prog.statistics(nseqs,fprnd,mix,vnseq,vmemsize,vfnum,vecmix,use.amo,use.mul,use.div)
    val sname = "output/" + outFileName + ".stats"
    val fw2 = new FileWriter(sname)
    fw2.write(stats)
    fw2.close()
    oname
  }
}

