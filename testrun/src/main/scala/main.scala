package torture
package testrun

import scopt.OptionParser
import scala.sys.process._
import scala.collection.mutable.ArrayBuffer
import java.io.FileWriter
import java.util.Properties
import java.io.FileInputStream
import java.util.Scanner
import java.io.File
import scala.util.Random

case class Options(var testAsmName: Option[String] = None,
  var testBinName: Option[String] = None,
  var cSimPath: Option[String] = None,
  var rtlSimPath: Option[String] = None,
  var seekOutFailure: Boolean = false,
  var output: Boolean = false,
  var dumpWaveform: Boolean = false,
  var confFileName: String = "config/default.config",
  var isaName: String = "RV64IMAFD",
  var extName: Option[String] = None)

abstract sealed class Result
case object Failed extends Result
case object Mismatched extends Result
case object Matched extends Result

object TestRunner extends App
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
  //TODO: need to make the class Options above look like the new website should get us to remove the options!
    val parser = new OptionParser[Options]("testrun/run") {
      opt[String]('C', "config") valueName("<file>") text("config file") action {(s: String, c) => c.copy(confFileName = s)}
      opt[String]('a', "asm") valueName("<file>") text("input ASM file") action {(s: String, c) => c.copy(testAsmName = Some(s))}
      opt[String]('c', "csim") valueName("<file>") text("C simulator") action {(s: String, c) => c.copy(cSimPath = Some(s))}
      opt[String]('r', "rtlsim") valueName("<file>") text("RTL simulator") action {(s: String, c) => c.copy(rtlSimPath = Some(s))}
      opt[Unit]("seek") abbr("s") text("Seek for failing pseg") action {(_, c) => c.copy(seekOutFailure = true)}
      opt[Unit]("output") abbr("o") text("Write verbose output of simulators to file") action {(_, c) => c.copy(output = true)}
      opt[Unit]("dumpwaveform") abbr("dump") text("Create a vcd from csim or a vpd from vsim") action {(_, c) => c.copy(dumpWaveform= true)}
      opt[String]('I', "isa") valueName("<isa>") text("RISC-V ISA string") action {(s: String, c) => c.copy(isaName = s)}
      opt[String]('E', "extension") valueName("<extension>") text("RoCC Extension name") action {(s: String, c) => c.copy(extName = Some(s))}
    }
    parser.parse(args, Options()) match {
      case Some(opts) =>
        testrun(opts.testAsmName, opts.cSimPath, opts.rtlSimPath, opts.seekOutFailure, opts.output, opts.dumpWaveform, opts.confFileName, opts.isaName, opts.extName) 
      case None =>
        System.exit(1) // error message printed by parser
    }
  }

  //                 sim      bin     debug    output   dump        sig    
  type Simulation = (String, (String, Boolean, Boolean, Boolean) => String)

  def testrun(testAsmName:  Option[String], 
              cSimPath:     Option[String], 
              rtlSimPath:   Option[String], 
              doSeek:       Boolean,
              output:       Boolean,
              dumpWaveform: Boolean,
              confFileName: String,
              isaName:      String,
              extNameIn:    Option[String] = None): (Boolean, Option[Seq[String]]) =
  {
    val config = new Properties()
    val configin = new FileInputStream(confFileName)
    config.load(configin)
    configin.close()

    val maxCycles = config.getProperty("torture.testrun.maxcycles", "10000000").toInt
    val virtualMode = (config.getProperty("torture.testrun.virtual", "false").toLowerCase == "true")
    val dump = (config.getProperty("torture.testrun.dump", "false").toLowerCase == "true")
    val seek = (config.getProperty("torture.testrun.seek", "true").toLowerCase == "true")
    val extName = if (config.getProperty("torture.testrun.vec", "true").toLowerCase == "true") Some("hwacha") else extNameIn

    // Figure out which binary file to test
    val finalBinName = testAsmName match {
      case Some(asmName) => compileAsmToBin(asmName, isaName, extName, virtualMode)
      case None => {
        val gen = generator.Generator
        val newAsmName = gen.generate(confFileName, "test", isaName, extName)
        compileAsmToBin(newAsmName, isaName, extName, virtualMode)
      }
    }

    // Add the simulators that should be tested
    val golden = ("spike", runIsaSim(isaName, extName) _ )
    val simulations = new ArrayBuffer[Simulation]
    cSimPath foreach { p => simulations += (("csim", runCSim(p, isaName, extName, maxCycles) _ )) }
    rtlSimPath foreach { p => simulations += (("rtlsim", runRtlSim(p, isaName, extName, maxCycles) _ )) }

    // Test the simulators on the complete binary
    finalBinName match {
      case Some(binName) => {
        val res = runSimulations(golden, simulations)(binName, false, output, dumpWaveform || dump)
        val fail_names = res.filter(_._2 == Failed).map(_._1._1.toString)
        val mism_names = res.filter(_._2 == Mismatched).map(_._1._1.toString)
        val bad_sims   = res.filter(_._2 != Matched).map(_._1)
        if (bad_sims.length > 0) {
          println("///////////////////////////////////////////////////////")
          println("//  Simulation failed for " + binName + ":")
          fail_names.foreach(n => println("\t"+n))
          println("//  Mismatched sigs for " + binName + ":")
          mism_names.foreach(n => println("\t"+n))
          println("//  Rerunning in Debug mode")
          // run debug for failed/mismatched
          println("///////////////////////////////////////////////////////")
          if(doSeek || seek) {
            val failName = seekOutFailureBinary(golden, bad_sims)(binName, true, output, dumpWaveform || dump)(isaName, extName, virtualMode)
            println("///////////////////////////////////////////////////////")
            println("//  Failing pseg identified. Binary at " + failName)
            println("///////////////////////////////////////////////////////")
            dumpFromBin(failName)
            (true, Some(failName.split("/")))
          } else {
            dumpFromBin(binName)
            (true, Some(binName.split("/")))
          }
        } else {
          println("///////////////////////////////////////////////////////")
          println("//  All signatures match for " + binName)
          println("///////////////////////////////////////////////////////")
          (false, Some(binName.split("/")))
        }
      }
      case None => {
        println("Error: ASM file could not be compiled or generated.")
        (false, None)
      }
    }
  }

  def compileAsmToBin(asmFileName: String, isaName: String, extName: Option[String], virtualMode: Boolean): Option[String] = {  
    assert(asmFileName.endsWith(".S"), println("Filename does not end in .S"))
    val binFileName = asmFileName.dropRight(2)
    val ext = if(extName.isDefined) "X" + extName.get else ""
    val march = isaName + ext
    val process = if (virtualMode) {
      println("Virtual mode")
      val entropy = (new Random()).nextLong()
      println("entropy: " + entropy)
      s"riscv64-unknown-elf-gcc -nostdlib -nostartfiles -Wa,-march=$march -DENTROPY=$entropy" +
        s" -std=gnu99 -O2 -I./env/v -T./env/v/link.ld ./env/v/entry.S ./env/v/vm.c $asmFileName -lc -o $binFileName"
    } else {
      println("Physical mode")
      s"riscv64-unknown-elf-gcc -nostdlib -nostartfiles -Wa,-march=$march -I./env/p -T./env/p/link.ld $asmFileName -o $binFileName"
    }
    println(process)
    val pb = Process(process)
    val exitCode = pb.!
    if (exitCode == 0) Some(binFileName) else None
  }

  def dumpFromBin(binFileName: String): Option[String] = {
    val dumpFileName = binFileName + ".dump"
    val pd = Process(s"riscv64-unknown-elf-objdump --disassemble-all --section=.text --section=.data --section=.bss $binFileName")
    val dump = pd.!!
    val fw = new FileWriter(dumpFileName)
    fw.write(dump)
    fw.close()
    Some(dumpFileName)
  }

  def generateHexFromBin(binFileName: String) = {
    import java.io.File
    // Determine binary size
    val binfile = new File(binFileName)
    
    val hexlines = 2 << (Math.log(binfile.length >>> 4)/Math.log(2)+1).toInt

    val hexFileName = binFileName + ".hex"
    val pd = Process(s"elf2hex 16 $hexlines $binFileName")
    val hexdump = pd.!!

    val fw = new FileWriter(hexFileName)
    fw.write(hexdump)
    fw.close()

    hexFileName
  }

  def runSim(sim: String, simargs: Seq[String], signature: String, output: Boolean, outName: String, args: Seq[String], invokebin: String, extName: Option[String]): String = {
    val cmd = Seq(sim) ++ simargs ++ Seq("+signature="+signature) ++ args ++ Seq(invokebin)
    println("running:" + cmd.mkString(" "))
    if(output) {
      var fw = new FileWriter(outName+".raw")
      cmd ! ProcessLogger(
         {s => fw.write(s+"\n") },
         {s => fw.write(s+"\n") })
      fw.close()
      val fwd = new FileWriter(outName)
      Process(Seq("cat",outName+".raw")) #| Process("spike-dasm" + extName.map(" --extension="+_)) ! ProcessLogger(
         {s => fwd.write(s+"\n") },
         {s => fwd.write(s+"\n") })
      fwd.close()
      new File(outName+".raw").delete()
    } else {
      cmd !!
    }
    val sigFile = new File(signature)
    if(!sigFile.exists()) ""
    else new Scanner(sigFile).useDelimiter("\\Z").next()
  }

  def runCSim(sim: String, isaName: String, extName: Option[String], maxCycles: Int)(bin: String, debug: Boolean, output: Boolean, dump: Boolean): String = {
    val outputArgs = if(output) Seq("+verbose") else Seq()
    val dumpArgs = if(dump && debug) Seq("-v"+bin+".vcd") else Seq()
    val debugArgs = if(debug) outputArgs ++ dumpArgs else Seq()
    val simArgs = Seq(s"+max-cycles=$maxCycles") ++ debugArgs
    runSim(sim, Seq(), bin+".csim.sig", output, bin+".csim.out", simArgs, bin, extName)
  }

  def runRtlSim(sim: String, isaName: String, extName: Option[String], maxCycles: Int)(bin: String, debug: Boolean, output: Boolean, dump: Boolean): String = {
    val outputArgs = if(output) Seq("+verbose") else Seq()
    val dumpArgs = if(dump && debug) Seq("+vcdplusfile="+bin+".vpd") else Seq()
    val debugArgs = if(debug) outputArgs ++ dumpArgs else Seq()
    val simArgs = Seq(s"+max-cycles=$maxCycles") ++ debugArgs
    runSim(sim, Seq(), bin+".rtlsim.sig", output, bin+".rtlsim.out", simArgs, bin, extName)
  }

  def runIsaSim(isaName: String, extName: Option[String])(bin: String, debug: Boolean, output: Boolean, dump: Boolean): String = {
    val debugArgs = if(debug && output) Seq("-d") else Seq()
    val simArgs = Seq(s"--isa=$isaName") ++ (if(extName.isDefined) Seq("--extension="+extName.get) else Seq())
    runSim("spike", simArgs ++ debugArgs, bin+".spike.sig", output, bin+".spike.out", Seq(), bin, extName)
  }

  def runSimulations(golden: Simulation, simulations: Seq[Simulation])(
      bin: String,
      debug: Boolean,
      output: Boolean,
      dumpWaveform: Boolean): Seq[(Simulation, Result)] = {
    if(simulations.length == 0) println("Warning: No simulators specified for comparison. Comparing golden model to itself...")
    val goldSig = golden._2(bin, debug, output, false)
    simulations.map { case (name, sim) =>
      val res = try {
        if (goldSig != sim(bin, debug, output, dumpWaveform)) Mismatched else Matched
      } catch { case e:RuntimeException => Failed }
      ((name, sim), res)
    }
  }

  def seekOutFailureBinary(golden: Simulation, simulations: Seq[Simulation])
      (bin: String, debug: Boolean, output: Boolean, dumpWaveform: Boolean)
      (isaName: String, extName: Option[String], virtualMode: Boolean): String =
  {
    // Find failing asm file
    val source = scala.io.Source.fromFile(bin+".S")
    val lines = source.mkString
    source.close()

    // For all psegs
    val psegFinder = """pseg_\d+""".r
    val psegNums: List[Int] = psegFinder.findAllIn(lines).map(_.drop(5).toInt).toList
    var (low, high) = (psegNums.min, psegNums.max)
    if (low == high) 
    {
      println("Only one pseg was detected.")
      return bin
    }
    var lastfound = ""
    while (low <= high)
    {
      val p = (high + low)/2
      // Replace jump to pseg with jump to reg_dump
      val psegReplacer = ("pseg_" + p + ":\\n").r
      val newAsmSource = psegReplacer.replaceAllIn(lines, "pseg_" + p + ":\n\tj reg_dump\n")
      val newAsmName = bin + "_pseg_" + p + ".S"
      val fw = new FileWriter(newAsmName)
      fw.write(newAsmSource)
      fw.close()

      // Compile new asm and test on sims
      val newBinName = compileAsmToBin(newAsmName, isaName, extName, virtualMode)
      newBinName match {
        case Some(b) => {
          val res = runSimulations(golden, simulations)(b, debug, output, dumpWaveform)
          if (!res.forall(_._2 == Matched)) {
            lastfound = b
            high = p-1
          } else {
            low = p+1
          }
        }
        case None => println("Warning: Subset test could not compile.")    
      }
    }
    if (lastfound == "") {
      println("Warning: No subset tests could compile.")
      bin
    } else {
      lastfound 
    }
  }

  def seekOutFailure(golden: Simulation, simulations: Seq[Simulation])
      (bin: String, debug: Boolean, output: Boolean, dumpWaveform: Boolean)
      (isaName: String, extName: Option[String], virtualMode: Boolean): String =
  {
    // Find failing asm file
    val source = scala.io.Source.fromFile(bin+".S")
    val lines = source.mkString
    source.close()

    // For all psegs
    val psegFinder = """pseg_\d+""".r
    val psegNums: List[Int] = psegFinder.findAllIn(lines).map(_.drop(5).toInt).toList
    if (psegNums.min == psegNums.max) 
    {
      println("Only one pseg was detected.")
      return bin
    }
    for( p <- psegNums.min to psegNums.max) {
      // Replace jump to pseg with jump to reg_dump
      val psegReplacer = ("pseg_" + p + ":\\n").r
      val newAsmSource = psegReplacer.replaceAllIn(lines, "pseg_" + p + ":\n\tj reg_dump\n")
      val newAsmName = bin + "_pseg_" + p + ".S"
      val fw = new FileWriter(newAsmName)
      fw.write(newAsmSource)
      fw.close()

      // Compile new asm and test on sims
      val newBinName = compileAsmToBin(newAsmName, isaName, extName, virtualMode)
      newBinName match {
        case Some(b) => {
        val res = runSimulations(golden, simulations)(b, debug, output, dumpWaveform)
          if (!res.forall(_._2 == Matched)) {
            return b
          }
        }
        case None => println("Warning: Subset test could not compile.")    
      }
    }
    println("Warning: No subset tests could compile.")
    bin
  }
}
