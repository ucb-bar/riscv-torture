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

case class Options(var testAsmName: Option[String] = None,
  var testBinName: Option[String] = None,
  var cSimPath: Option[String] = None,
  var rtlSimPath: Option[String] = None,
  var seekOutFailure: Option[Boolean] = None,
  var dumpSigs: Option[Boolean] = None,
  var confFileName: Option[String] = None)

abstract sealed class Result
case object Failed extends Result
case object Mismatched extends Result
case object Matched extends Result

object TestRunner extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("testrun/run") {
      opt("C", "config", "<file>", "config file", {s: String => opts.confFileName = Some(s)})
      opt("a", "asm", "<file>", "input ASM file", {s: String => opts.testAsmName = Some(s)})
      opt("c", "csim", "<file>", "C simulator", {s: String => opts.cSimPath = Some(s)})
      opt("r", "rtlsim", "<file>", "RTL simulator", {s: String => opts.rtlSimPath = Some(s)})
      booleanOpt("s", "seek", "<boolean>", "Seek for failing pseg", {b: Boolean => opts.seekOutFailure = Some(b)})
      booleanOpt("d", "dump", "<boolean>", "Dump mistmatched signatures", {b: Boolean => opts.dumpSigs = Some(b)})
    }
    if (parser.parse(args)) {
      val confFileName = opts.confFileName.getOrElse("config") // TODO: This is removable?
      testrun(opts.testAsmName, opts.cSimPath, opts.rtlSimPath, opts.seekOutFailure, opts.dumpSigs, opts.confFileName)
    }
  }

  var virtualMode = false
  var maxcycles = 10000000
  var dump = true
  var seek = true

  def testrun(testAsmName:  Option[String], 
              cSimPath:     Option[String], 
              rtlSimPath:   Option[String], 
              doSeek:       Option[Boolean], 
              dumpSigs:     Option[Boolean],
              confFileName: Option[String]): (Boolean, Option[Seq[String]]) = 
  {

    val config = new Properties()
    val configin = new FileInputStream(confFileName.getOrElse("config"))
    config.load(configin)
    configin.close()

    maxcycles = config.getProperty("torture.testrun.maxcycles", "10000000").toInt
    virtualMode = (config.getProperty("torture.testrun.virtual", "false").toLowerCase == "true")
    dump = (config.getProperty("torture.testrun.dump", "false").toLowerCase == "true")
    seek = (config.getProperty("torture.testrun.seek", "true").toLowerCase == "true")

    // Figure out which binary file to test
    val finalBinName = testAsmName match {
      case Some(asmName) => compileAsmToBin(asmName)
      case None => {
        val gen = generator.Generator
        val newAsmName = gen.generate(confFileName.getOrElse("config"), "test")
        compileAsmToBin(newAsmName)
      }
    }

    // Add the simulators that should be tested
    val simulators = new ArrayBuffer[(String, (String) => String)]
    simulators += (("spike",runIsaSim _ ))
    cSimPath match { 
      case Some(p) => simulators += (("csim",runCSim(p) _ ))
      case None =>
    }
    rtlSimPath match { 
      case Some(p) => simulators += (("rtlsim",runRtlSim(p) _ ))
      case None => 
    }

    // Test the simulators on the complete binary
    finalBinName match {
      case Some(binName) => {
        val res = runSimulators(binName, simulators, dumpSigs.getOrElse(dump) && !doSeek.getOrElse(seek) )   
        val fail_names = res.filter(_._3 == Failed).map(_._1.toString)
        val mism_names = res.filter(_._3 == Mismatched).map(_._1.toString)
        val bad_sims  = res.filter(_._3 != Matched).map(_._2)
        if (bad_sims.length > 0) {
          println("///////////////////////////////////////////////////////")
          println("//  Simulation failed for " + binName + ":")
          fail_names.foreach(n => println("\t"+n))
          println("//  Mismatched sigs for " + binName + ":")
          mism_names.foreach(n => println("\t"+n))
          println("///////////////////////////////////////////////////////")
          if(doSeek.getOrElse(seek)) {
            val failName = seekOutFailureBinary(binName, bad_sims, dumpSigs.getOrElse(false))
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

  def compileAsmToBin(asmFileName: String): Option[String] = {  
    assert(asmFileName.endsWith(".S"), println("Filename does not end in .S"))
    val binFileName = asmFileName.dropRight(2)
    var process = ""
    if (virtualMode)
    {
      println("Virtual mode")
      process = "riscv64-unknown-elf-gcc -nostdlib -nostartfiles -Wa,-march=RVIMAFDXhwacha -std=gnu99 -O2 -I./env/v -T./env/v/link.ld ./env/v/entry.S ./env/v/vm.c " + asmFileName + " -lc -o " + binFileName
    }
    else
    {
      println("Physical mode")
      process = "riscv64-unknown-elf-gcc -nostdlib -nostartfiles -Wa,-march=RVIMAFDXhwacha -I./env/p -T./env/p/link.ld " + asmFileName + " -o " + binFileName
    }
    val pb = Process(process)
    val exitCode = pb.!
    if (exitCode == 0) Some(binFileName) else None
  }

  def dumpFromBin(binFileName: String): Option[String] = {
    val dumpFileName = binFileName + ".dump"
    val pd = Process("riscv-objdump --disassemble-all --section=.text --section=.data --section=.bss " + binFileName)
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
    val pd = Process("elf2hex 16 "+hexlines+" " + binFileName)
    val hexdump = pd.!!

    val fw = new FileWriter(hexFileName)
    fw.write(hexdump)
    fw.close()

    hexFileName
  }

  def runSim(sim: String, signature: String, args: Seq[String], invokebin: String): String = {
    val cmd = Seq(sim, "+signature="+signature) ++ args ++ Seq(invokebin)
    cmd!!

    new Scanner(new File(signature)).useDelimiter("\\Z").next()
  }

  def runCSim(sim: String)(bin: String): String = {
    runSim(sim, bin+".csim.sig", Seq("+max-cycles="+maxcycles),bin)
  }

  def runRtlSim(sim: String)(bin: String): String = {
    runSim(sim, bin+".rtlsim.sig", Seq("+max-cycles="+maxcycles),bin)
  }

  def runIsaSim(bin: String): String = {
    runSim("spike", bin+".spike.sig", Seq(), bin)
  }

  def runSimulators(bin: String, simulators: Seq[(String, (String) => String)], dumpSigs: Boolean): Seq[(String, (String, (String) => String), Result)] = { 
    if(simulators.length == 0) println("Warning: No simulators specified for comparison. Comparing ISA to ISA...")
    val isa_sig = runIsaSim(bin)
    simulators.map( sim => {
      val res =
        try {
          if (isa_sig != sim._2(bin)) Mismatched
          else Matched
        } catch {
          case e:RuntimeException => Failed
        }
      (sim._1, sim, res)
    })
  }

  def seekOutFailureBinary(bin: String, simulators: Seq[(String, (String) => String)], dumpSigs: Boolean): String =
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
      val newBinName = compileAsmToBin(newAsmName)
      newBinName match {
        case Some(b) => {
          val res = runSimulators(b, simulators, dumpSigs)   
          if (!res.forall(_._3 == Matched)) {
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

  def seekOutFailure(bin: String, simulators: Seq[(String, (String) => String)], dumpSigs: Boolean): String = {
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
      val newBinName = compileAsmToBin(newAsmName)
      newBinName match {
        case Some(b) => {
          val res = runSimulators(b, simulators, dumpSigs)   
          if (!res.forall(_._3 == Matched)) {
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

