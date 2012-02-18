package torture
package testrun

import scopt.OptionParser
import scala.sys.process._
import scala.collection.mutable.ArrayBuffer

case class Options(var testAsmName: Option[String] = None,
  var testBinName: Option[String] = None,
  var cSimPath: Option[String] = None,
  var rtlSimPath: Option[String] = None,
  var seekOutFailure: Option[Boolean] = None
)

abstract sealed class Result
case object Failed extends Result
case object Mismatched extends Result
case object Matched extends Result

object TestRunner extends Application
{
  var opts = new Options()
  override def main(args: Array[String]) =
  {
    val parser = new OptionParser("generator") {
      opt("a", "asm", "<file>", "input ASM file", {s: String => opts.testAsmName = Some(s)})
      opt("c", "csim", "<file>", "C simulator", {s: String => opts.cSimPath = Some(s)})
      opt("r", "rtlsim", "<file>", "RTL simulator", {s: String => opts.rtlSimPath = Some(s)})
      booleanOpt("s", "seek", "<boolean>", "Seek for failing pseg", {b: Boolean => opts.seekOutFailure = Some(b)})
    }
    if (parser.parse(args)) {

      // Figure out which binary file to test
      val finalBinName = opts.testAsmName match {
        case Some(asmName) => compileAsmToBin(asmName)
        case None => {
          val gen = generator.Generator
          val newAsmName = gen.generate("config", "test")
          compileAsmToBin(newAsmName)
        }
      }

      // Add the simulators that should be tested
      val simulators = new ArrayBuffer[(String) => (String, String)]
      simulators += (runIsaSim _)
      opts.cSimPath   match { 
        case Some(p) => simulators += (runCSim(p) _ ) 
        case None =>
      }
      opts.rtlSimPath match { 
        case Some(p) => simulators += (runRtlSim(p) _ )  
        case None => 
      }

      // Test the simulators on the complete binary
      finalBinName match {
        case Some(binName) => {
          val res = runSimulators(binName, simulators)   
          val fail_names = res.filter(_._3 == Failed).map(_._1.toString)
          val mism_names = res.filter(_._3 == Mismatched).map(_._1.toString)
          val bad_sims  = res.filter(_._3 != Matched).map(_._2)
          if (bad_sims.length > 0) {
            println("///////////////////////////////////////////////////////")
            println("Failed for " + binName + ":")
            fail_names.foreach(n => println("\t"+n))
            println("Mismatched Sigs " + binName + ":")
            mism_names.foreach(n => println("\t"+n))
            println("///////////////////////////////////////////////////////")
            if(opts.seekOutFailure.getOrElse(true)) seekOutFailure(binName, bad_sims)
          } else {
            println("///////////////////////////////////////////////////////")
            println("All signatures match for " + binName)
            println("///////////////////////////////////////////////////////")
          }
        }
        case None => {
          println("Error: ASM file could not be compiled or generated.")
        }
      }
    }
  }

  def compileAsmToBin(asmFileName: String): Option[String] = {  
    assert(asmFileName.endsWith(".S"))
    val binFileName = asmFileName.dropRight(2)
    val pb = Process("riscv-gcc -O2 -nostdlib -nostartfiles -T output/test.ld " + asmFileName + " -o " + binFileName)
    val exitCode = pb.!
    if (exitCode == 0) Some(binFileName) else None
  }

  def runSim(bin: String, args: Seq[String]): String = {
    val contents = Process("riscv-readelf -Ws " + bin).lines
    var found_begin = false
    var found_end = false
    var sig_addr = 0
    var sig_len = 0
    for ( line <- contents ) {
      if (line.contains("begin_signature")) {
        sig_addr = Integer.parseInt(line.split("""\s+""")(2), 16)
        found_begin = true
      } else if (line.contains("end_signature")) {
        sig_len  = Integer.parseInt(line.split("""\s+""")(2), 16)
        found_end = true
      }
    }

    if( !found_begin || !found_end) {
      println("Error: Couldn't find the .test_signature section in %s", bin)
      throw new RuntimeException()
    }
    
    val cmd = Seq("fesvr") ++ args ++ Seq("-testsig", sig_addr.toString, (sig_len-sig_addr).toString, bin)
    val out = try {
      cmd.!!
    } catch {
      case e:RuntimeException => "SIM CRASHED"
    }
    out.toString
  }

  def runCSim(sim: String)(bin: String): (String, String) = {
    ("C Simulator", runSim(bin, Seq("-c"+sim)))
  }

  def runRtlSim(sim: String)(bin: String): (String, String) = {
    val temp = ("RTL","Not done")
    temp
  }

  def runIsaSim(bin: String): (String, String) = {
    ("ISA Simulator", runSim(bin, Seq("-testrun")))
  }

  def runSimulators(bin: String, simulators: Seq[(String) => (String, String)]): Seq[(String, (String) => (String, String), Result)] = { 
    if(simulators.length == 0) println("Warning: No simulators specified for comparison. Comparing ISA to ISA...")
    val isa_sig = runIsaSim(bin)._2
    simulators.map( sim => {
      val (name, sig) = sim(bin)
      val res = if(isa_sig != sig) Mismatched
                else if (sig == "SIM CRASHED") Failed
                else Matched
      (name, sim, res)
    })
  }

  def seekOutFailure(bin: String, simulators: Seq[(String) => (String, String)]) =  
  {
    println("TODO")
  } 

}

