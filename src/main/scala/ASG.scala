/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 *
 * Create Date:  Thu Mar 16 14:24:06 CET 2023 
 * Module Name:  ASG - A simple alternating step generator 
 *               (cf. A. Menezes et al., Handbook of Applied Cryptography, p. 209)
 * Project Name: ASG - A simple random number generator 
 * Remark:       This is only a demo! Since R1 doesn't deliver a de Bruijn sequence 
 *               (and we didn't pad for making one) the period is not guaranteed! 
 *               Hence the given period is only a rough and not reliable assumption.
 */

import sys.exit

import scopt.OptionParser

import spinal.core._

import cc.redberry.rings.bigint.BigInteger
import cc.redberry.rings.primes.BigPrimes._
import cc.redberry.rings.poly.FiniteField
import cc.redberry.rings.poly.PolynomialMethods._
import cc.redberry.rings.scaladsl._
import syntax._

class ASG (R1PolyString : String,
           R2PolyString : String,
           R3PolyString : String,
           trust        : Boolean = false) extends Component {

  val io = new Bundle {

    val loadIt = in Bits(2 bits)
    val load   = in Bool()

    val enable = in Bool()

    val newBit = out Bool()

  }

  noIoPrefix()

  // Create LSFR R1
  println("[ASG]: Create LSFR R1")
  val R1 = new LSFR(R1PolyString, trust)

  // Create LSFR R2
  println("[ASG]: Create LSFR R2")
  val R2 = new LSFR(R2PolyString, trust)

  // Create LSFR R3
  println("[ASG]: Create LSFR R3")
  val R3 = new LSFR(R3PolyString, trust)

  // Create the enable logic
  R1.io.enable := io.enable
  R2.io.enable := io.enable &  R1.io.newBit
  R3.io.enable := io.enable & ~R1.io.newBit

  // Select the correct register to load initial data
  R1.io.loadIt := io.loadIt === B"00"
  R2.io.loadIt := io.loadIt === B"01"
  R3.io.loadIt := io.loadIt === B"10"

  // Create the load signals of the shift registers
  R1.io.load := io.load
  R2.io.load := io.load
  R3.io.load := io.load

  // Create the new output bit
  io.newBit := R2.io.newBit ^ R3.io.newBit

  // Check whether the length of R2 and R3 are co-prime
  if (R2.getPeriod.gcd(R3.getPeriod()) > 1) {

    // Give a warning about not reaching the full period
    println("[ASG]: WARNING - The choice of your parameters will not result in the full period length!")

  }

  // Give some information about the expected cycle length
  println(s"[ASG]: The expected period length is 2^${R1.getDegree}*${R2.getPeriod}*${R3.getPeriod} = ${2.pow(R1.getDegree) * R2.getPeriod * R3.getPeriod}")  

}

object ASG {

  // Make a synchronous reset and use the rising edge for the clock
  private val globalClockConfig = ClockDomainConfig(clockEdge        = RISING,
                                                    resetKind        = SYNC,
                                                    resetActiveLevel = HIGH)

  // Suppose that the design runs with 100 MHz
  private val globalFrequency = FixedFrequency(100 MHz)

  def main(args: Array[String]) : Unit = {

    // Create a new scopt parser
    val parser = new OptionParser[ArgsConfig]("Alternating Step Generator") {

      // A simple header for the help text
      head("ASG - A simple alternating step generator to produce pseudo-random bits", "")

      // Option to trust the polynomails (avoid the time consuming test to check of primitiveness)
      opt[Boolean]("trust").action {(v,c) => c.copy(trustArg = Some(v)) }
                           .text("Trust the provided connection polynomials")

      // Help option
      help("help").text("print this text")

    }

    // Get the argument values
    val (trust) = parser.parse(args, ArgsConfig(trustArg = Some(false))).map {cfg =>

      // Return the arguments
      (cfg.trustArg.get)

    } getOrElse {

      // Terminate program with error-code (wrong argument / option)
      exit(1)

    }

    // Generate VHDL
    SpinalConfig(mergeAsyncProcess            = true,
                 genVhdlPkg                   = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 defaultClockDomainFrequency  = globalFrequency,
                 targetDirectory              = "gen/src/vhdl").generateVhdl(new ASG("x^3+x^2+1", "x^4+x^3+1", "x^5+x^4+x^3+x+1", trust)).printPruned()

    // Generate Verilog / Maybe mergeAsyncProcess = false helps verilator to avoid wrongly detected combinatorial loops
    SpinalConfig(mergeAsyncProcess            = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 defaultClockDomainFrequency  = globalFrequency,
                 targetDirectory              = "gen/src/verilog").generateVerilog(new ASG("x^3+x^2+1", "x^4+x^3+1", "x^5+x^4+x^3+x+1", trust)).printPruned()

  }

}
