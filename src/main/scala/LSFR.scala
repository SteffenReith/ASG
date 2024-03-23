/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 *
 * Create Date:  Thu Mar 16 14:24:06 CET 2023
 * Module Name:  LSFR - A linear shift feedback register (cf. A. Menezes et al., Handbook of Applied Cryptography, p. 196)
 * Project Name: ASG  - A simple random number generator
 *
 */

import spinal.core._
import cc.redberry.rings
import cc.redberry.rings.bigint.BigInteger
import cc.redberry.rings.primes.BigPrimes._
import cc.redberry.rings.poly.FiniteField
import cc.redberry.rings.poly.PolynomialMethods._
import cc.redberry.rings.scaladsl
import cc.redberry.rings.scaladsl._
import syntax._

import scala.collection.JavaConverters._

class LSFR (degree : Int, polyString : String) extends Component {

  val io = new Bundle {

    val load   = in Bool()
    val loadIt = in Bool()

    val enable = in Bool()

    val newBit = out Bool()

  }
  noIoPrefix()

  // Calculate all non-trivial divisors of num (e.g. calculateNonTrivialDivisors(6) == List(2,3))
  private def calculateNonTrivialDivisors(num : IntZ) = {

    // Convert the list representation into an integer
    def convertToNum(l : List[IntZ]) : IntZ = l.fold(Z(1))((x, y) => x * y)

    // Calculate a list of all prime factors of num 
    val p = primeFactors(num).asScala

    // Calculate all non trivial divisors represented as a list of primes
    val divAsLists = ((1 until p.length flatMap(x => p.combinations(x))).toList).map(x => x.toList)
     
    // Convert the list representations into a list of integers
    divAsLists.map(x => convertToNum(x)).sorted

  }

  // Create the ring Z/2Z[x]
  private val ring = UnivariateRingZp64(2, "x")

  // Create the polynom in Z/2Z[x] to check irreducibility 
  private val ringPoly = ring(polyString)

  // Check for irreducibility
  assert(irreducibleQ(ringPoly), "ERROR: The connection polynomial has to be irreducible over Z/2Z") 

  // Calculate the field size
  val fieldSize = 2.pow(ringPoly.degree)

  // A finite field having 2^degree elements
  private val field = GF(2, fieldSize, "x")
  
  // The polynomial as field element
  private val fieldPoly = field(polyString)

  // Calculate all orders of possible non-trivial subgroups of the multiplicative group
  private val orders = calculateNonTrivialDivisors(fieldSize - 1)

  // Test for all possible non-trivial sub-group whether poly generates a subgroup only
  private val subGroupTests = orders.map(x => field.pow(fieldPoly, x))//.filter(x => (x != field.getOne))
  println(s"HHHH: ${orders} --- ${subGroupTests}")
  System.exit(-1)

  // Check if we can reach the full period
  assert(subGroupTests.isEmpty, "ERROR: The connection polynomial has to be primitive (generate Z/(2^degree)Z[x])")

  // Give some information about the period
  print("[LSFR] The given polynomial is primitive! ")
  println("The full period 2^" + degree + " - 1 = " + (2.pow(degree) - 1) + " can be reached!")

  // Generate a list of taps and ignore the final +1 monom (it exists since poly is irreducible)
  private val activeTaps = ringPoly.exponents().toArray().dropRight(1)

  // Print some information about the taps to be used
  print("[LSFR] Use the connection polynomial " + polyString + " with taps at ")
  println(activeTaps.mkString("(",",",")"))

  // Some debug message
  println("[LSFR] Create a register of width " + ringPoly.degree())

  // Init the register with 0 such that the register generates no random bits
  private val fsRegN = Bits(ringPoly.degree() bits)
  private val fsReg = RegNextWhen(fsRegN, io.enable) init(0)

  // Select the tap-bits from register
  private val taps = activeTaps.map(x => fsReg(ringPoly.degree() - x))

  // xor all tap-bits together
  private val genBit = taps.reduce((x,y) => x ^ y)

  // Check if the register should be loaded
  when(io.load) {

    // Get data to be loaded into the register
    genBit := io.loadIt

  }

  // Get the output bit
  io.newBit := fsReg.lsb

  // Shift the register
  fsRegN := genBit ## fsReg(fsReg.high downto 1)

}

object LSFR {

  // Make a synchronous reset and use the rising edge for the clock
  private val globalClockConfig = ClockDomainConfig(clockEdge        = RISING,
                                                    resetKind        = SYNC,
                                                    resetActiveLevel = HIGH)

  // Suppose that the design runs with 100 MHz
  private val globalFrequency = FixedFrequency(100 MHz)

  def main(args: Array[String]) : Unit = {


    // Generate VHDL
    SpinalConfig(mergeAsyncProcess            = true,
                 genVhdlPkg                   = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 defaultClockDomainFrequency  = globalFrequency,
                 targetDirectory              = "gen/src/vhdl").generateVhdl(new LSFR(4, "x^4+x+1")).printPruned()

    // Generate Verilog / Maybe mergeAsyncProcess = false helps verilator to avoid wrongly detected combinatorial loops
    SpinalConfig(mergeAsyncProcess            = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 defaultClockDomainFrequency  = globalFrequency,
                 targetDirectory              = "gen/src/verilog").generateVerilog(new LSFR(4, "x^4+x+1")).printPruned()

  }

}
