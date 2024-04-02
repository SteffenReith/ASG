/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 *
 * Create Date:  Thu Mar 16 14:24:06 CET 2023
 * Module Name:  LSFR - A linear shift feedback register 
 *               (cf. A. Menezes et al., Handbook of Applied Cryptography, p. 196)
 * Project Name: ASG  - A simple random number generator
 *
 */

import spinal.core._

import cc.redberry.rings.primes.BigPrimes._
import cc.redberry.rings.poly.PolynomialMethods._
import cc.redberry.rings.scaladsl._
import syntax._

import scala.collection.JavaConverters._

class LSFR (polyString : String, trust : Boolean = false) extends Component {

  val io = new Bundle {

    val load   = in Bool()
    val loadIt = in Bool()

    val enable = in Bool()

    val newBit = out Bool()

  }
  noIoPrefix()

  // Calculate all non-trivial divisors of num (e.g. calculateNonTrivialDivisors(6) == List(2,3))
  private def calculateNonTrivialDivisors(num : IntZ) = {

    // Convert the list representation of a divisor into an integer
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

  // Create the polynomial in Z/2Z[x]
  private val ringPoly = ring(polyString)

  // Calculate the degree if the given connection polynom
  private val degree = ringPoly.degree()

  // Calculate the size of the multiplicative group of the field (Z/2Z[x])/(ringPoly) iff ringPoly is irreducible
  private val multOrder = 2.pow(degree) - 1

  // Get the period of the LSFR
  def getMaxPeriod = multOrder
  
  // Get the degree of the used polynomial
  def getDegree = degree

  // Check if we trust the connection polynomial 
  if (trust) {
  
    // Give a debug message about the trust mode
    println("[LSFR]: We are in trust mode!")
    println(s"The properties of the provided connection polynomial ${polyString} is _not_ checked!")

  } else {

    // Check for irreducibility over Z/2Z
    assert(irreducibleQ(ringPoly), "ERROR: The connection polynomial has to be irreducible over Z/2Z!") 

    // Calculate all powers x^d where d is a non-trival divider of multOrder as a string
    val strPowersOfX = calculateNonTrivialDivisors(multOrder).map(y => "x^" + y.toString)
  
    // Convert the strings representations to real ring elements
    val ringPowersOfX = strPowersOfX.map(y => ring.parse(y))

    // Reduce the powers of X mod ringPoly
    val reducedPowersOfX = ringPowersOfX.map(y => remainder(y, ringPoly))

    // If ringPoly is primitive x generates the field, hence check whether x^j==1 
    val powerToOnes = reducedPowersOfX.filter(y => (y == ring.one))

    // If x^j==1 then x only generates a non-trivial subgroup of the field, hence ringPoly is not primitive
    assert(powerToOnes.isEmpty, "ERROR: The connection polynomial has to be primitive over Z/2Z!")
    
    // Give some information about the period
    print("[LSFR] The given polynomial is primitive! ")
    println("The full period 2^" + degree + " - 1 = " + multOrder + " can be reached!")

  }

  // Generate a list of taps and ignore the final +1 monom (it exists since poly is irreducible)
  private val activeTaps = ringPoly.exponents().toArray().dropRight(1)

  // Print some information about the taps to be used
  print("[LSFR] Use the connection polynomial " + polyString + " with taps at ")
  println(activeTaps.mkString("(",",",")"))

  // Some debug message
  println("[LSFR] Create a register of width " + degree)

  // Init the register with 0 such that the register generates no random bits
  private val fsRegN = Bits(degree bits)
  private val fsReg = RegNextWhen(fsRegN, io.enable) init(0)

  // Select the tap-bits from register
  private val taps = activeTaps.map(x => fsReg(degree - x))

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

    // Specification of the used connection polynomial
    val connPolyStr = "x^18 + x^7 + 1"

    // Generate VHDL (do not check the connection polynomial)
    SpinalConfig(mergeAsyncProcess            = true,
                 genVhdlPkg                   = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 defaultClockDomainFrequency  = globalFrequency,
                 targetDirectory              = "gen/src/vhdl").generateVhdl(new LSFR(connPolyStr, trust = true)).printPruned()

    // Generate Verilog / Maybe mergeAsyncProcess = false helps verilator to avoid wrongly detected combinatorial loops
    SpinalConfig(mergeAsyncProcess            = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 defaultClockDomainFrequency  = globalFrequency,
                 targetDirectory              = "gen/src/verilog").generateVerilog(new LSFR(connPolyStr)).printPruned()

  }

}
