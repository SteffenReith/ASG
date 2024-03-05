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
import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes.BigPrimes._

import scala.collection.JavaConverters._

case class LSFR (polyString : String) extends Component {

  // Note that the given polynom hat to be irreducible!
  private def primitivePolynomQ(primeField : UnivariateRingZp64, minPoly : UnivariatePolynomialZp64, order : BigInteger) : Boolean = {

    def decomposeMult(a : (BigInteger, Int), b : (BigInteger, Int) ) : (BigInteger, Int) = (a._1 * b._1, 1)

    // Check if group order is a prime with probability 1-1/2^60
    if (!order.isProbablePrime(60)) {

      // Calculate all divisors of the group orders >1 as an indexed set of prime factors
      val indexedFactors = primeFactors(order).asScala.zipWithIndex.toSet.subsets().toList.filter(z => z.nonEmpty)

      // Convert the prime factor representation into a number by multiplying all factors
      val groupOrdersIndexed = indexedFactors.map(x => x.reduceLeft(decomposeMult))

      // Get rid of the unneeded indices, select distinct divisors and choose only nontrivial ones
      val groupOrders = groupOrdersIndexed.map { case (y, _) => y }.distinct.sorted.dropRight(1)

      // Give some informations about the group structure
      print("[LSFR] Possible non-trivial orders of the multiplicative subgroup of GF(2^" + minPoly.degree() + ") are ")
      println(groupOrders.mkString("(", ",", ")"))

      // Create the field
      val field = rings.Rings.GF(minPoly)

      // Find all roots of our polynom in the field
      val roots = field.factor(minPoly).asScala.toList.filter(x => x.degree() == 1).map(x => (-1 * x.evaluate(0)))
      print("IIII" + minPoly + "OOO")
      roots.foreach(println(_))
      print("JJJJ ")
      val isSubGroupListQ = groupOrders.map(x => (!field.pow(minPoly, x).isOne))

      val a = groupOrders.map(x => (field.pow(minPoly, x)))
      println("DDD" + poly + "XX" + field.pow(poly, 2) + "LL" + a.mkString("(",",",")"))

      // Check if the polynom creates no non-trival subgroup
      isSubGroupListQ.reduceLeft(_ && _)

    } else {

      // Check if given polynom is 1
      if (!minPoly.isOne) {

        // Give some information
        println("[LSFR] Group order " + order + " is prime! Hence there are no non-trivial subgroups!")

      }

      // Check if polynom is primitive
      !minPoly.isOne

    }

  }

  // Create a Z/2Z[x] ring object
  private val ring = rings.scaladsl.UnivariateRingZp64(2, "x")

  // Convert the given polynom description for creating the finite field
  private val poly = ring(polyString)

  // Check for irreducibility
  assert(irreducibleQ(poly), "WARNING: The connection polynomial has to be irreducible")

  // The order of the multiplicative group
  val order = 2.pow(poly.degree()) - 1

  // Check if we can reach the full period
  if (primitivePolynomQ(ring, poly, order)) {

    // Give some information about the period
    print("[LSFR] The given polynom is primitive! ")
    println("The full period 2^" + poly.degree() + " - 1 = " + order + " can be reached!")

  } else {

    // Give a warning
    println("[LSFR] WARNING: The given polynom is not primitive! The full period is not reached!")

  }

  // Generate a list of taps and ignore the final +1 monom (it exists since poly is irreducible)
  private val activeTaps = poly.exponents().toArray().dropRight(1)

  // Print some information about the taps to be used
  print("[LSFR] Use the connection polyom " + polyString + " with taps at ")
  println(activeTaps.mkString("(",",",")"))

  val io = new Bundle {

    val load   = in Bool()
    val loadIt = in Bool()

    val enable = in Bool()

    val newBit = out Bool()

  }

  noIoPrefix()

  // Some debug message
  println("[LSFR] Create a register of width " + poly.degree())

  // Init the register with 0 such that the register generates no random bits
  private val fsRegN = Bits(poly.degree() bits)
  private val fsReg = RegNextWhen(fsRegN, io.enable) init(0)

  // Select the tap-bits from register
  private val taps = activeTaps.map(x => fsReg(poly.degree() - x))

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

  def main(args: Array[String]) {

    // Make the reset synchronous and use the rising edge for sampling
    val globalClockConfig = ClockDomainConfig(clockEdge        = RISING,
                                              resetKind        = SYNC,
                                              resetActiveLevel = HIGH)

    // Generate VHDL
    SpinalConfig(mergeAsyncProcess            = true,
                 genVhdlPkg                   = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 defaultClockDomainFrequency  = FixedFrequency(value = 10 MHz),
                 targetDirectory              = "gen/src/vhdl").generateVhdl(new LSFR("x^4+x+1")).printPruned()

    // Generate Verilog / Maybe mergeAsyncProcess = false helps verilator to avoid wrongly detected combinatorial loops
    SpinalConfig(mergeAsyncProcess            = true,
                 defaultConfigForClockDomains = globalClockConfig,
                 defaultClockDomainFrequency  = FixedFrequency(value = 10 MHz),
                 targetDirectory              = "gen/src/verilog").generateVerilog(new LSFR("x^4+x+1")).printPruned()

  }

}
