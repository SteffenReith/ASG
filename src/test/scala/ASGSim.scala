/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 *
 * Create Date:  Tue Apr  2 18:29:53 CEST 2024 
 * Module Name:  ASGSim - A simple testbench for ASG
 * Project Name: ASG - A simple random number generator
 */

import scala.sys.exit
import scala.util.Random

import spinal.lib._

import spinal.core._
import spinal.core.sim._

import Reporter._
import java.io._

object ASGSim {

  def main(args: Array[String]) : Unit = {

    // Period used for the simulation
    val simPeriod = 10

    // Number of bit to generate after loading R1, R2 and R3
    val numberOfBits = 100000

    // Make a synchronous reset and use the rising edge for the clock
    val globalClockConfig = ClockDomainConfig(clockEdge        = RISING,
                                              resetKind        = SYNC,
                                              resetActiveLevel = HIGH)

     // Use 100 MHz for the simulation
    val spinalConfig = SpinalConfig(defaultClockDomainFrequency  = FixedFrequency(100 MHz),
                                    defaultConfigForClockDomains = globalClockConfig)

    // Specification of the connection polynomials
    val connPolyStrR1 = "x^31+x^3+1"
    val connPolyStrR2 = "x^127+x^1+1"
    val connPolyStrR3 = "x^89+x^38+1"

    // Cycle counter to the simulation
    var cycles = 0

    // Create a simple simulation environment
    SimConfig.workspacePath("gen/sim")
             .allOptimisation
             .withConfig(spinalConfig)
             .withWave
             .withTimeScale(1 ns)
             .withTimePrecision(100 ps)
             .compile(new ASG(connPolyStrR1, connPolyStrR2, connPolyStrR3, false)).doSim(seed = Random.nextInt) { dut =>

      // Get the sized of the used registers
      val (lenR1, lenR2, lenR3) = dut.getRegisterLength
      val lenASG = lenR1 + lenR2 + lenR3
                                              
      // Give some general info about the simulation
      printReport(s"Start simulation of ASG using R1=${connPolyStrR1}, R2=${connPolyStrR2} and R3=${connPolyStrR3}\n")

      // Create a clock
      dut.clockDomain.forkStimulus(simPeriod)

      // Count the spent cycles 
      dut.clockDomain.onRisingEdges { cycles += 1 }

      // Report debug info
      setVerboseness(true)
      
      // Disable the device and wait for the first clock
      dut.io.enable #= false
      dut.clockDomain.waitSampling()

      // Init R1 with 0s
      printReport(s"Init R1 (cycles: ${cycles})\n")
      dut.io.loadIt #= 0x01
      dut.io.enable #= true
      dut.io.load   #= false
      dut.clockDomain.waitSampling(lenR1)

      // Feed B"101" to R1
      dut.io.loadIt #= 0x01
      dut.io.enable #= true
      dut.io.load   #= true
      dut.clockDomain.waitSampling()
      dut.io.loadIt #= 0x01
      dut.io.enable #= true
      dut.io.load   #= false
      dut.clockDomain.waitSampling()
      dut.io.loadIt #= 0x01
      dut.io.enable #= true
      dut.io.load   #= true
      dut.clockDomain.waitSampling()
      
      // Init R2 with 0s
      printReport(s"Init R2 (cycles: ${cycles})\n")
      dut.io.loadIt #= 0x02
      dut.io.enable #= true
      dut.io.load   #= false
      dut.clockDomain.waitSampling(lenR2)

      // Feed B"010" to R2
      dut.io.loadIt #= 0x02
      dut.io.enable #= true
      dut.io.load   #= false
      dut.clockDomain.waitSampling()
      dut.io.loadIt #= 0x02
      dut.io.enable #= true
      dut.io.load   #= true
      dut.clockDomain.waitSampling()
      dut.io.loadIt #= 0x02
      dut.io.enable #= true
      dut.io.load   #= false
      dut.clockDomain.waitSampling()

      // Init R3 with 0s
      printReport(s"Init R3 (cycles: ${cycles})\n")
      dut.io.loadIt #= 0x03
      dut.io.enable #= true
      dut.io.load   #= false
      dut.clockDomain.waitSampling(lenR3)

      // Feed B"111" to R3
      dut.io.loadIt #= 0x03
      dut.io.enable #= true
      dut.io.load   #= true
      dut.clockDomain.waitSampling()
      dut.io.loadIt #= 0x03
      dut.io.enable #= true
      dut.io.load   #= true
      dut.clockDomain.waitSampling()
      dut.io.loadIt #= 0x03
      dut.io.enable #= true
      dut.io.load   #= true
      dut.clockDomain.waitSampling()

      // Give some information about the simulation
      printReport(s"Start simulation of ASG (cycles: ${cycles})\n")
    
      // Counts the number of generates 1 bits
      var ones = 0

      // Open a file for the generated bits
      val dataFile = new FileWriter(new File("gen/sim/RandomData.txt"))

      // Remember the start time in cycles
      val startCycles = cycles

      // Do 'numberOfBits' simulation steps (plus some more to make the output random)
      for (i <- 1 to numberOfBits + 4 * lenASG) {

        // Generate another bit
        dut.io.enable #= true
        dut.io.loadIt #= 0x00
        dut.clockDomain.waitSampling()
     
        // Wait at least the length of all LFSR cycles (due to simple initialization)
        if (cycles - startCycles > 4 * lenASG) {

          // Check for 1 bit
          if (dut.io.newBit.toBoolean) {
          
            // Write a 1 to the file
            dataFile.write("1")

            // Sum up
            ones = ones + 1

          } else {

            // Write a 0 to the file
            dataFile.write("0")
        
          }

        }

      }

      // Remember the end time in cycles
      val endCycles = cycles

      // Print a report about the ratio between 1 and 0 (should be about 0.5)
      printReport(s"${ones.toDouble / (endCycles.toDouble - startCycles.toDouble) * 100.0} percent of ${numberOfBits} bits were 1\n")
    
      // Disable the ASG
      dut.io.enable #= false
      dut.clockDomain.waitSampling(10)

      // Close file that stores the generated bits
      dataFile.close()

    }

    // Give some information about the ended simulation
    setVerboseness(true)
    printReport(s"Simulation terminated successfully after ${cycles} cycles!\n")

  }

}
