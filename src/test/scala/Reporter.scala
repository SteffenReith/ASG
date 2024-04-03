/*
 * Author: Steffen Reith (Steffen.Reith@hs-rm.de)
 *
 * Create Date:    Wed Apr  3 01:32:35 CEST 2024
 * Module Name:    Reporter - A simple report function
 * Project Name: ASG - A simple random number generator
 */

object Reporter {

    // Store the verbose mode
    private var level : Boolean = false

    // Print a simple string as report
    def printReport(s : String) : Unit = {if (level) printf(s"INFO: ${s}")}

    // The the verboseness mode 
    def setVerboseness(m : Boolean) : Unit = {level = m}

}
