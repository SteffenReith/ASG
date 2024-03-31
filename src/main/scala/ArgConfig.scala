/*
 * Author: Steffen Reith (steffen.reith@hs-rm.de)
 *
 * Creation Date: Sat Mar 30 21:26:34 CET 2024 
 * Module Name:   ArgsConfig - Holds command line parameters
 * Project Name:  ASG - A simple random number generator 
 *
 */

// Used by scopt to store information
case class ArgsConfig(trustArg : Option[Boolean])
