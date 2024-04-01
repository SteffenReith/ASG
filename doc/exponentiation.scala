// Calculate p^n for an univariate polynomial (the rings version seems to be buggy)
  private def fieldPowerMod(p : UnivariatePolynomialZp64, n : IntZ)(implicit field : GaloisField64) : UnivariatePolynomialZp64 = {

    // We only accept positive exponents
    assert(n >= 0, "ERROR: The exponent of fieldPowerMod has to be positive")

    // Init the result to 1
    var result = field.one

    // For all bit of the exponent n
    for (i <- (n.bitLength() - 1) to 0 by -1) {

      // Square the temporal value
      result = remainder(result.square(), field.getMinimalPolynomial())

      // If ith bit of exponent n is set
      if (n.testBit(i)) {

        // Multiply p to the result
        result = remainder(result.multiply(p), field.getMinimalPolynomial())

      }

    }

    // Give some debug info
    println(s"[LFSR] Calculation of (${p})^${n} = ${result} completed")

    // Return the result
    result

  }


