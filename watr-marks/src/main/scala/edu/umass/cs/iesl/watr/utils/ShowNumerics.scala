package edu.umass.cs.iesl.watr
package utils

object EnrichNumerics {
  def fmt = (d: Double) => f"${d}%1.2f"


  implicit class RicherDouble_1(val d: Double) extends AnyVal {

    def prettyPrint:String = fmt(d)
    def pp(): String = fmt(d)

    def eqFuzzy(tolerance: Double)(d2: Double): Boolean =
      compareFuzzy(tolerance)(d2) == 0


    def compareFuzzy(tolerance: Double)(d2: Double): Int = {
      if (math.abs(d - d2) < tolerance) 0
      else if (d < d2) -1
      else 1
    }

  }
}
