package edu.umass.cs.iesl.watr
package utils

object ShowNumerics {
  def fmt = (d: Double) => f"${d}%1.2f"

  implicit class RicherDouble(val d: Double) extends AnyVal {
    def pp:String = fmt(d)
  }
}
