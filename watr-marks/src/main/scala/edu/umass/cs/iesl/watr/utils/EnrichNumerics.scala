package edu.umass.cs.iesl.watr
package utils


import scalaz.@@
import TypeTags._


sealed trait RangeAB

object Ranges {
  case class Doubles(min: Double, max: Double)
  case class Ints(min: Int, max: Int)
}

object EnrichNumerics {
  def fmt = (d: Double) => f"${d}%1.2f"


  implicit class RicherDouble_1(val theDouble: Double) extends AnyVal {

    def prettyPrint:String = fmt(theDouble)
    def pp(): String = fmt(theDouble)

    def eqFuzzy(tolerance: Double)(d2: Double): Boolean =
      compareFuzzy(tolerance)(d2) == 0


    def compareFuzzy(tolerance: Double)(d2: Double): Int = {
      if (math.abs(theDouble - d2) < tolerance) 0
      else if (theDouble < d2) -1
      else 1
    }

    def percent: Double@@Percent = {
      assert(0.0 <= theDouble && theDouble <= 100.0)
      Percent(theDouble)
    }

    def plusOrMinus(i: Double@@Percent): Ranges.Doubles ={
      val half = theDouble * i.unwrap
      Ranges.Doubles(theDouble-half, theDouble+half)
    }

    def withinRange(r: Ranges.Doubles): Boolean = {
      r.min <= theDouble && theDouble <= r.max
    }


  }
}
