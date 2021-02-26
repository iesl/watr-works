package org.watrworks
package utils

import TypeTags._

object Maths {

  def clamp(low: Int, high: Int)(i: Int): Int =
    math.max(low, math.min(i, high))

}

object EnrichNumerics {

  def fmt = (d: Double) => {
    f"${d}%1.2f"
  }


  implicit class RicherInt_EnrichNumerics(val theInt: Int) extends AnyVal {
    def percent: Double@@Percent = {
      Percent(theInt.toDouble)
    }
  }

  implicit class RicherDouble_EnrichNumerics(val theDouble: Double) extends AnyVal {
    import Interval._

    def pp(): String = fmt(theDouble)

    def eqFuzzy(tolerance: Double)(d2: Double): Boolean =
      compareFuzzy(tolerance)(d2) == 0


    def compareFuzzy(tolerance: Double)(d2: Double): Int = {
      if (math.abs(theDouble - d2) < tolerance) 0
      else if (theDouble < d2) -1
      else 1
    }

    def percent: Double@@Percent = {
      // assert(0.0 <= theDouble && theDouble <= 100.0)
      Percent(theDouble)
    }

    def plusOrMinus(i: Double@@Percent): Interval.Doubles ={
      val half = theDouble * i.unwrap
      Interval.DblBeginLen(theDouble-half, theDouble+half)
    }

    def intersects(r: Interval.Doubles): Boolean = {
      r.min <= theDouble && theDouble <= r.max
    }

    def withinRange(r: Interval.Doubles): Boolean = {
      r.min <= theDouble && theDouble <= r.max
    }

    def nan = java.lang.Double.isNaN(theDouble)
    def inf = java.lang.Double.isInfinite(theDouble)
  }

  implicit class RicherFloat_EnrichNumerics(val d: Float) extends AnyVal {
    def nan = java.lang.Float.isNaN(d)
    def inf = java.lang.Float.isInfinite(d)
  }

}
