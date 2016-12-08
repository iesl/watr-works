package edu.umass.cs.iesl.watr
package utils

import scalaz.@@
import TypeTags._



object EnrichNumerics {
  def fmt = (d: Double) => f"${d}%1.2f"

  type RangeDouble = (Double, Double)@@Ranging
  type RangeInt = (Int, Int)@@Ranging

  def RangeInt(b: Int, len: Int): RangeInt = {
    Ranging((b, len))
  }
  def RangeDouble(b: Double, len: Double): RangeDouble = {
    Ranging((b, len))
  }

  def rangeIntersection(r1: RangeInt, r2: RangeInt): Option[RangeInt] = {
    val start = math.max(r1.min, r2.min)
    val end = math.min(r1.max, r2.max)

    if (start < end) Some(Ranging((start, end-start)))
    else None
  }

  implicit class RicherRangeDouble(val theRange: RangeDouble) extends AnyVal {
    def min: Double = theRange.unwrap._1
    def max: Double = theRange.unwrap._1+theRange.unwrap._2
  }

  implicit class RicherRangeInt(val theRange: RangeInt) extends AnyVal {
    def min: Int = theRange.unwrap._1
    def max: Int = theRange.unwrap._1+theRange.unwrap._2

    def intersect(r2: RangeInt): Option[RangeInt] =
      rangeIntersection(theRange, r2)

  }

  implicit class RicherDouble_EnrichNumerics(val theDouble: Double) extends AnyVal {

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


    // def plusOrMinus(i: Double@@Percent): Ranges.Doubles ={
    def plusOrMinus(i: Double@@Percent): RangeDouble ={
      val half = theDouble * i.unwrap
      Ranging((theDouble-half, theDouble+half))
    }

    def intersects(r: RangeDouble): Boolean = {
      r.min <= theDouble && theDouble <= r.max
    }


  }



}
