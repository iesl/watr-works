package edu.umass.cs.iesl.watr
package utils

import TypeTags._

object EnrichNumerics {
  def fmt = (d: Double) => {
    f"${d}%1.2f"
  }

  type DoubleInterval = (Double, Double)@@Interval
  type RangeInt = (Int, Int)@@Interval

  def RangeInt(b: Int, len: Int): RangeInt = {
    Interval((b, len))
  }
  def DoubleInterval(b: Double, len: Double): DoubleInterval = {
    Interval((b, len))
  }

  def intervalSlices(interval: DoubleInterval, slices: Int): Seq[DoubleInterval] = {
    val stepSize = interval.len / slices

    for {
      i <- 0 until slices
    } yield {
      DoubleInterval(i*stepSize, stepSize)
    }
  }

  def rangeIntersection(r1: RangeInt, r2: RangeInt): Option[RangeInt] = {
    val start = math.max(r1.min, r2.min)
    val end = math.min(r1.max, r2.max)

    if (start < end) Some(Interval((start, end-start)))
    else None
  }

  def rangeUnion(r1: RangeInt, r2: RangeInt): RangeInt = {
    val start = math.min(r1.min, r2.min)
    val end = math.max(r1.max, r2.max)

    Interval((start, end-start))
  }
  def doubleIntervalUnion(r1: DoubleInterval, r2: DoubleInterval): DoubleInterval = {
    val start = math.min(r1.min, r2.min)
    val end = math.max(r1.max, r2.max)

    Interval((start, end-start))
  }

  implicit class RicherDoubleInterval(val theRange: DoubleInterval) extends AnyVal {
    def min: Double = theRange.unwrap._1
    def max: Double = theRange.unwrap._1+theRange.unwrap._2
    def len: Double = theRange.unwrap._2

    def union(r2: DoubleInterval): DoubleInterval = {
      doubleIntervalUnion(theRange, r2)
    }
  }

  implicit class RicherRangeInt(val theRange: RangeInt) extends AnyVal {
    def len: Int = theRange.unwrap._2
    def min: Int = theRange.unwrap._1
    def max: Int = min + len

    def translate(x: Int) = RangeInt(min+x, len)

    def contains(i: Int): Boolean = min <= i && i < max

    def intersect(r2: RangeInt): Option[RangeInt] =
      rangeIntersection(theRange, r2)

    def union(r2: RangeInt): RangeInt =
      rangeUnion(theRange, r2)

  }

  implicit class RicherInt_EnrichNumerics(val theInt: Int) extends AnyVal {
    def percent: Double@@Percent = {
      Percent(theInt.toDouble)
    }
  }

  implicit class RicherDouble_EnrichNumerics(val theDouble: Double) extends AnyVal {

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

    def plusOrMinus(i: Double@@Percent): DoubleInterval ={
      val half = theDouble * i.unwrap
      Interval((theDouble-half, theDouble+half))
    }

    def intersects(r: DoubleInterval): Boolean = {
      r.min <= theDouble && theDouble <= r.max
    }

    def withinRange(r: DoubleInterval): Boolean = {
      r.min <= theDouble && theDouble <= r.max
    }

    def nan = java.lang.Double.isNaN(theDouble)
    def inf = java.lang.Double.isInfinite(theDouble)
  }

  implicit class RicherFloat_EnrichNumerics(val d: Float) extends AnyVal {
    def nan = java.lang.Float.isNaN(d)
    def inf = java.lang.Float.isInfinite(d)
    // def prettyPrint:String = fmt(d.toDouble)
    // def pp(): String = fmt(d.toDouble)
  }

}
