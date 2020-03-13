package edu.umass.cs.iesl.watr
package utils

import TypeTags._
import ExactFloats._

sealed trait IntervalT

object Interval {
  val IntervalT = Tag.of[IntervalT]

  type Doubles = (Double, Double)@@IntervalT
  type Ints = (Int, Int)@@IntervalT
  type FloatExacts = (FloatExact, FloatExact)@@IntervalT

  def Ints(b: Int, len: Int): Ints = {
    IntervalT((b, len))
  }

  object Ints {
    def unapply(v: Ints): Option[(Int, Int)] = {
      Some((v.min, v.len))
    }
  }

  def Doubles(b: Double, len: Double): Doubles = {
    IntervalT((b, len))
  }

  def FloatExacts(b: FloatExact, len: FloatExact): FloatExacts = {
    IntervalT((b, len))
  }

  def intervalSlices(interval: Doubles, slices: Int): Seq[Doubles] = {
    val stepSize = interval.len / slices

    for {
      i <- 0 until slices
    } yield {
      Doubles(i*stepSize, stepSize)
    }
  }

  def rangeIntersection(r1: Ints, r2: Ints): Option[Ints] = {
    val start = math.max(r1.min, r2.min)
    val end = math.min(r1.max, r2.max)

    if (start < end) Some(Ints(start, end-start))
    else None
  }

  def rangeUnion(r1: Ints, r2: Ints): Ints = {
    val start = math.min(r1.min, r2.min)
    val end = math.max(r1.max, r2.max)

    Ints(start, end-start)
  }

  def doubleIntervalUnion(r1: Doubles, r2: Doubles): Doubles = {
    val start = math.min(r1.min, r2.min)
    val end = math.max(r1.max, r2.max)

    Doubles(start, end-start)
  }

  implicit class RicherFloatExactInterval(val theRange: FloatExacts) extends AnyVal {
    def min: FloatExact = theRange.unwrap._1
    def max: FloatExact = theRange.unwrap._1+theRange.unwrap._2
    def len: FloatExact = theRange.unwrap._2
  }

  implicit class RicherDoubles(val theRange: Doubles) extends AnyVal {
    def min: Double = theRange.unwrap._1
    def max: Double = theRange.unwrap._1+theRange.unwrap._2
    def len: Double = theRange.unwrap._2

    def union(r2: Doubles): Doubles = {
      doubleIntervalUnion(theRange, r2)
    }
  }

  implicit class RicherInts(val theRange: Ints) extends AnyVal {
    def len: Int = theRange.unwrap._2
    def min: Int = theRange.unwrap._1
    def max: Int = min + len

    def translate(x: Int) = Ints(min+x, len)

    def contains(i: Int): Boolean = min <= i && i < max

    def contains(i: Ints): Boolean =
      contains(i.min) && contains(i.max)

      // min <= i && i < max
    def intersect(r2: Ints): Option[Ints] =
      rangeIntersection(theRange, r2)

    def union(r2: Ints): Ints =
      rangeUnion(theRange, r2)

  }
}

