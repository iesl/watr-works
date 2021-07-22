package org.watrworks
package utils

import TypeTags._
import ExactFloats._
import scalaz.Show

sealed trait IntervalT

object Interval {
  val IntervalT = Tag.of[IntervalT]

  type Doubles     = (Double, Double) @@ IntervalT
  type Ints        = (Int, Int) @@ IntervalT
  type FloatExacts = (FloatExact, FloatExact) @@ IntervalT

  def Ints(b: Int, len: Int): Ints = {
    IntervalT((b, len))
  }

  object Ints {
    def unapply(v: Ints): Option[(Int, Int)] = {
      Some((v.min, v.len))
    }

    def rangeIntersection(r1: Ints, r2: Ints): Option[Ints] = {
      val start = math.max(r1.min, r2.min)
      val end   = math.min(r1.max, r2.max)

      if (start < end) Some(Ints(start, end - start))
      else None
    }

    def rangeUnion(r1: Ints, r2: Ints): Ints = {
      val start = math.min(r1.min, r2.min)
      val end   = math.max(r1.max, r2.max)

      Ints(start, end - start)
    }
  }

  def DblBeginLen(b: Double, len: Double): Doubles = {
    IntervalT((b, len))
  }

  def FloatExacts(b: FloatExact, len: FloatExact): FloatExacts = {
    IntervalT((b, len))
  }

  object FloatExacts {
    def unapply(v: FloatExacts): Option[(FloatExact, FloatExact)] = {
      Some((v.min, v.len))
    }
    def fromInts(ints: Ints): FloatExacts = {
      FloatExacts(FloatRep(ints.min), FloatRep(ints.len))
    }
    def toInts(fes: FloatExacts): Ints = {
      Ints(fes.min.unwrap, fes.len.unwrap)
    }
    def rangeIntersection(r1: FloatExacts, r2: FloatExacts): Option[FloatExacts] = {
      Ints.rangeIntersection(toInts(r1), toInts(r2)).map(fromInts(_))
    }

    def rangeUnion(r1: FloatExacts, r2: FloatExacts): FloatExacts = {
      fromInts(Ints.rangeUnion(toInts(r1), toInts(r2)))
    }

    implicit val FloatExactsShow: Show[FloatExacts] = scalaz.Show.shows[FloatExacts](fes => {
      val min = ExactFloats.ShowFloatExact.show(fes.min)
      val max = ExactFloats.ShowFloatExact.show(fes.max)

      s"(${min}-${max})"
    })
  }

  def intervalSlices(interval: Doubles, slices: Int): Seq[Doubles] = {
    val stepSize = interval.len / slices

    for {
      i <- 0 until slices
    } yield {
      DblBeginLen(i * stepSize, stepSize)
    }
  }

  def doubleIntervalUnion(r1: Doubles, r2: Doubles): Doubles = {
    val start = math.min(r1.min, r2.min)
    val end   = math.max(r1.max, r2.max)

    DblBeginLen(start, end - start)
  }

  implicit class RicherFloatExactInterval(val theRange: FloatExacts) extends AnyVal {
    def min: FloatExact = theRange.unwrap._1
    def max: FloatExact = theRange.unwrap._1 + theRange.unwrap._2

    def len: FloatExact = theRange.unwrap._2

    def translate(x: FloatExact) = FloatExacts(min + x, len)

    // def contains(i: FloatExact): Boolean = min <= i && i < max
    def containsLCRC(i: FloatExact): Boolean = min <= i && i <= max

    def intersect(r2: FloatExacts): Option[FloatExacts] =
      FloatExacts.rangeIntersection(theRange, r2)

    def union(r2: FloatExacts): FloatExacts =
      FloatExacts.rangeUnion(theRange, r2)

    def show = FloatExacts.FloatExactsShow.show(theRange).toString()
  }

  implicit class RicherDoubles(val theRange: Doubles) extends AnyVal {
    def min: Double = theRange.unwrap._1
    def max: Double = theRange.unwrap._1 + theRange.unwrap._2
    def len: Double = theRange.unwrap._2

    def contains(i: Double): Boolean = min <= i && i < max
    def union(r2: Doubles): Doubles = {
      doubleIntervalUnion(theRange, r2)
    }
  }

  implicit class RicherInts(val theRange: Ints) extends AnyVal {
    def len: Int = theRange.unwrap._2
    def min: Int = theRange.unwrap._1
    def max: Int = min + len

    def translate(x: Int) = Ints(min + x, len)

    def contains(i: Int): Boolean = min <= i && i < max

    def contains(i: Ints): Boolean =
      contains(i.min) && contains(i.max)

    // min <= i && i < max
    def intersect(r2: Ints): Option[Ints] =
      Ints.rangeIntersection(theRange, r2)

    def union(r2: Ints): Ints =
      Ints.rangeUnion(theRange, r2)

  }
}
