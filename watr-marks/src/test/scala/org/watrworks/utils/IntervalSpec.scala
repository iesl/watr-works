package edu.umass.cs.iesl.watr
package utils

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import EnrichNumerics._
import Interval._

class IntervalSpec extends AnyFlatSpec with Matchers {

  behavior of "ranges int, double"


  it should "return intersections" in {
    val i = Interval.Ints(0, 3) intersect Interval.Ints(1, 1)

    i shouldBe { Some(Interval.Ints(1, 1)) }

    (Interval.Ints(0, 3) intersect Interval.Ints(1, 1)) shouldBe Some(Interval.Ints(1, 1))

    (Interval.Ints(0, 3) intersect Interval.Ints(2, 1)) shouldBe Some(Interval.Ints(2, 1))

    (Interval.Ints(0, 3) intersect Interval.Ints(3, 20)) shouldBe None

  }

  it should "generate double interval slices" in {
    val examples = List(
      Interval.Doubles(0d, 100d)
    )

    examples.foreach{ example =>
      val slices = intervalSlices(example, 4)
      val res = slices.map(_.toString).mkString("\n  ", "\n  ", "\n")
      println(res)
    }

  }
}

import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object IntervalsCheck extends Properties("IntervalsCheck")  {

  val smallishDouble = Gen.choose(0d, 10000d)

  // property("") = forAll{ (interval: Interval.Ints) =>
  //   true
  // }
}
