package edu.umass.cs.iesl.watr
package utils

import org.scalatest._

import EnrichNumerics._

class EnrichNumericsSpec extends FlatSpec with Matchers {
  behavior of "ranges int, double"


  it should "return intersections" in {
    val i = RangeInt(0, 3) intersect RangeInt(1, 1)
    i shouldBe { Some(RangeInt(1, 1)) }

    (RangeInt(0, 3) intersect RangeInt(1, 1)) shouldBe Some(RangeInt(1, 1))
    (RangeInt(0, 3) intersect RangeInt(2, 1)) shouldBe Some(RangeInt(2, 1))
    (RangeInt(0, 3) intersect RangeInt(3, 20)) shouldBe None

  }

  it should "generate double interval slices" in {
    val examples = List(
      DoubleInterval(0d, 100d)
    )

    examples.foreach{ example =>
      val slices = intervalSlices(example, 4)
      val res = slices.map(_.toString).mkString("\n  ", "\n  ", "\n")
      println(res)
    }

  }

}

import org.scalacheck._

object EnrichNumericsCheck extends Properties("EnrichNumericsCheck")  {

  val smallishDouble = Gen.choose(0d, 10000d)

}
