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

}
