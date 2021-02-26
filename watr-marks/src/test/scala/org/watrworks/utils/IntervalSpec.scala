package org.watrworks
package utils

import Interval._

class IntervalSpec extends WatrSpec {

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
