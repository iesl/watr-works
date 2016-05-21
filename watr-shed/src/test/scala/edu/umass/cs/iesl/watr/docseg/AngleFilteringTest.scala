package edu.umass.cs.iesl
package watr
package docseg

import org.scalatest._

import watrmarks._

class AngleFilteringTest extends FlatSpec with Matchers {
  behavior of "docstrum segmenter"

  val LB = watrmarks.StandardLabels

  it should "compute angles correctly" in {
    import Bounds._

    val points = List(
      Point(1, 0) -> Point(1, 0),
      Point(1, 0) -> Point(1, 1),
      Point(1, 0) -> Point(0, 1),
      Point(1, 0) -> Point(-1, -1)
    )

    points.foreach { case(p1, p2) =>
      println(s"${p1.prettyPrint} -> ${p2.prettyPrint}: angle = ${p1.angleTo(p2)}")
    }

  }

  import DocstrumSegmenter._

  it should "filter angles correctly" in {
    assert{ filterAngle(0, math.Pi/2)(0) }
    assert{ filterAngle(0, math.Pi/2)(math.Pi/4-0.001d) }
    assert{ ! filterAngle(0, math.Pi/2)(math.Pi/4+0.001d) }
    assert{ ! filterAngle(0, math.Pi/2)(math.Pi) }

  }
}
