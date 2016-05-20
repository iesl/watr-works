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
}
