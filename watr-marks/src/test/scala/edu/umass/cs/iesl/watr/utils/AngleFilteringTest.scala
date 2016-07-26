package edu.umass.cs.iesl.watr
package utils

import org.scalatest._

import spindex._
import IndexShapeOperations._
import GeometricFigure._

class AngleFilteringTest extends FlatSpec with Matchers {
  // import DocumentSegmenter._

  behavior of "angle filtering"

  it should "make sure I compute angles correctly" in {
    import math._

    val ctr = Point(0, 0)

    val pointsOnUnitCircle = (0.0 to Pi*2 by 0.3).map{ r =>
      (Point(cos(r),  sin(r)), r)
    }

    pointsOnUnitCircle.foreach { case(point, r) =>
      val atan = math.atan2(point.y, point.x)
      println(s"r:${r.pp} (${(r/Pi).pp})  = ${point.prettyPrint}, atan: ${atan.pp} p1 angleto p2: ${ctr.angleTo(point)}")
    }


    // val pointsOnUnitCircleRev = (0.0 to -Pi by -0.2).map{ r =>
    //   (Point(cos(r),  sin(r)), r)
    // }

    // pointsOnUnitCircleRev.foreach { case(point, r) =>
    //   val atan = math.atan2(point.y, point.x)
    //   println(s"r:${r.pp} (${(r/Pi).pp})  = ${point.prettyPrint}, atan: ${atan.pp} p1 angleto p2: ${ctr.angleTo(point)}")
    // }






  }

  // it should "compute angles correctly" in {
  //   import Bounds._

  //   val points = List(
  //     Point(1, 0) -> Point(1, 0),
  //     Point(1, 0) -> Point(1, 1),
  //     Point(1, 0) -> Point(0, 1),
  //     Point(1, 0) -> Point(-1, -1)
  //   )

  //   points.foreach { case(p1, p2) =>
  //     println(s"${p1.prettyPrint} -> ${p2.prettyPrint}: angle = ${p1.angleTo(p2)}")
  //   }
  // }


  // it should "filter angles correctly" in {
  //   assert{ filterAngle(0, math.Pi/2)(0) }
  //   assert{ filterAngle(0, math.Pi/2)(math.Pi/4-0.001d) }
  //   assert{ ! filterAngle(0, math.Pi/2)(math.Pi/4+0.001d) }
  //   assert{ ! filterAngle(0, math.Pi/2)(math.Pi) }

  // }
}
