package edu.umass.cs.iesl.watr
package geometry

import org.scalatest._

import org.scalacheck._
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Prop._
import Arbitrary._
import utils.{RelativeDirection => Dir}
import scalaz.syntax.equal._

trait ArbitraryGeometries {

  implicit def arbLTBounds: Arbitrary[LTBounds] = {
    (arbDouble |@| arbDouble |@| arbDouble |@| arbDouble)(
      LTBounds.Doubles.apply
    )
  }
}

object GeometricFigureProperties extends Properties("GeometricFigureProperties") with ArbitraryGeometries {
  property("json <--> LTBounds") = forAll{ (bounds: LTBounds) =>


    true
  }
}

class GeometricFigureTests  extends FlatSpec with Matchers {
  behavior of "Geometric Figures"

  import GeometryImplicits._

  it should "split figures h/v" in {
    val outer = LTBounds.Ints(0, 0, 100, 100)
    val inner = LTBounds.Ints(20, 30, 10, 10)

    inner.adjacentRegion(outer, Dir.Left)
      .foreach { adj =>
        assertResult(LTBounds.Ints(0, 30, 20, 10))(adj)
      }

    inner.adjacentRegion(outer, Dir.Right)
      .foreach { adj =>
        assertResult(LTBounds.Ints(30, 30, 70, 10))(adj)
      }
    inner.adjacentRegion(outer, Dir.Top)
      .foreach { adj =>
        assertResult(LTBounds.Ints(20, 0, 10, 30))(adj)
      }
    inner.adjacentRegion(outer, Dir.Bottom)
      .foreach { adj =>
        assertResult(LTBounds.Ints(20, 40, 10, 60))(adj)
      }
  }
}
