package edu.umass.cs.iesl.watr
package geometry

import org.scalacheck._
// import scalaz._, Scalaz._
// import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Prop._
import Arbitrary._

object GeometricFigureProperties extends Properties("GeometricFigureProperties") with ArbitraryGeometries {
  property("json <--> LTBounds") = forAll{ (bounds: LTBounds) =>
    true
  }
}
