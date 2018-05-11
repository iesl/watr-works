package edu.umass.cs.iesl.watr
package geometry

import org.scalacheck._
// import org.scalacheck.Gen
import org.scalacheck.Arbitrary
// import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._

import scalaz.{@@ => _, _}, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._


import geometry.syntax._
import utils.DoOrDieHandlers._
import _root_.io.circe, circe.syntax._


object ArbitraryGeometry {
  import Arbitrary._


  implicit def Arb_LTBounds: Arbitrary[LTBounds] = {
    (arbDouble |@| arbDouble |@| arbDouble |@| arbDouble)(
      LTBounds.Doubles.apply
    )
  }
}

object GeometryChecks extends Properties("GeometricFigures") {

  import ArbitraryGeometry._
  import GeometryCodecs._

  property("json <--> LTBounds") = forAll{ (example: LTBounds) =>
    example.asJson.decodeOrDie[LTBounds]() === example
  }

}
