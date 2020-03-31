package org.watrworks
package geometry

import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

import scalaz.{@@ => _, _}, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._


import geometry.syntax._
import utils.DoOrDieHandlers._
import _root_.io.circe, circe.syntax._


object ArbitraryGeometry {
  import Arbitrary._

  import utils.ExactFloats._

  implicit def Arb_FloatExact: Arbitrary[Int@@FloatRep] = {
    arbDouble.map(_.toFloatExact())
  }

  def Gen_LTBounds: Gen[LTBounds] = for {
    d1 <- arbitrary[Double]
    d2 <- arbitrary[Double]
    d3 <- arbitrary[Double]
    d4 <- arbitrary[Double]
  } yield LTBounds.Doubles.apply(d1, d2, d3, d4)

  def Gen_Point: Gen[Point] = for {
    d1 <- arbitrary[Double]
    d2 <- arbitrary[Double]
  } yield Point.Doubles.apply(d1, d2)

  def Gen_Line: Gen[Line] = {
    for {
      p1 <- arbitrary[Point]
      p2 <- arbitrary[Point]
    } yield Line(p1, p2)
  }

  def Gen_Trapezoid: Gen[Trapezoid] =  for {
    topLeft <- arbitrary[Point]
    bottomLeft <- arbitrary[Point]
    topW <- arbitrary[Int]
    bottomW <- arbitrary[Int]
  } yield Trapezoid(topLeft, topW.toFloatExact(), bottomLeft, bottomW.toFloatExact)

  implicit def Arb_LTBounds: Arbitrary[LTBounds] =  Arbitrary(Gen_LTBounds)
  implicit def Arb_Point: Arbitrary[Point] = Arbitrary(Gen_Point)
  implicit def Arb_Line: Arbitrary[Line] = Arbitrary(Gen_Line)
  implicit def Arb_Trapezoid: Arbitrary[Trapezoid] = Arbitrary(Gen_Trapezoid)

  implicit def Arb_Figure: Arbitrary[GeometricFigure] = {
    Arbitrary{
      Gen.oneOf(
        Gen_Line,
        Gen_Point,
        Gen_LTBounds,
        Gen_Trapezoid
      )
    }
  }
}

object GeometryChecks extends Properties("GeometricFigures") {

  import ArbitraryGeometry._
  import GeometryCodecs._

  property("json <--> LTBounds") = forAll{ (example: LTBounds) =>
    example.asJson.decodeOrDie[LTBounds]() === example
  }

  property("json <--> Point") = forAll{ (example: Point) =>
    example.asJson.decodeOrDie[Point]() === example
  }

  property("json <--> Line") = forAll{ (example: Line) =>
    example.asJson.decodeOrDie[Line]() === example
  }

  property("json <--> Trapezoid") = forAll{ (example: Trapezoid) =>
    example.asJson.decodeOrDie[Trapezoid]() === example
  }

  property("json <--> GeometricFigure") = forAll{ (example: GeometricFigure) =>
    // val js = example.asJson
    // println(s"example: ${example} as Json: ${js}")
    example.asJson.decodeOrDie[GeometricFigure]() === example
  }
}

object ArbitaryPageComponents {
  import Arbitrary._
  import TypeTags._
  import ArbitraryGeometry._

  def Gen_StableDocument: Gen[StableDocument] = for {
    s <- arbitrary[String]
  } yield StableDocument(DocumentID(s))

  implicit def Arb_StableDocument: Arbitrary[StableDocument] = Arbitrary(Gen_StableDocument)

  def Gen_StablePage: Gen[StablePage] = for {
    s <- arbitrary[String]
    n <- arbitrary[Int]
  } yield StablePage(DocumentID(s), PageNum(n))

  implicit def Arb_StablePage: Arbitrary[StablePage] = Arbitrary(Gen_StablePage)

  def Gen_PageRegion: Gen[PageRegion] = for {
    s <- arbitrary[StablePage]
    b <- arbitrary[LTBounds]
  } yield PageRegion(s, b)

  implicit def Arb_PageRegion: Arbitrary[PageRegion] = Arbitrary(Gen_PageRegion)

  def Gen_PageGeometry: Gen[PageGeometry] = for {
    n <- arbitrary[Int]
    b <- arbitrary[LTBounds]
  } yield PageGeometry(PageNum(n), b)

  implicit def Arb_PageGeometry: Arbitrary[PageGeometry] = Arbitrary(Gen_PageGeometry)



  def Gen_PageItem_Path: Gen[PageItem.Path] = Gen.sized{ size =>
    for {
      p <- arbitrary[PageRegion]
      points <- Gen.listOf[Point](Gen_Point)
    } yield PageItem.Path(p, points)
  }

  def Gen_PageItem_ImageAtom: Gen[PageItem.ImageAtom] = Gen.sized{ size =>
    for {
      p <- arbitrary[PageRegion]
    } yield PageItem.ImageAtom(p)
  }

  def Gen_PageItem_CharAtom: Gen[PageItem.CharAtom] = Gen.sized{ size =>
    for {
      n <- arbitrary[Int]
      c <- arbitrary[Char]
      p <- arbitrary[PageRegion]
    } yield PageItem.CharAtom(CharID(n), p, c.toString)
  }

  def Gen_PageItem: Gen[PageItem] = Gen.oneOf(
    Gen_PageItem_Path,
    Gen_PageItem_ImageAtom,
    Gen_PageItem_CharAtom
  )

  implicit def Arb_PageItem_Path: Arbitrary[PageItem.Path] = Arbitrary(Gen_PageItem_Path)
  implicit def Arb_PageItem_CharAtom: Arbitrary[PageItem.CharAtom] = Arbitrary(Gen_PageItem_CharAtom)
  implicit def Arb_PageItem_ImageAtom: Arbitrary[PageItem.ImageAtom] = Arbitrary(Gen_PageItem_ImageAtom)
  implicit def Arb_PageItem: Arbitrary[PageItem] = Arbitrary(Gen_PageItem)


}

object PageComponentProps extends Properties("PageComponents") {

  import ArbitaryPageComponents._

  property("json <--> StableDocument") = forAll{ (example: StableDocument) =>
    example.asJson.decodeOrDie[StableDocument]() == example
  }

  property("json <--> StablePage") = forAll{ (example: StablePage) =>
    example.asJson.decodeOrDie[StablePage]() == example
  }

  property("json <--> PageRegion") = forAll{ (example: PageRegion) =>
    example.asJson.decodeOrDie[PageRegion]() == example
  }

  property("json <--> PageGeometry") = forAll{ (example: PageGeometry) =>
    example.asJson.decodeOrDie[PageGeometry]() == example
  }

  property("json <--> PageItem.Path") = forAll{ (example: PageItem.Path) =>
    example.asJson.decodeOrDie[PageItem.Path]() == example
  }
  property("json <--> PageItem.ImageAtom") = forAll{ (example: PageItem.ImageAtom) =>
    example.asJson.decodeOrDie[PageItem.ImageAtom]() == example
  }

  property("json <--> PageItem.CharAtom") = forAll{ (example: PageItem.CharAtom) =>
    example.asJson.decodeOrDie[PageItem.CharAtom]() == example
  }

  property("json <--> PageItem") = forAll{ (example: PageItem) =>
    example.asJson.decodeOrDie[PageItem]() == example
  }
}
