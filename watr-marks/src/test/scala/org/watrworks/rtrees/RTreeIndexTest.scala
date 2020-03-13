package edu.umass.cs.iesl.watr
package rtrees

import scalaz.{@@ => _, _}, Scalaz._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

import geometry._
import geometry.syntax._
import geometry.GeometryCodecs._

import _root_.io.circe
import circe._
import circe.syntax._
// import circe.generic.auto._
import circe.generic._

import watrmarks._
import TypeTags._
import utils.DoOrDieHandlers._
import LabeledShape._

@JsonCodec
case class TestShape(
  shape: GeometricFigure,
  id: Int@@ShapeID
) extends LabeledShape[GeometricFigure, Unit] {

  def attr: Unit = ()
  def labels() = Set()
  def addLabels(l: Label*) = this
}

object TestShape {

  // implicit def TestShapeEncoder: Encoder[TestShape] = Encoder.instance[TestShape]{ shape =>
  //   Json.obj(
  //     "id" := shape.id,
  //     "figure" := shape.shape,
  //   )
  // }

  // implicit def TestShapeDecoder: Decoder[TestShape] = Decoder.instance[TestShape]{ cursor =>
  //   for {
  //     id <- cursor.downField("id").as[Int]
  //     fig <- cursor.downField("figure").as[GeometricFigure]
  //   } yield {
  //     TestShape(fig, ShapeID(id))
  //   }
  // }

}

class RTreeIndexTest extends AnyFlatSpec with Matchers {

  behavior of "RTreeIndex"

  it should "index/unindex delete" in {
    val rtreeIndex = RTreeIndex.empty[GeometricFigure, Unit, TestShape]()
    val shape = TestShape(LTBounds.Ints(0, 0, 1, 1), ShapeID(0))
    rtreeIndex.add(shape)

    println(rtreeIndex.spatialIndex.asString())
    rtreeIndex.getItems.size should equal (1)

    rtreeIndex.remove(shape)

    rtreeIndex.getItems.size should equal (0)
    println(rtreeIndex.spatialIndex.asString())

  }


  it should "serialize to/from JSON" in {
    val rtreeIndex = RTreeIndex.empty[GeometricFigure, Unit, TestShape]()

    val jsval = rtreeIndex.asJson
    // println(s"as js: \n${rtreeIndex.asJson}")

    val shape = TestShape(LTBounds.Ints(0, 0, 1, 1), ShapeID(0))
    rtreeIndex.add(shape)
    // println(s"as js: \n${rtreeIndex.asJson}")
  }
}


object RTreeIndexProps extends Properties("RTreeIndex") {

  import ArbitraryGeometry._
  import RTreeIndex._
  import ArbitraryStuff._

  property("json <--> RTreeIndex") = forAll{ (example: List[LTBounds]) =>
    val rtreeIndex = RTreeIndex.empty[GeometricFigure, Unit, TestShape]()

    println("len: " + example.length)

    example.zipWithIndex.foreach{ case(bbox, i) =>
      val shape = TestShape(bbox, ShapeID(i))
      rtreeIndex.add(shape)
    }

    val rtreeAsJson = rtreeIndex.asJson
    val rt = rtreeAsJson.decodeOrDie[RTreeIndex[GeometricFigure, Unit, TestShape]]("invalid json")

    val initItems= rtreeIndex.getItems()
    val rtItems = rt.getItems()

    rtItems == initItems

  }

}
