package org.watrworks
package rtrees

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

class LabeledShapeIndexTest extends AnyFlatSpec {

  behavior of "LabeledShapeIndex"


}

import geometry._
import geometry.syntax._
import TypeTags._

object LabeledShapeIndexProps extends Properties("LabeledShapeIndex") {

  import geometry.ArbitraryGeometry._
  import RTreeIndex._
  import ArbitraryStuff._
  import LabeledShapeIndex._
  import TestShape._
  import utils.DoOrDieHandlers._

  import _root_.io.circe
  import circe._
  import circe.syntax._


  property("json <--> LabeledShapeIndex") = forAll{ (example: List[LTBounds]) =>

    val rtreeIndex = RTreeIndex.empty[GeometricFigure, Unit, TestShape]()

    example.zipWithIndex.foreach{ case(bbox, i) =>
      val shape = TestShape(bbox, ShapeID(i))
      rtreeIndex.add(shape)
    }

    val shapeIndex = LabeledShapeIndex.withRTree[GeometricFigure, Unit, TestShape](rtreeIndex)

    val asJson = shapeIndex.asJson

    // println(s"asJson: ${asJson}")

    val shapeIndexRT = asJson.decodeOrDie[LabeledShapeIndex[GeometricFigure, Unit, TestShape]]("error decoding json")

    shapeIndex.getAllShapes == rtreeIndex.getItems

    shapeIndex.getAllShapes.map(_.shape) == example

    shapeIndex.getAllShapes.length == shapeIndexRT.getAllShapes().length

  }

}
