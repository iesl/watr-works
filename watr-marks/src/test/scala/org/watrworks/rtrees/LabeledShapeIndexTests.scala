package org.watrworks
package rtrees

import org.scalacheck._
import org.scalacheck.Prop._

class LabeledShapeIndexTest extends WatrSpec {
  behavior of "LabeledShapeIndex"

}

import geometry._
import TypeTags._

object LabeledShapeIndexProps extends Properties("LabeledShapeIndex") {

  import geometry.ArbitraryGeometry._
  import ArbitraryStuff._


  property("json <--> LabeledShapeIndex") = forAll{ (example: List[LTBounds]) =>

    val rtreeIndex = RTreeIndex.empty[GeometricFigure, Unit, TestShape]()

    example.zipWithIndex.foreach{ case(bbox, i) =>
      val shape = TestShape(bbox, ShapeID(i))
      rtreeIndex.add(shape)
    }

    val shapeIndex = LabeledShapeIndex.withRTree[GeometricFigure, Unit, TestShape](rtreeIndex)

    // val asJson = shapeIndex.asJson

    // println(s"asJson: ${asJson}")

    // val shapeIndexRT = asJson.decodeOrDie[LabeledShapeIndex[GeometricFigure, Unit, TestShape]]("error decoding json")


    val allShapes = shapeIndex.getAllShapes()

    allShapes == rtreeIndex.getItems()

    allShapes.map(_.shape) == example

    // allShapes.length == shapeIndexRT.getAllShapes().length

  }

}
