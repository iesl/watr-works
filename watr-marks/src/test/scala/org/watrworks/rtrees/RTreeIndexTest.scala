package org.watrworks
package rtrees

import geometry._

import watrmarks._
import TypeTags._

case class TestShape(
  shape: GeometricFigure,
  shapeType: String,
  id: Int@@ShapeID
) extends LabeledShape[GeometricFigure, Unit] {

  def attr: Unit = ()
  def labels = Set()
  def addLabels(l: Label*) = this
}

class RTreeIndexTest extends WatrSpec {

  behavior of "RTreeIndex"

  it should "index/unindex delete" in {
    val rtreeIndex = RTreeIndex.empty[GeometricFigure, Unit, TestShape]()
    val shape = TestShape(LTBounds.Ints(0, 0, 1, 1), "LTBounds", ShapeID(0))
    rtreeIndex.add(shape)

    println(rtreeIndex.spatialIndex.asString())
    rtreeIndex.getItems().size should equal (1)

    rtreeIndex.remove(shape)

    rtreeIndex.getItems().size should equal (0)
    println(rtreeIndex.spatialIndex.asString())

  }
}
