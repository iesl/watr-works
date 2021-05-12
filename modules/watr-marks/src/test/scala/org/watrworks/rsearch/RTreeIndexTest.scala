package org.watrworks
package rsearch

import geometry._

import watrmarks._
import TypeTags._
import scala.reflect._

case class TestShape[+T <: GeometricFigure: ClassTag](
  shape: T,
  id: Int@@ShapeID,
  labels: Set[Label] = Set[Label]()
) extends LabeledShape[T] {
  val shapeType = implicitly[ClassTag[T]].runtimeClass.getSimpleName()
  def addLabels(l: Label*) = copy(
    labels = this.labels ++ l.toSet
  )
}

class RTreeIndexTest extends WatrSpec {

  behavior of "RTreeIndex"
  type RectShape  = TestShape[Rect]

  it should "index/unindex delete" in {
    val rtreeIndex = RTreeIndex.empty[GeometricFigure, RectShape]()
    val shape = TestShape[Rect](Rect.Ints(0, 0, 1, 1), ShapeID(0))
    rtreeIndex.add(shape)
    println(s"+ shape: ${shape}")

    println("spatialIndex.toString()")
    println(rtreeIndex.spatialIndex.asString())
    rtreeIndex.getItems().size should equal (1)

    rtreeIndex.remove(shape)
    println(s"- shape: ${shape}")

    rtreeIndex.getItems().size should equal (0)

    println("spatialIndex.toString()")
    println(rtreeIndex.spatialIndex.asString())

  }
}
