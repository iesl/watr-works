package org.watrworks
package rsearch 

import watrmarks._
import geometry._
import geometry.syntax._


trait LabeledShape[+T <: GeometricFigure] {
  def id: Int@@ShapeID
  def shape: T
  def shapeType: String
  def labels: Set[Label]
  // def attr: W

  def hasLabel(l: Label) = labels.exists(_ == l)
  def addLabels(l: Label*): LabeledShape[T]

  private var _isIndexed: Boolean = false
  def isIndexed(): Boolean = _isIndexed
  def setIndexed(b: Boolean): Unit = _isIndexed = b

  val minBounds: Rect = minBoundingRect(shape)
}


object LabeledShape {
  type Aux[T <: GeometricFigure] = LabeledShape[T]
  //   implicit val shapeIdCodec = TypeTagCodecs.intCodec[ShapeID]
}
