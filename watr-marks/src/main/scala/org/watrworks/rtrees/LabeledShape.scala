package org.watrworks
package rtrees

import watrmarks._
import geometry._
import geometry.syntax._


trait LabeledShape[+T <: GeometricFigure, W] {
  def id: Int@@ShapeID
  def shape: T
  def labels(): Set[Label]
  def attr: W

  def hasLabel(l: Label) = labels.exists(_ == l)
  def addLabels(l: Label*): LabeledShape[T, W]

  private var _isIndexed: Boolean = false
  def isIndexed(): Boolean = _isIndexed
  def setIndexed(b: Boolean): Unit = _isIndexed = b

  val bounds: LTBounds = minBoundingRect(shape)

}


object LabeledShape {
  type Aux[T <: GeometricFigure, W] = LabeledShape[T, W]


  import _root_.io.circe._

  import TypeTags._

  implicit val Enc_Int_ShapeID: Encoder[Int@@ShapeID] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_ShapeID: Decoder[Int@@ShapeID] = Decoder.decodeInt.map(ShapeID(_))
}
