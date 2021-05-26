package org.watrworks
package segment

import geometry._

import watrmarks.Label
import rsearch._
import scala.reflect._

case class DocSegShape[+T <: GeometricFigure: ClassTag](
  id: Int @@ ShapeID,
  pageNum: Int @@ PageNum,
  shape: T,
  _init: Set[Label]
) extends LabeledShape[T] {
  val shapeType = implicitly[ClassTag[T]].runtimeClass.getSimpleName()
  var _labels: Set[Label] = _init
  override def labels = _labels


  def addLabels(ls: Label*): DocSegShape[T] = {
    _labels = _labels ++ ls
    this
  }
}

object DocSegShape {
  def create[T <: GeometricFigure: ClassTag](
    id: Int @@ ShapeID,
    pageNum: Int @@ PageNum,
    shape: T,
    labels: Label*
  ) = DocSegShape[T](
    id,
    pageNum,
    shape,
    labels.toSet
  )


  implicit class SS_LabeledShapeCoercion[+A <: GeometricFigure](val theShape: DocSegShape[A]) {
    def asLineShape: LineShape   = theShape.asInstanceOf[LineShape]
    def asPointShape: PointShape = theShape.asInstanceOf[PointShape]
    def asRectShape: RectShape   = theShape.asInstanceOf[RectShape]
    def asTrapShape: TrapShape   = theShape.asInstanceOf[TrapShape]
  }

  implicit class RicherLabeledShapes[A <: GeometricFigure](
    val theShapes: Seq[ShapeIndex#AnyShape]
  ) {
    def asLineShapes: Seq[LineShape]   = theShapes.asInstanceOf[Seq[LineShape]]
    def asPointShapes: Seq[PointShape] = theShapes.asInstanceOf[Seq[PointShape]]
    def asRectShapes: Seq[RectShape]   = theShapes.asInstanceOf[Seq[RectShape]]
    def asTrapShapes: Seq[TrapShape]   = theShapes.asInstanceOf[Seq[TrapShape]]
  }
}

