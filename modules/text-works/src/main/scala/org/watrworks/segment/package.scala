package org.watrworks
package segment

object `package` {
  import geometry._
  import rtrees.LabeledShapeIndex

  type LineShape = DocSegShape[Line]
  type PointShape = DocSegShape[Point]
  type RectShape = DocSegShape[LTBounds]
  type TrapShape = DocSegShape[Trapezoid]
  type AnyShape = DocSegShape[GeometricFigure]

  type ShapeIndex = LabeledShapeIndex[GeometricFigure, Unit, DocSegShape[GeometricFigure]]

  val Dir = utils.RelativeDirection
  val LB = SegmentationLabels


}
