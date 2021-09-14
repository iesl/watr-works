package org.watrworks
package segment

object `package` {
  import geometry._
  import prelude._
  import rsearch.LabeledShapeIndex

  type LineShape = DocSegShape[Line]
  type PointShape = DocSegShape[Point]
  type RectShape = DocSegShape[Rect]
  type TrapShape = DocSegShape[Trapezoid]
  type AnyShape = DocSegShape[GeometricFigure]
  type Shape[F <: GeometricFigure] = DocSegShape[F]

  type ShapeIndex = LabeledShapeIndex[GeometricFigure, AnyShape]

  val M3 = utils.M3x3Position
  val LB = SegmentationLabels
}
