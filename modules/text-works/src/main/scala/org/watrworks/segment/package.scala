package org.watrworks
package segment

object `package` {
  import geometry._
  import rsearch.LabeledShapeIndex

  type LineShape = DocSegShape[Line]
  type PointShape = DocSegShape[Point]
  type RectShape = DocSegShape[Rect]
  type TrapShape = DocSegShape[Trapezoid]
  type AnyShape = DocSegShape[GeometricFigure]

  type ShapeIndex = LabeledShapeIndex[GeometricFigure, AnyShape]

  val M3 = utils.M3x3Position
  val LB = SegmentationLabels
}
