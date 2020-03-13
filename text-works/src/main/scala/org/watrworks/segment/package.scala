package edu.umass.cs.iesl.watr
package segment

object `package` {
  import geometry._

  type LineShape = DocSegShape[Line]
  type PointShape = DocSegShape[Point]
  type RectShape = DocSegShape[LTBounds]
  type AnyShape = DocSegShape[GeometricFigure]

  val Dir = utils.RelativeDirection
  val LB = SegmentationLabels


}
