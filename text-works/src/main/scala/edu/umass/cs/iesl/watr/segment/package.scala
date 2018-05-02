package edu.umass.cs.iesl.watr
package segment

object `package` {
  import spindex.LabeledShape
  import geometry._

  type LineShape = LabeledShape[Line]
  type PointShape = LabeledShape[Point]
  type RectShape = LabeledShape[LTBounds]
  type AnyShape = LabeledShape[GeometricFigure]

  val Dir = utils.RelativeDirection
  val LB = SegmentationLabels


}
