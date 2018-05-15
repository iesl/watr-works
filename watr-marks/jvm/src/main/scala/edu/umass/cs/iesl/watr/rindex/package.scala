package edu.umass.cs.iesl.watr
package rindex

object `package` {

  import geometry._

  type LineShape = LabeledShape[Line]
  type PointShape = LabeledShape[Point]
  type RectShape = LabeledShape[LTBounds]
  type AnyShape = LabeledShape[GeometricFigure]

}
