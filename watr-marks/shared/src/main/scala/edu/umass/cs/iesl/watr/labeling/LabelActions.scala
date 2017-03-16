package edu.umass.cs.iesl.watr
package labeling

import geometry._

sealed trait Constraint

case object ByLine extends Constraint
case object ByChar extends Constraint
case object ByRegion extends Constraint


sealed trait UIAction

case object Create extends UIAction
case object Delete extends UIAction


final case class UIChange(
  action: UIAction,
  visual: Option[GeometricGroup]
)

