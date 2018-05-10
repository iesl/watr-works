package edu.umass.cs.iesl.watr
package utils

sealed trait RelativeDirection

sealed trait Orientation

object RelativeDirection {
  case object Top         extends RelativeDirection
  case object Bottom      extends RelativeDirection
  case object TopLeft     extends RelativeDirection
  case object TopRight    extends RelativeDirection
  case object BottomLeft  extends RelativeDirection
  case object BottomRight extends RelativeDirection
  case object Center      extends RelativeDirection

  case object Left        extends RelativeDirection with Orientation
  case object Right       extends RelativeDirection with Orientation

  case object Up          extends Orientation
  case object Down        extends Orientation

  val AllAdjacent: List[RelativeDirection] = List(
    Left        ,
    Right       ,
    Top         ,
    Bottom      ,
    TopLeft     ,
    TopRight    ,
    BottomLeft  ,
    BottomRight
  )

  val All: List[RelativeDirection] = Center :: AllAdjacent
}


// object Orientation {
//   case object Left       extends Orientation
//   case object Right      extends Orientation
//   case object Up         extends Orientation
//   case object Down       extends Orientation
// }
