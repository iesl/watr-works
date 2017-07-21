package edu.umass.cs.iesl.watr
package utils


sealed trait CompassDirection

object CompassDirection {
  case object N extends CompassDirection
  case object S extends CompassDirection
  case object E extends CompassDirection
  case object W extends CompassDirection

  case object NW extends CompassDirection
  case object SW extends CompassDirection
  case object NE extends CompassDirection
  case object SE extends CompassDirection
}


sealed trait RelativeDirection

object RelativeDirection {
  case object Left extends RelativeDirection
  case object Right extends RelativeDirection
  case object Top extends RelativeDirection
  case object Bottom extends RelativeDirection
  case object TopLeft extends RelativeDirection
  case object TopRight extends RelativeDirection
  case object BottomLeft extends RelativeDirection
  case object BottomRight extends RelativeDirection
  case object Center extends RelativeDirection
}
