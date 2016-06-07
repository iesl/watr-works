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
  case object Up extends RelativeDirection
  case object Down extends RelativeDirection
}


// TODO Angular directions Double@@Radians, etc
