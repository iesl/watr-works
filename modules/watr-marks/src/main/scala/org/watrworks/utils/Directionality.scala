package org.watrworks
package utils

import _root_.io.circe, circe._

sealed trait RelativeDirection {
  import RelativeDirection._
  override def toString(): String = {
    this match {
      case Top         => "Top"
      case Bottom      => "Bottom"
      case TopLeft     => "TopLeft"
      case TopRight    => "TopRight"
      case BottomLeft  => "BottomLeft"
      case BottomRight => "BottomRight"
      case Center      => "Center"
      case Left        => "Left"
      case Right       => "Right"
    }
  }
}

sealed trait Orientation

object RelativeDirection {
  case object Top         extends RelativeDirection
  case object Bottom      extends RelativeDirection
  case object TopLeft     extends RelativeDirection
  case object TopRight    extends RelativeDirection
  case object BottomLeft  extends RelativeDirection
  case object BottomRight extends RelativeDirection
  case object Center      extends RelativeDirection

  case object Left  extends RelativeDirection with Orientation
  case object Right extends RelativeDirection with Orientation

  case object Up   extends Orientation
  case object Down extends Orientation

  val AllAdjacent: List[RelativeDirection] = List(
    Left,
    Right,
    Top,
    Bottom,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight
  )

  val All: List[RelativeDirection] = Center :: AllAdjacent

  implicit val DirEncoder: Encoder[RelativeDirection] =
    Encoder.encodeString.contramap((dir: RelativeDirection) =>
      dir match {
        case Top         => "Top"
        case Bottom      => "Bottom"
        case TopLeft     => "TopLeft"
        case TopRight    => "TopRight"
        case BottomLeft  => "BottomLeft"
        case BottomRight => "BottomRight"
        case Center      => "Center"
        case Left        => "Left"
        case Right       => "Right"
      }
    )

  implicit val DirDecoder: Decoder[RelativeDirection] =
    Decoder.decodeString.map(_ match {
      case "Top"         => Top
      case "Bottom"      => Bottom
      case "TopLeft"     => TopLeft
      case "TopRight"    => TopRight
      case "BottomLeft"  => BottomLeft
      case "BottomRight" => BottomRight
      case "Center"      => Center
      case "Left"        => Left
      case "Right"       => Right
    })
}
