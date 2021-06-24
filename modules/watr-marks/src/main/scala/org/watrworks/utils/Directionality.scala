package org.watrworks
package utils

import _root_.io.circe, circe._

sealed trait M3x3Position {
  import M3x3Position._
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

object M3x3Position {
  case object Top         extends M3x3Position
  case object Bottom      extends M3x3Position
  case object Left        extends M3x3Position
  case object Right       extends M3x3Position
  case object TopLeft     extends M3x3Position
  case object TopRight    extends M3x3Position
  case object BottomLeft  extends M3x3Position
  case object BottomRight extends M3x3Position
  case object Center      extends M3x3Position

  val AllAdjacent: List[M3x3Position] = List(
    Top,
    Bottom,
    Left,
    Right,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight
  )

  val All: List[M3x3Position] = Center :: AllAdjacent

  implicit val M3Encoder: Encoder[M3x3Position] =
    Encoder.encodeString.contramap((dir: M3x3Position) =>
      dir match {
        case Top         => "Top"
        case Bottom      => "Bottom"
        case Left        => "Left"
        case Right       => "Right"
        case TopLeft     => "TopLeft"
        case TopRight    => "TopRight"
        case BottomLeft  => "BottomLeft"
        case BottomRight => "BottomRight"
        case Center      => "Center"
      }
    )

  implicit val M3Decoder: Decoder[M3x3Position] =
    Decoder.decodeString.map(_ match {
      case "Top"         => Top
      case "Bottom"      => Bottom
      case "Left"        => Left
      case "Right"       => Right
      case "TopLeft"     => TopLeft
      case "TopRight"    => TopRight
      case "BottomLeft"  => BottomLeft
      case "BottomRight" => BottomRight
      case "Center"      => Center
    })
}

sealed trait Direction {
  import Direction._

  override def toString(): String = {
    this match {
      case Left  => "Left"
      case Right => "Right"
      case Up    => "Up"
      case Down  => "Down"
    }
  }
}

object Direction {
  val M3 = M3x3Position

  case object Left  extends Direction
  case object Right extends Direction
  case object Up    extends Direction
  case object Down  extends Direction

  val All: List[Direction] = List(
    Left,
    Right,
    Up,
    Down
  )

  implicit val DirEncoder: Encoder[Direction] =
    Encoder.encodeString.contramap(_.toString())

  implicit val DirDecoder: Decoder[Direction] =
    Decoder.decodeString.map(_ match {
      case "Left"  => Left
      case "Right" => Right
      case "Up"    => Up
      case "Down"  => Down
    })

  def toPosition(dir: Direction): M3x3Position = dir match {
    case Left  => M3.Left
    case Right => M3.Right
    case Up    => M3.Top
    case Down  => M3.Bottom
  }

  def toReverse(dir: Direction): Direction = dir match {
    case Left  => Right
    case Right => Left
    case Up    => Down
    case Down  => Up
  }
}
