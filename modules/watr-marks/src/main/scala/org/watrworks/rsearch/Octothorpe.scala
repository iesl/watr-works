package org.watrworks
package rsearch

import geometry._
import geometry.syntax._
import utils.ExactFloats._

import utils.{RelativeDirection => Dir}

import io.circe
import _root_.io.circe, circe.syntax._, circe._
import io.circe.generic._

/** An octothorpe is a 2D-plane search primitive.
  *  It consists of a focal region (the center
  *  of the grid), and a set of search and/or geometric definitions to define
  *  the regions in the cardinal directions.
  */

import GeometryCodecs._

@JsonCodec
case class Octothorpe(
  focalRect: Rect,
  horizonRect: Rect,
  searchBounds: List[Octothorpe.Bounds]
) {

  import Octothorpe._
  import Bounds._

  val burstDirections: Map[Dir, Option[Rect]] = focalRect
    .withinRegion(horizonRect)
    .burstAllPossibleDirections()
    .map({ case (dir, maybeRect) =>
      val shavedRect = maybeRect.map(r => {
        dir match {
          case Dir.Top         => r.shave(Dir.Bottom, FloatExact.epsilon)
          case Dir.Bottom      => r.shave(Dir.Top, FloatExact.epsilon)
          case Dir.Right       => r.shave(Dir.Left, FloatExact.epsilon)
          case Dir.Left        => r.shave(Dir.Right, FloatExact.epsilon)
          case Dir.TopLeft     => r.shave(Dir.BottomRight, FloatExact.epsilon)
          case Dir.BottomLeft  => r.shave(Dir.TopRight, FloatExact.epsilon)
          case Dir.TopRight    => r.shave(Dir.BottomLeft, FloatExact.epsilon)
          case Dir.BottomRight => r.shave(Dir.TopLeft, FloatExact.epsilon)
          case Dir.Center      => r
        }
      })

      (dir, shavedRect)
    })
    .toMap

  def searchAreas[Shape <: LabeledShape[GeometricFigure]](): Seq[(Bounds, Rect)] =
    for {
      sbound <- searchBounds
      found <- sbound match {
                 case Cell(dir) =>
                   for {
                     cellRectOpt <- burstDirections.get(dir)
                     cellRect    <- cellRectOpt
                   } yield (sbound, cellRect)

                 case CellSpan(d1, d2) =>
                   for {
                     cellRectOpt1 <- burstDirections.get(d1)
                     cellRectOpt2 <- burstDirections.get(d2)
                     cellRect1    <- cellRectOpt1
                     cellRect2    <- cellRectOpt2
                     searchRect = cellRect1.union(cellRect2)
                   } yield (sbound, searchRect)
               }
    } yield found

  def runSearch[Shape <: LabeledShape[GeometricFigure]](
    searchFunc: (Rect) => Seq[Shape]
  ): Seq[Shape] = (for {
    (sbound, searchArea) <- searchAreas()
  } yield searchFunc(searchArea)).flatten

}

object Octothorpe {
  type ShapeT = LabeledShape[GeometricFigure]

  type SearchFunc = (Rect) => Seq[ShapeT]

  def withSearchRegions(
    bs: Bounds*
  ): RegionDefs = {
    RegionDefs(bs.to(List))
  }

  case class RegionDefs(
    bounds: List[Bounds]
  ) {
    def withHorizon(rect: Rect): BoundedRegionDefs =
      BoundedRegionDefs(rect, this)
  }

  case class BoundedRegionDefs(
    horizon: Rect,
    regionDefs: RegionDefs
  ) {

    def centeredOn(rect: Rect): Octothorpe = {
      Octothorpe(rect, horizon, regionDefs.bounds)
    }
  }

  sealed trait Bounds
  object Bounds {

    @JsonCodec
    case class CellSpan(cell1: Dir, cell2: Dir) extends Bounds

    @JsonCodec
    case class Cell(cell: Dir) extends Bounds

    implicit val BoundsEncoder: Encoder[Bounds] = Encoder.instance {
      case v: Bounds.CellSpan => v.asJson
      case v: Bounds.Cell     => v.asJson
    }

    implicit val BoundsDecoder: Decoder[Bounds] = new Decoder[Bounds] {
      def apply(c: HCursor): Decoder.Result[Bounds] = {
        ???
      }
    }
  }

  def cell(d: Dir): Bounds.Cell  = Bounds.Cell(d)
  def cellspan(d1: Dir, d2: Dir) = Bounds.CellSpan(d1, d2)

}
