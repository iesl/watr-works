package org.watrworks
package rsearch

import geometry._
import geometry.syntax._
import utils.ExactFloats._

import utils.{M3x3Position => M3}

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

  val burstDirections: Map[M3, Option[Rect]] = focalRect
    .withinRegion(horizonRect)
    .burstAllPossibleDirections()
    .map({ case (dir, maybeRect) =>
      val shavedRect = maybeRect.map(r => {
        dir match {
          case M3.Top         => r.shave(M3.Bottom, FloatExact.epsilon)
          case M3.Bottom      => r.shave(M3.Top, FloatExact.epsilon)
          case M3.Right       => r.shave(M3.Left, FloatExact.epsilon)
          case M3.Left        => r.shave(M3.Right, FloatExact.epsilon)
          case M3.TopLeft     => r.shave(M3.BottomRight, FloatExact.epsilon)
          case M3.BottomLeft  => r.shave(M3.TopRight, FloatExact.epsilon)
          case M3.TopRight    => r.shave(M3.BottomLeft, FloatExact.epsilon)
          case M3.BottomRight => r.shave(M3.TopLeft, FloatExact.epsilon)
          case M3.Center      => r
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
    case class CellSpan(cell1: M3, cell2: M3) extends Bounds

    @JsonCodec
    case class Cell(cell: M3) extends Bounds

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

  def cell(d: M3): Bounds.Cell  = Bounds.Cell(d)
  def cellspan(d1: M3, d2: M3) = Bounds.CellSpan(d1, d2)

}
