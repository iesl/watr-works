package org.watrworks
package rsearch

import geometry._
import geometry.syntax._
import utils.ExactFloats._

import utils.{RelativeDirection => Dir}

/** An octothorpe is a 2D-plane search primitive.
  *  It consists of a focal region (the center
  *  of the grid), and a set of search and/or geometric definitions to define
  *  the regions in the cardinal directions.
  */

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
      // dir match {
      //   case Dir.Top         =>
      //   case Dir.Bottom      => // shave bottom
      //   case Dir.Right       =>
      //   case Dir.Left        =>
      //   case Dir.TopLeft     =>
      //   case Dir.BottomLeft  =>
      //   case Dir.TopRight    =>
      //   case Dir.BottomRight =>
      //   case Dir.Center      =>
      // }

      // TODO this should shave off parts of the regions based on Dir, not just uniformly
      (dir, maybeRect.map(_.shave(FloatRep(1))))
    })
    .toMap

  def runSearch[Shape <: LabeledShape[GeometricFigure]](
    searchFunc: (Rect) => Seq[Shape]
  ): Seq[Shape] = {
    val f2: List[Seq[Shape]] = searchBounds.map(sbound => {
      val found = sbound match {
        case Cell(dir) =>
          for {
            cellRectOpt <- burstDirections.get(dir)
            cellRect    <- cellRectOpt
          } yield {
            searchFunc(cellRect)
          }

        case CellSpan(d1, d2) =>
          for {
            cellRectOpt1 <- burstDirections.get(d1)
            cellRectOpt2 <- burstDirections.get(d2)
            cellRect1    <- cellRectOpt1
            cellRect2    <- cellRectOpt2
            searchRect = cellRect1.union(cellRect2)
          } yield searchFunc(searchRect)
      }
      found.getOrElse(List.empty)
    })

    f2.flatten
  }
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
    case class CellSpan(cell1: Dir, cell2: Dir) extends Bounds
    case class Cell(cell: Dir)                  extends Bounds
  }

  def cell(d: Dir): Bounds.Cell  = Bounds.Cell(d)
  def cellspan(d1: Dir, d2: Dir) = Bounds.CellSpan(d1, d2)

}
