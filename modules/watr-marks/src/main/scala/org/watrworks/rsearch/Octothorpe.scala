package org.watrworks
package rsearch

import geometry._
import GeometryImplicits._
import utils.{RelativeDirection => Dir}

/** An octothorpe is a 2D-plane search primitive.
  *  It consists of a focal region (the center
  *  of the grid), and a set of search and/or geometric definitions to define
  *  the regions in the cardinal directions.
  */

// case class Octothorpe[A <: GeometricFigure, Shape <: LabeledShape[A]] (
case class Octothorpe(
  focalRect: Rect,
  horizonRect: Rect,
  searchBounds: List[Octothorpe.Bounds]
) {

  import Octothorpe._
  import Bounds._

  val burstDirections: Map[Dir, Option[Rect]] =
    focalRect.withinRegion(horizonRect).burstAllPossibleDirections().toMap

  def runSearch[Shape <: LabeledShape[GeometricFigure]](
    searchFunc: (Rect) => Seq[Shape]
  ): Seq[Shape] = {
    val f2: List[Seq[Shape]] = searchBounds.map(sbound => {
      val found = sbound match {
        case Cell(dir) =>
          for {
            cellRectOpt <- burstDirections.get(dir)
            cellRect    <- cellRectOpt
          } yield searchFunc(cellRect)

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

    def focusedOn(rect: Rect): Octothorpe = {
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

// case class Def(bounds: List[Bounds])
// def define(bounds: Bounds*): Def = {
//   Def(bounds.toList)
// }

// def focusedOn(odef: Def, rect: Rect): Octothorpe = {
//   ???
// }

// import scalaz.TreeLoc
// import scalaz.syntax.tree._
// // import scalaz.Tree

// case class Traversal(
//   nextfn: Traversal.NextFn,
//   loc: TreeLoc[Octothorpe]
// )

// object Traversal {
//   type NextFn = Octothorpe => List[Octothorpe]

//   def init(nextFn: NextFn, o: Octothorpe): Traversal = {
//     Traversal(
//       nextFn,
//       o.leaf.loc
//     )
//   }
// }

// val Box3x3 = GraphPaper
//   .boxAt(0, 0)
//   .setWidth(3)
//   .setHeight(3)
// val CellToDir = Dir.All
//   .map(dir => (Box3x3.getCell(dir), dir))
//   .toMap
// case class Subregion(box: GraphPaper.Box, rect: Option[Rect])
// activeRegions: List[Subregion] = List()
// def activateRegion(dir: Dir): Octothorpe = {
//   val box = Box3x3.getCell(dir).toBox()
//   activateBox(box)
// }

// def activateRegion(
//   dir1: Dir,
//   dir2: Dir
// ): Octothorpe = {
//   val box = Box3x3
//     .getCell(dir1)
//     .toBox()
//     .union(Box3x3.getCell(dir2).toBox())

//   activateBox(box)
// }

// protected def activateBox(box: GraphPaper.Box): Octothorpe = {
//   val regionBounds = box
//     .getCells()
//     .flatMap(cell => {
//       val cellDir = CellToDir(cell)
//       burstDirections(cellDir)
//     })

//   val totalBounds = regionBounds.headOption.map(r0 => regionBounds.tail.foldLeft(r0)(_.union(_)))
//   this.copy(
//     activeRegions = Subregion(box, totalBounds) :: this.activeRegions
//   )
// }
