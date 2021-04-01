package org.watrworks
package geometry

import GeometryImplicits._
import utils.{RelativeDirection => Dir}

/** An octothorpe is a construct to define a geometric focal region (the center
  *  of the grid), and a set of search and/or geometric definitions to define
  *  the regions in the cardinal directions.
  */

case class Octothorpe(
  focalRect: LTBounds,
  horizonRect: LTBounds
) {

  val burstDirections: Map[Dir, Option[LTBounds]] =
    focalRect.withinRegion(horizonRect).burstAllPossibleDirections().toMap

}
object Octothorpe {
  val Box3x3 = GraphPaper
    .boxAt(0, 0)
    .setWidth(3)
    .setHeight(3)

  val CellToDir = Dir.All
    .map(dir => (Box3x3.getCell(dir), dir))
    .toMap

  case class Subregion(box: GraphPaper.Box, rect: Option[LTBounds])

  case class Def(bounds: List[Bounds])

  sealed trait Bounds
  object Bounds {
    case class Plane(cell1: Dir, cell2: Dir) extends Bounds
    case class Cell(cell: Dir)               extends Bounds
  }

  def define(bounds: Bounds*): Def = {
    Def(bounds.toList)
  }
  def focusedOn(odef: Def, rect: LTBounds): Octothorpe = {
    ???
  }

  implicit class RicherOctothrope(val self: Octothorpe) extends AnyVal {
    //
  }

  import scalaz.TreeLoc
  import scalaz.syntax.tree._
  // import scalaz.Tree

  case class Traversal(
    nextfn: Traversal.NextFn,
    loc: TreeLoc[Octothorpe]
  )

  object Traversal {
    type NextFn = Octothorpe => List[Octothorpe]

    def init(nextFn: NextFn, o: Octothorpe): Traversal = {
      Traversal(
        nextFn,
        o.leaf.loc
      )
    }
  }
}

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
