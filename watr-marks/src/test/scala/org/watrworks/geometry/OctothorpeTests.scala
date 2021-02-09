//
package org.watrworks
package geometry

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import GeometryImplicits._
import utils.{RelativeDirection => Dir}
import utils.GraphPaper

import scalaz._, Scalaz._
import utils.ExactFloats._

import Octothorpe._

case class Octothorpe(
  innerRect: LTBounds,
  outerRect: LTBounds,
  activeRegions: List[Subregion] = List()
) {

  val burstDirections = innerRect.withinRegion(outerRect).burstAllPossibleDirections().toMap

  def activateRegion(dir: Dir): Octothorpe = {
    val box = Box3x3.getCell(dir).toBox()
    activateBox(box)
  }

  def activateRegion(
    dir1: Dir,
    dir2: Dir
  ): Octothorpe = {
    val box = Box3x3
      .getCell(dir1)
      .toBox()
      .union(Box3x3.getCell(dir2).toBox())

    activateBox(box)
  }

  protected def activateBox(box: GraphPaper.Box): Octothorpe = {
    val regionBounds = box
      .getCells()
      .flatMap(cell => {
        val cellDir = CellToDir(cell)
        burstDirections(cellDir)
      })

    val totalBounds = regionBounds.headOption.map(r0 => regionBounds.tail.foldLeft(r0)(_.union(_)))
    this.copy(
      activeRegions = Subregion(box, totalBounds) :: this.activeRegions
    )
  }
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

  def define(bounds: Bounds*): Def = {
    Def(bounds.toList)
  }

  sealed trait Bounds
  object Bounds {
    case class Plane(cell1: Dir, cell2: Dir) extends Bounds
    case class Cell(cell: Dir) extends Bounds
  }
}

case class TrapFeatureRec(
  leftBaseAngle: Float, // quantize around modal values
  upLeftPointCharCase: String // upper/lower
)

class OctothorpeTest extends AnyFlatSpec {
  import GeometryImplicits._
  import GeometryTestUtils._
  import textboxing.{TextBoxing => TB}, TB._

  it should "smokescreen" in {
    // pprint.pprintln(Box3x3)
    // pprint.pprintln(CellToDir)

    val graphSize = LTBounds.Ints(0, 0, 14, 14)
    val outer = LTBounds.Ints(3, 3, 6, 6)
    val inner = LTBounds.Ints(2, 2, 4, 4)

    def drawAdjacencyDiagram(adjacent: LTBounds): Box = {
      val g = makeGraph(graphSize)
      drawBoxDouble(g, outer)
      drawBox(g, inner)
      drawBoxBold(g, adjacent)
      g.asMonocolorString().box
    }
    val octothorpe = Octothorpe(inner, outer)
    val topThorpe = octothorpe.activateRegion(Dir.Right)
    // drawAdjacencyDiagram
    pprint.pprintln(topThorpe)
  }

  it should "specify an octothorpe" in {
    // val octodef = Octothorpe
    //   .init(centerRect => outerRect=(Dir.Top -> centerRect.height*2))
    //   .dirInit(Dir.Center, (r: Rect) => searchForTrapezoids)
    //   .dirInit(Dir.Top, Dir.Bottom, (r: Rect) => searchForTrapezoids)
    //   .dirInit(Dir.Center, Dir.Right, (r: Rect) => searchFor(Rect/LB.CharSpan))
    //)
    // val octo = octodef.centeredOn(trapShape0)
  }
}
