package org.watrworks
package geometry

import utils.{RelativeDirection => Dir}

case class TrapFeatureRec(
  leftBaseAngle: Float, // quantize around modal values
  upLeftPointCharCase: String // upper/lower
)

class OctothorpeTest extends WatrSpec {
  import GeometryTestUtils._
  import textboxing.{TextBoxing => TB}, TB._

  it should "smokescreen" in {
    // pprint.pprintln(Box3x3)
    // pprint.pprintln(CellToDir)

    val _ = s"""|*
                | aaa
                |"""
    val graphSize = Rect.Ints(0, 0, 14, 14)
    val outer = Rect.Ints(3, 3, 6, 6)
    val inner = Rect.Ints(2, 2, 4, 4)

    val traverseFn = (othorpe: Octothorpe) => {

      ???
    }
    // Octothorpe.Traversal.init()

    // def drawAdjacencyDiagram(adjacent: Rect): Box = {
    //   val g = makeGraph(graphSize)
    //   drawBoxDouble(g, outer)
    //   drawBox(g, inner)
    //   drawBoxBold(g, adjacent)
    //   g.asMonocolorString().box
    // }
    // val octothorpe = Octothorpe(inner, outer)
    // // val topThorpe = octothorpe.activateRegion(Dir.Right)
    // // drawAdjacencyDiagram
    // pprint.pprintln(topThorpe)
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
