package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

trait ConnectedComponentTestUtil extends Matchers {
  import utils.IdGenerator
  import TypeTags._
  import GeometricFigure._
  import EnrichGeometricFigures._
  import watrmarks.{StandardLabels => LB}
  import edu.umass.cs.iesl.watr.watrmarks.Label

  // import scalaz.@@
  val regionIDs = IdGenerator[RegionID]()
  // val pageIDs = IdGenerator[PageID]()

  val page0 = PageID(0)
  val xscale = 10.0d
  val yscale = 10.0d

  def stringToPageAtoms(str: String): (Seq[PageAtom], PageGeometry) = {

    val atoms = str.split("\n")
      .map(_.trim).zipWithIndex
      .map({ case (line, linenum) =>
        line.zipWithIndex
          .filterNot(_._1 == ' ')
          .map({ case (ch, chnum) =>
            CharAtom(
              TargetRegion(regionIDs.nextId, page0,
                LTBounds(
                  left=chnum*xscale, top=linenum*yscale,
                  width=xscale, height=yscale
                )
              ),
              ch.toString
            )
          })
      })
      .flatten.toSeq

    val maxX = atoms.map(_.region.bbox.right).max
    val maxY = atoms.map(_.region.bbox.bottom).max


    val pageGeom = PageGeometry(
      PageID(0), LTBounds(
        left=0, top=0,
        width=maxX, height=maxY
      )
    )


    (atoms, pageGeom)
  }

  def queryComponents(pageInfo: PageInfo, x: Int, y: Int, w: Int, h: Int): Seq[Component] = {
    val q = LTBounds(x*xscale, y*yscale, w*xscale, h*xscale)
    pageInfo.componentIndex.queryForContained(q)
  }

  def labelRow(zoneIndex: ZoneIndexer, row: Int, l: Label): Option[RegionComponent] = {
    val pageInfo = zoneIndex.getPageInfo(page0)
    val q = LTBounds(0, row*yscale, Int.MaxValue, yscale)
    val charAtoms = pageInfo.componentIndex.queryForContained(q)
    val reg = zoneIndex.labelRegion(charAtoms, l)
    // assert each region can select its contained page atoms
    reg.foreach { rc =>
      val patoms = rc.queryInside(LB.PageAtom)
      assertResult(charAtoms.toSet){
        patoms.toSet
      }
    }
    reg
  }

  def createZoneIndexer(str: String): ZoneIndexer = {
    val (atoms, geom) = stringToPageAtoms(str)
    ZoneIndexer.loadSpatialIndices2(Seq((atoms -> geom)))
  }
}


class ConnectedComponentTest extends FlatSpec with ConnectedComponentTestUtil {
  import ComponentRendering.VisualLine

  behavior of "connected components"

  import watrmarks.{StandardLabels => LB}



  it should "demonstrate sup/subscript labeling" in {
    val zoneIndex = createZoneIndexer(
      // 012 3 4567890
      """Eu1 - xBixVO4"""
    )

    //  when ccs are 'connected' they query is cached such that future lookups return the ordered children
    for { row  <- labelRow(zoneIndex, 0, LB.VisualLine) } {

      row.connectChildren(LB.PageAtom, Some(_.bounds.left))

      // Create a CC w/same target region, same atom sorting, linked as child to row
      val textSpanRegion = row.cloneAs(LB.TextSpan)

      row.setChildren(LB.TextSpan, Seq(textSpanRegion))
      row.addLabel(LB.Tokenized)

      // Region CCs r1,r2 become siblings in parent ordering tree, and inherit PageAtom ordering
      val splitRegions = textSpanRegion
        .splitAtomsIf((_, _, pairIndex) => Set(1, 4, 6, 7, 9).contains(pairIndex))

      splitRegions.zipWithIndex.foreach{case (r, i) =>
        if ((i % 2)==1) {
          r.addLabel(LB.Sub)
        }
        if (i==1) {
          val exprCCs = r.splitAtomsIf((_, _, pairIndex) => Set(0, 1).contains(pairIndex))
          r.setChildren(LB.TextSpan, exprCCs)
          r.addLabel(LB.Tokenized)
        }
      }

      textSpanRegion.setChildren(LB.TextSpan, splitRegions)

      println("Final Tree")
      println(VisualLine.renderRoleTree(row))

      val rendered = VisualLine.render(row)

      assertResult("Eu_{1 - x}Bi_{x}VO_{4}"){
        rendered.toString()
      }

    }



  }


}
