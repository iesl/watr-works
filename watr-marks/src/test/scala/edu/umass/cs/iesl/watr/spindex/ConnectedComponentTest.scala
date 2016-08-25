package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._


class ConnectedComponentTest extends FlatSpec {

  behavior of "connected components"

  import utils.IdGenerator
  import TypeTags._
  import GeometricFigure._
  import IndexShapeOperations._
  import watrmarks.{StandardLabels => LB}
  val regionIDs = IdGenerator[RegionID]()
  // val pageIDs = IdGenerator[PageID]()

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
              TargetRegion(regionIDs.nextId, PageID(0),
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
  def queryAtoms(pageInfo: PageInfo, x: Int, y: Int, w: Int, h: Int): Seq[PageAtom] = {
    val q = LTBounds(x*xscale, y*yscale, w*xscale, h*xscale)
    pageInfo.charAtomIndex.queryForContained(q)
  }

  it should "label regions" in {
    val ccs =(
      """|a b c
         |d e f
         |""".stripMargin)

    val (atoms, geom) = stringToPageAtoms(ccs)

    val zoneIndex = ZoneIndexer.loadSpatialIndices2(Seq((atoms -> geom)))
    val pageInfo = zoneIndex.getPageInfo(PageID(0))
    val atomComps = pageInfo.getPageAtoms.foreach(atom => zoneIndex.toComponent(atom))

    val l1atoms = queryComponents(pageInfo, 0, 0, 5, 1)
    val l2atoms = queryComponents(pageInfo, 0, 1, 5, 1)

    val l1 = zoneIndex.labelRegion(l1atoms, LB.VisualLine)
    val l2 = zoneIndex.labelRegion(l2atoms, LB.VisualLine)
    l1.foreach { rc =>
      println(s"l1:${rc.chars}")
    }

  }


}
