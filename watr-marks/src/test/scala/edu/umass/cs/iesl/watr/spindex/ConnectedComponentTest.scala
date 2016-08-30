package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

trait ConnectedComponentTestUtil extends Matchers {
  import utils.IdGenerator
  import TypeTags._
  import GeometricFigure._
  import IndexShapeOperations._
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

  def sortAndConnect(c: Component, l:Label, orderingLabel: Label, sortf: (Component) => Double): Unit = {
    val patoms = c.queryInside(l)
    c.connectChildren(orderingLabel, patoms.sortBy(sortf))
  }

  def createZoneIndexer(str: String): ZoneIndexer = {
    val (atoms, geom) = stringToPageAtoms(str)
    ZoneIndexer.loadSpatialIndices2(Seq((atoms -> geom)))
  }
}


class ConnectedComponentTest extends FlatSpec with ConnectedComponentTestUtil {
  import scalaz.std.string._

  behavior of "connected components"

  // import TypeTags._
  import watrmarks.{StandardLabels => LB}



  it should "label, sort and connect regions" in {
    val ccs =(
      """|a b c
         |d e f
         |""".stripMargin)

    val zoneIndex = createZoneIndexer(ccs)

    val l1 = labelRow(zoneIndex, 0, LB.VisualLine)
    val l2 = labelRow(zoneIndex, 1, LB.VisualLine)


    // Create an ordered list of page atoms within a labeled region
    l1.foreach { regionComp =>
      sortAndConnect(regionComp, LB.PageAtom, LB.PageAtom, _.bounds.left)
    }

    l2.foreach { regionComp =>
      sortAndConnect(regionComp, LB.PageAtom, LB.PageAtom, _.bounds.left)
    }

    println("create ordering")

    l1.foreach { regionComp =>
      val children = regionComp.getChildTree(LB.PageAtom)
      children.foreach { tree =>
        println(tree.mkString(", "))
      }
    }

    l2.foreach { regionComp =>
      val children = regionComp.getChildTree(LB.PageAtom)
      children.foreach { tree =>
        println(tree.mkString(", "))
      }
    }
  }

  it should "demonstrate sup/subscript labeling" in {
    val zoneIndex = createZoneIndexer(
      // 01234567890
      """Eu1-xBixVO4"""
    )

    //  when ccs are 'connected' they query is cached such that future lookups return the ordered children
    for {
      row       <- labelRow(zoneIndex, 0, LB.VisualLine)
    } {
      sortAndConnect(row, LB.PageAtom, LB.PageAtom, _.bounds.left)

      // Create a CC w/same target region, same atom sorting, linked as child to row
      val textSpanRegion = row.cloneAs(LB.TextSpan)

      // Region CCs r1,r2 become siblings in parent ordering tree, and inherit PageAtom ordering
      val splitRegions = textSpanRegion
        .splitAtomsIf((_, _, pairIndex) => Set(1, 4, 6, 7, 9).contains(pairIndex))

      splitRegions.zipWithIndex.foreach{case (r, i) =>
        if ((i % 2)==1) {
          r.addLabel(LB.Sub)
        }
      }

      row.setChildren(LB.TextSpan, splitRegions)

      println("Super/subscript=")
      val treeView = row.toTree(LB.TextSpan, LB.PageAtom).map(_.toString()).drawTree

      import scalaz.Tree

      val rendered = row.toTree(LB.TextSpan, LB.PageAtom)
        .scanr[String]({case (c: Component, ts: Stream[Tree[String]]) =>
          c.roleLabel match {
            case LB.VisualLine =>
              ts.map(_.rootLabel).mkString

            case LB.TextSpan =>
              val sdf = ts.map(_.rootLabel).mkString
              if (c.getLabels.contains(LB.Sub)) {
                s"""_{${sdf}}"""
              } else {
                s"""${sdf}"""
              }
            case LB.PageAtom => c.chars
            case _ => c.toString
          }

        })

      println("Super/subscript=")
      println(treeView)
      println(rendered.rootLabel)

    }



  }


}
