package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

trait ConnectedComponentTestUtil extends FlatSpec with Matchers {
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

  def lines(str: String): Seq[String] = {
    str.split("\n")
      .map(_.trim)
  }

  val ffi = 0xFB03.toChar
  val charSubs = Map(
    ffi -> "ffi",
    'ﬂ' -> "fl",
    'ﬆ' -> "st",
    'æ' -> "ae",
    'Æ' -> "AE"
  )

  def lineWithSubs(line: String): Unit = {

    line.zipWithIndex
      .foldLeft(List[Char]())({case (acc, e) => 

        acc
      })
  }

  def stringToPageAtoms(str: String): (Seq[PageAtom], PageGeometry) = {

    val atoms = lines(str).zipWithIndex
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

  def queryComponents(pageInfo: PageIndex, x: Int, y: Int, w: Int, h: Int): Seq[Component] = {
    val q = LTBounds(x*xscale, y*yscale, w*xscale, h*xscale)
    pageInfo.componentIndex.queryForContained(q)
  }


  def labelRow(zoneIndex: ZoneIndexer, row: Int, l: Label): Option[RegionComponent] = {
    val pageInfo = zoneIndex.getPageIndex(page0)
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

  import java.net.URI
  def createZoneIndexer(str: String): ZoneIndexer = {
    val (atoms, geom) = stringToPageAtoms(str)
    val dummyUri = URI.create("/")
    ZoneIndexer.loadSpatialIndices(dummyUri, Seq((atoms -> geom)))
  }

  def createZoneIndexerAndLines(str: String): (ZoneIndexer, Seq[String]) = {
    val (atoms, geom) = stringToPageAtoms(str)
    val dummyUri = URI.create("/")
    (ZoneIndexer.loadSpatialIndices(dummyUri, Seq((atoms -> geom))),
      lines(str))
  }

  def tokenizeStringToZoneIndex(str: String): (ZoneIndexer, Seq[RegionComponent]) = {
    val (zoneIndex, lines) = createZoneIndexerAndLines(str)
    val visualLines = for {
      i <- 0 until lines.length
      visualLine <- labelRow(zoneIndex, i, LB.VisualLine)
    } yield {
      // visualLine.addLabel(LB.Tokenized)
      val textSpanRegion = visualLine.cloneAs(LB.TextSpan)
      textSpanRegion.addLabel(LB.Tokenized)
      visualLine.setChildren(LB.TextSpan, Seq(textSpanRegion))

      textSpanRegion.groupAtomsIf({(atom1, atom2, pairIndex) =>
        val dist = atom2.bounds.right - atom1.bounds.right
        dist <= xscale
      }, {(region, regionIndex) =>
        region.addLabel(LB.TextSpan)
        region.addLabel(LB.Token)
        textSpanRegion.addChild(LB.TextSpan, region)
      })

      visualLine
    }
    (zoneIndex, visualLines)
  }

}
