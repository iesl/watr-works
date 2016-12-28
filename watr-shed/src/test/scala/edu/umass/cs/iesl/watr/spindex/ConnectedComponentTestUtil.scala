package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

import textreflow._
import watrmarks._
import geometry._
import matryoshka._
import matryoshka.implicits._
import TypeTags._
import GeometricFigure._
import EnrichGeometricFigures._
import TextReflowF._

trait ConnectedComponentTestUtil extends FlatSpec with Matchers with PlainTextReflow {

  def textReflowToComponentReflow(textReflow: TextReflow, mpageIndex: MultiPageIndex): TextReflow = {
    val regions = textReflow.collect({
      case t@ Embed(Atom(c)) => c.targetRegion.bbox
    })
    val totalPageGeometry = regions.reduce(_ union _)
    val geom = PageGeometry(PageID(0), totalPageGeometry)

    mpageIndex.addPage(geom)

    val _ = textReflow
      .collect { case a@ Embed(Atom(c)) => c }
      .map(a => mpageIndex.addPageAtom(a))

    textReflow
  }

  def labelRow(mpageIndex: MultiPageIndex, row: Int, l: Label): Option[RegionComponent] = {
    val pageInfo = mpageIndex.getPageIndex(page0)
    val q = LTBounds(0, row*yscale, Int.MaxValue, yscale)
    val charAtoms = pageInfo.componentIndex.queryForContained(q)
    val reg = mpageIndex.labelRegion(charAtoms, l)
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



  def createMultiPageIndex(str: String): MultiPageIndex = {
    val (atoms, geom) = stringToPageAtoms(str)
    val dummyUri = URI.create("/")
    MultiPageIndex.loadSpatialIndices(dummyUri, Seq((atoms -> geom)))
  }

  def createMultiPageIndexAndLines(str: String): (MultiPageIndex, Seq[String]) = {
    val (atoms, geom) = stringToPageAtoms(str)
    val dummyUri = URI.create("/")
    (MultiPageIndex.loadSpatialIndices(dummyUri, Seq((atoms -> geom))),
      lines(str))
  }

  def tokenizeStringToZoneIndex(str: String): (MultiPageIndex, Seq[RegionComponent]) = {
    val (mpageIndex, lines) = createMultiPageIndexAndLines(str)
    val visualLines = for {
      i <- 0 until lines.length
      visualLine <- labelRow(mpageIndex, i, LB.VisualLine)
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
    (mpageIndex, visualLines)
  }

  def queryComponents(pageInfo: PageIndex, x: Int, y: Int, w: Int, h: Int): Seq[Component] = {
    val q = LTBounds(x*xscale, y*yscale, w*xscale, h*xscale)
    pageInfo.componentIndex.queryForContained(q)
  }


}
