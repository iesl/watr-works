package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

import textreflow._
import watrmarks._
import geometry._
// import matryoshka._
// import matryoshka.implicits._

// import EnrichGeometricFigures._
// import TextReflowF._

trait ConnectedComponentTestUtil extends FlatSpec with Matchers with PlainTextReflow {

  // def textReflowToComponentReflow(textReflow: TextReflow, mpageIndex: MultiPageIndex): TextReflow = {
  //   val regions = textReflow.collect({
  //     case t@ Embed(Atom(c)) => c.targetRegion.bbox
  //   })
  //   val totalPageGeometry = regions.reduce(_ union _)
  //   val geom = PageGeometry(PageID(0), totalPageGeometry)

  //   mpageIndex.addPage(geom)

  //   val _ = textReflow
  //     .collect { case a@ Embed(Atom(c)) => c }
  //     .map(a => mpageIndex.addPageAtom(a))

  //   textReflow
  // }

  def labelRow(mpageIndex: MultiPageIndex, row: Int, l: Label): Option[RegionComponent] = {
    val pageIndex = mpageIndex.getPageIndex(page0)
    val q = LTBounds(0, row*yscale, Int.MaxValue, yscale)
    val charAtoms = pageIndex.componentIndex.queryForContained(q)
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

  def createMultiPageIndex(docId: String@@DocumentID, strs: String*): MultiPageIndex = {
    val pages = for {
      (pstr, pagenum) <- strs.zipWithIndex
    } yield {
      stringToPageAtoms(pstr, pagenum, docId)
    }

    MultiPageIndex.loadSpatialIndices(docId, pages)
  }

  def queryComponents(pageIndex: PageIndex, x: Int, y: Int, w: Int, h: Int): Seq[Component] = {
    val q = LTBounds(x*xscale, y*yscale, w*xscale, h*xscale)
    pageIndex.componentIndex.queryForContained(q)
  }


}
