package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

import textreflow._
import watrmarks._
import geometry._

import watrmarks.{StandardLabels => LB}
import TypeTags._

trait ConnectedComponentTestUtil extends FlatSpec with Matchers with ImageTextReflow {

  def labelRow(mpageIndex: MultiPageIndex, row: Int, l: Label, pageId: Int@@PageID=PageID(0)): Option[RegionComponent] = {
    val pageIndex = mpageIndex.getPageIndex(pageId)
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
    MultiPageIndex.loadSpatialIndices(
      docId,
      stringsToMultiPageAtoms(docId, strs:_*)
    )
  }

  import com.sksamuel.scrimage._
  def createMultiPageIndexWithImages(docId: String@@DocumentID, strs: String*): (MultiPageIndex, Seq[Image]) = {
    val pages =
      stringsToMultiPageAtomsWithImages(docId, strs:_*)
        .map({case (atom, geom, img) => ((atom, geom), img)})

    (MultiPageIndex.loadSpatialIndices(docId, pages.map(_._1)), pages.map(_._2))
  }

}
