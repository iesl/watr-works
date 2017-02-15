package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

import watrmarks._
import geometry._

import watrmarks.{StandardLabels => LB}
import TypeTags._

import corpora._

trait ConnectedComponentTestUtil extends FlatSpec with Matchers with CorpusTestingUtil {

  def labelRow(mpageIndex: MultiPageIndex, row: Int, l: Label, pageId: Int@@PageNum=PageNum(0)): Option[RegionComponent] = {
    val pageIndex = mpageIndex.getPageIndex(pageId)
    val q = LTBounds(0, row*yscale, Int.MaxValue, yscale)
    val charAtoms = pageIndex.componentIndex.queryForContained(q)
    val reg = mpageIndex.labelRegion(charAtoms, l)
    // assert each region can select its contained page atoms
    reg.foreach { rc =>
      val patoms = rc.queryInside(LB.PageAtom)
      assertResult(charAtoms.toSet) {
        patoms.toSet
      }
    }
    reg
  }

  def createMultiPageIndex(stableId: String@@DocumentID, strs: String*): MultiPageIndex = {
    MultiPageIndex.initDocument(
      stableId,
      stringsToMultiPageAtoms(stableId, strs:_*),
      docStore
    )
  }

  // import com.sksamuel.scrimage._
  // def createMultiPageIndexWithImages(stableId: String@@DocumentID, strs: String*): (MultiPageIndex, Seq[Image]) = {
  //   val pages =
  //     stringsToMultiPageAtomsWithImages(stableId, strs:_*)
  //       .map({case (atom, geom, img) => ((atom, geom), img)})

  //   (MultiPageIndex.initDocument(stableId, pages.map(_._1)), pages.map(_._2))
  // }
  // def textReflowToImage(pageReflow: TextReflow): Image = {
  //   val vlines =  pageReflow.sliceLabels(LB.VisualLine)

  //   // total page target region
  //   val TargetRegion(id, stableId, pageId, LTBounds(l, t, w, h) ) =
  //     pageReflow.targetRegions.reduce(_ union _)

  //   val blank = Image.filled((w*10).toInt, (h*10).toInt, X11Colorlist.White)
  //   val canvas = new Canvas(blank)
  //   for ((vline, n) <- vlines.zipWithIndex) yield {
  //     val ltext = vline.toText()
  //     canvas.draw(Drawable(ltext, 0, 10*n*yscale.toInt))
  //   }

  //   canvas.image.scale(0.10, ScaleMethod.FastScale)
  // }

}
