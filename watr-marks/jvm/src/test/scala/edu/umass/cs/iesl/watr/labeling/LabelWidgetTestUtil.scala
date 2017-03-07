package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._

import geometry._

import corpora._
import utils.EnrichNumerics._

  // import LabelWidgets._
  // import LabelWidgetLayoutHelpers._
  // import TypeTags._

abstract class LabelWidgetTestUtil extends FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {
  def generatePageRegions(divs: Int): Seq[PageRegion] = {
    val allRegions = for {
      stableId <- docStore.getDocuments
      docId    <- docStore.getDocument(stableId).toSeq
      pageId   <- docStore.getPages(docId)
    }  yield {
      val pageGeometry = docStore.getPageGeometry(pageId)

      for {
        yslice <- intervalSlices(DoubleInterval(pageGeometry.top, pageGeometry.height), divs).tails.filterNot(_.isEmpty).map(_.reduce(_ union _))
        // _  =  println(s"yslice ${yslice}")
        xslice <- intervalSlices(DoubleInterval(pageGeometry.left, pageGeometry.width), divs).tails.filterNot(_.isEmpty).map(_.reduce(_ union _))
      } yield {
        // println(s"   x ${xslice}")
        val regionBbox = LTBounds(
          xslice.min, yslice.min,
          xslice.len, yslice.len
        )
        PageRegion(pageId, regionBbox)
      }
    }
    allRegions.flatten
  }

  def printPageRegions(pageRegions: Seq[PageRegion]): Unit = {
    val res = pageRegions
    .sortBy({ p => (p.pageId.unwrap, p.bbox.top, p.bbox.height, p.bbox.left, p.bbox.width) })
      .map(_.toString)
      .mkString("\n  ", "\n  ", "\n")
    println(res)
  }

}
