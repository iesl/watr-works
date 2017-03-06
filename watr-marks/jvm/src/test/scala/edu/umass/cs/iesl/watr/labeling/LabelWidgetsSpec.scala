package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._

import geometry._
import geometry.syntax._
// import textreflow.data._
// import labeling.data._

import TypeTags._
import corpora._
import LabelWidgets._

class LabelWidgetsSpec extends FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {
  import utils.EnrichNumerics._

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

  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  initEmpty()

  val docs = List(
    List("01\n23")
      // List("01\n23", "45\n67")
    // List("01\n23", "45\n67")
  )

  for { (doc, i) <- docs.zipWithIndex } {
    addDocument(DocumentID(s"doc#${i}"), doc)
  }

  behavior of "label widgets"

  // Our invariant condition states:
  //  After running any layout algorithm on a given set of layout widgets,
  //   the intersection of a bbox with any widget, then transformed by that widgets
  //   positioning transform (inverse), gets us a rectangle in the coordinate space
  //   of the target region contained in that widget



  it should "include labeled targets" in {
    val pageRegion = PageRegion(PageID(1), LTBounds(5d, 4d, 3d, 2d) )
    val pageRegion2 = PageRegion(PageID(1), LTBounds(5.1d, 4.1d, 3.1d, 2.1d) )

    val subRegion = PageRegion(PageID(1), LTBounds(5.5d, 4.5d, 0.5d, 1.5d) )
    val subRegion2 = PageRegion(PageID(1), LTBounds(5.6d, 3.5d, 0.1d, 2.5d) )

    val widget0 = col(
      targetOverlay(pageRegion, List(labeledTarget(subRegion), labeledTarget(subRegion2))),
      targetOverlay(pageRegion2, List(labeledTarget(subRegion), labeledTarget(subRegion2)))
    )
    val widgetLayout = layoutWidgetPositions(widget0)

    widgetLayout.positioning
      .foreach{ pos =>
        println(s"${pos}")
        // val borigin = pos.widgetBounds.moveToOrigin()
        // val bwidget = borigin.translate(pos.translation)
        // println(s"   borigin: ${borigin}")
        // println(s"   bwidget: ${bwidget}")
      }
  }

  it should "correctly position overlays" in  {}


  // it should "create columns" in {
  //   val divs = 3
  //   val pageRegions = generatePageRegions(divs)


  //   val widget0 = col(
  //     pageRegions.map(targetOverlay(_, List())):_*
  //   )
  //   val widgetLayout = layoutWidgetPositions(widget0)

  //   // given some arbitrary layout..
  //   widgetLayout.positioning
  //     .sortBy({ p => (p.widgetBounds.width) })
  //     .foreach{ pos =>
  //       val borigin = pos.widgetBounds.moveToOrigin()
  //       val bwidget = borigin.translate(pos.translation)
  //       // println(s"${pos}")
  //       // println(s"   borigin: ${borigin}")
  //       // println(s"   bwidget: ${bwidget}")
  //     }

  // }



  it should "create rows" in {
  }
  it should "include inserted text (as textbox)" in {
  }
  it should "include reflows" in {
  }
  it should "include padding" in {
  }

}
