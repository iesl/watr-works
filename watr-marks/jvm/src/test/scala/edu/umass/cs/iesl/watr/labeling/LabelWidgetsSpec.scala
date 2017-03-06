package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._

// import geometry._
// import textreflow.data._
// import labeling.data._

import TypeTags._
import corpora._
// import LabelWidgets._

class LabelWidgetsSpec extends FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {

  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  initEmpty()

  val docs = List(
    List("01\n23", "45\n67"),
    List("01\n23", "45\n67")
  )

  for { (doc, i) <- docs.zipWithIndex } {
    addDocument(DocumentID(s"doc#${i}"), doc)
  }

  behavior of "label widgets"


  it should "include labeled targets" in {
    // docStore.addTargetRegion(pageId: <refinement>[Int, PageID], bbox: LTBounds)
    // labeledTarget()
  }

  it should "create overlays" in  {
    val bb00 = getRegionBounds(0, 0, 1, 1)
    // val w1 = LW.targetOverlay(reg0, List(
    //   LW.labeledTarget(sel0)
    // ))
  }

  it should "create columns" in {
  }
  it should "create rows" in {
  }
  it should "include inserted text (as textbox)" in {
  }
  it should "include reflows" in {
  }
  it should "include padding" in {
  }

  // prettyPrintLabelWidget(test)
  // layoutWidgetPositions(test)
  // prettyPrintPosition(abs0)

}
