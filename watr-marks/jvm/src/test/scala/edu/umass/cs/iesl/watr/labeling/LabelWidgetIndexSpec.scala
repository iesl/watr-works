package edu.umass.cs.iesl.watr
package labeling

// import geometry._
import geometry.syntax._
// import textreflow._
// import textreflow.data._
// import data._
import utils.EnrichNumerics._
// import watrmarks._

import LabelWidgets._

import watrmarks.{StandardLabels => LB}
// import TypeTags._
import corpora._

class LabelWidgetIndexSpec extends LabelWidgetTestUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "LabelWidgetIndexing"

  behavior of "rectangular selection"

  def getDocumentZonesWithLabels(stableId: String@@DocumentID): Seq[(Int@@ZoneID, Int@@LabelID)] = {
    val docId = docStore.getDocument(stableId).get
    for {
      labelId <- docStore.getZoneLabelsForDocument(docId)
      zoneId <- docStore.getZonesForDocument(docId, labelId)
    } yield {
      (zoneId, labelId)
    }
  }


  trait CommonSetup extends CleanDocstore {
    val stableId = add4pg_3x3SampleDoc()

    val layout = col(
      row(pageDivs3(page(1)), pageDivs2(page(2)))
    )

    val lwindex = LabelWidgetIndex.create(docStore, layout)
    // val lwindex = LabelWidgetIndex.init(docStore, NilLabelerIdentifier, (_) => {(layout, NilLabelerIdentifier)}, layout)

  }

  it should "apply a label to selected regions" in new CommonSetup {

    val zoneAndLabel = getDocumentZonesWithLabels(stableId)

    zoneAndLabel.length shouldBe(12)

    val queryBox = getRegionBounds(0, 0, 1, 2).shrink(3.percent)

    val qres = lwindex.queryRegion(queryBox)

    reportQueryHits(queryBox, qres)

    docStore.getTargetRegions(page(1)).length shouldBe (6)

    lwindex.addLabel(queryBox, ByLine, LB.Title)

    docStore.getTargetRegions(page(1)).length shouldBe (6)

    getDocumentZonesWithLabels(stableId).length shouldBe(13)

    lwindex.debugPrint(Some(queryBox))
    println(visualizeDocument(stableId))

  }



  it should "select region(s) by line" in new CommonSetup {

    val queryBox = getRegionBounds(2, 1, 2, 4).shrink(3.percent)

    val qres = lwindex.queryRegion(queryBox)

    // lwindex.debugPrint(Some(queryBox))
    // reportQueryHits(queryBox, qres)
  }

  it should "select region(s) by char" in new CommonSetup {

    val queryBox = getRegionBounds(1, 1, 4, 4).shrink(3.percent)

    val queryHits = lwindex.queryRegion(queryBox)

    // lwindex.debugPrint(Some(queryBox))

    val constrainedHits = lwindex.applyConstraint(ByChar, queryHits)

    // reportQueryHits(queryBox, queryHits)
    // reportQueryHits(queryBox, constrainedHits)
  }


  it should "select and label region by unconstrained rect" in new CommonSetup {

    val queryBox = getRegionBounds(4, 0, 1, 2).shrink(3.percent)
    lwindex.addLabel(queryBox, ByRegion, LB.Title)

    getDocumentZonesWithLabels(stableId).length shouldBe(13)

    // lwindex.debugPrint(Some(queryBox))
    // println(visualizeDocument(stableId))
  }



}
