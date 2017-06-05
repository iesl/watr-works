package edu.umass.cs.iesl.watr
package labeling

import corpora._



trait HeaderCoarseLabelers extends LabelWidgetUtils {
  def docStore: DocumentCorpus


  // register this collection of handlers

  // define a unit of work
  def getDocumentZonesWithLabels(stableId: String@@DocumentID): Seq[(Int@@ZoneID, Int@@LabelID)] = {
    val docId = docStore.getDocument(stableId).get
    for {
      labelId <- docStore.getZoneLabelsForDocument(docId)
      zoneId <- docStore.getZonesForDocument(docId, labelId)
    } yield {
      (zoneId, labelId)
    }
  }

  def assignWork(userId: Int@@UserID): Unit = {
    // docStore.getZonesWithLabel(LB.DocumentPages)
  }


  // given a unit of work, create a label widget

  // setup the server side handlers for the label widget

  // declare the appropriate client side handlers ??

}
