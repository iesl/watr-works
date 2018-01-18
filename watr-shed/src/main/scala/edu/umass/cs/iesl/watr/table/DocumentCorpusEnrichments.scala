package edu.umass.cs.iesl.watr
package table

import corpora._
import TypeTags._

import textboxing.{TextBoxing => TB}, TB._

trait DocumentZoningApiEnrichments  {

  implicit class RicherDocumentZoningApi(val theDocumentZoningApi: DocumentZoningApi) {

    def documents(n: Int=0, skip: Int=0): Seq[String@@DocumentID] = {
      val allEntries = theDocumentZoningApi.getDocuments()
      val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
      val entries = if (n > 0) skipped.take(n) else skipped
      entries
    }
  }


  implicit class RicherStableID(val thisStableId: String@@DocumentID) {
    def getDocument()(implicit docStore: DocumentZoningApi): Int@@DocumentID = {
      docStore.getDocument(thisStableId).getOrElse { sys.error(s"no document ${thisStableId}") }
    }

    def getPages()(implicit docStore: DocumentZoningApi): Seq[Int@@PageID] = {
      docStore.getDocument(thisStableId).toSeq
        .flatMap(docId => docStore.getPages(docId))
    }


    def reportDocument()(implicit docStore: DocumentZoningApi): TB.Box = {
      val docBoxes = for {
        docId <- docStore.getDocument(thisStableId).toSeq
      } yield {
        val pagesBox = for {
          pageId <- docStore.getPages(docId)
        } yield {
          val pageGeometry = docStore.getPageGeometry(pageId)

          val allTargetRegions = docStore.getTargetRegions(pageId)

          val regionCount =  s"TargetRegions for page ${pageId}: ${allTargetRegions.length} ".box

          (
            indent(2, "PageGeometry")
              % indent(4, pageGeometry.toString.box)
              % indent(2, regionCount)
              % indent(2, "Page Zones")
          )
        }

        val zoneBoxes = for {
          labelId <- docStore.getZoneLabelsForDocument(docId)
          zoneId <- docStore.getZonesForDocument(docId, labelId)
          // textReflow <- docStore.getTextReflowForZone(zoneId)
        } yield {
          // (textReflow.toText.box % docStore.getZone(zoneId).toString().box)
          docStore.getZone(zoneId).toString().box
        }
        (s"Document ${docId} (${thisStableId}) report"
          % indent(4, vcat(left,pagesBox))
          % indent(2, "Zones")
          % indent(4, vcat(left,zoneBoxes))
        )
      }
      vcat(left,docBoxes)
    }





  }

}
