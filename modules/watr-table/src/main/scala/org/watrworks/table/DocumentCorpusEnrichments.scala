package org.watrworks
package table

import corpora._

import textboxing.{TextBoxing => TB}, TB._
import geometry._

trait DocumentZoningApiEnrichments extends GeometricFigureCodecs {

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

  }



}
