package edu.umass.cs.iesl.watr
package table

import textreflow.data._
import corpora._
import corpora.filesys._
import TypeTags._
import labeling._

trait DocumentZoningApiEnrichments extends LabelWidgetUtils {

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

    def printPageLines(pageNum: Int)(implicit docStore: DocumentZoningApi): Unit = {
      for {
        (vlineZone, linenum) <- docStore.getPageVisualLines(thisStableId, PageNum(pageNum)).zipWithIndex
      }  {
        val maybeReflow = docStore.getTextReflowForZone(vlineZone.id)
        maybeReflow match {
          case Some(reflow) => println(reflow.toText)
          case None => println(s"no reflow for zone ${vlineZone}")
        }

      }
    }

  }

}
