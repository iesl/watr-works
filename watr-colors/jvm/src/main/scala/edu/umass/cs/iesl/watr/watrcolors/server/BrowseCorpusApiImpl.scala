package edu.umass.cs.iesl.watr
package watrcolors
package server

import scala.concurrent.Future
import corpora._
import docstore._
import scala.concurrent.ExecutionContext

class BrowseCorpusApiListeners(
  reflowDB: TextReflowDB,
  corpus: Corpus
)(implicit ec: ExecutionContext) extends BrowseCorpusApi {

  private def docStore = reflowDB.docStore

  def listDocuments(n: Int, skip: Int): Future[Seq[DocumentEntry]] = {
    println(s"listDocuments $n, $skip")


    Future {
      docStore.getDocuments(n, skip)
        .map{ stableId =>

          val zoneToLableTuples = for {
            docId <- docStore.getDocument(stableId).toList
          } yield for {
            labelId <- docStore.getZoneLabelsForDocument(docId)
          } yield {
            val label = docStore.getLabel(labelId)
            val nZones =  docStore.getZonesForDocument(docId, labelId).length
            (label, nZones)
          }

          DocumentEntry(stableId, stableId.unwrap, zoneToLableTuples.flatten)
        }
    }

  }

  def documentCount(): Future[Int] = {
    Future {
      docStore.getDocumentCount()
    }
  }
}
