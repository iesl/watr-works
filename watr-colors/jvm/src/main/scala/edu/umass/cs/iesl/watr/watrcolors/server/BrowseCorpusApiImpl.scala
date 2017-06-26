package edu.umass.cs.iesl.watr
package watrcolors
package server

import scala.concurrent.Future
import corpora._
import workflow._
import scala.concurrent.ExecutionContext
import watrmarks.Label

class BrowseCorpusApiListeners(
  corpusAccessApi: CorpusAccessApi
)(implicit ec: ExecutionContext) extends BrowseCorpusApi {

  val docStore: DocumentZoningApi = corpusAccessApi.docStore
  val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  val userbaseApi: UserbaseApi = corpusAccessApi.userbaseApi

  def listDocuments(n: Int, skip: Int, labelFilter: Seq[Label]): Future[Seq[DocumentEntry]] = {
    println(s"listDocuments $n, $skip")

    Future {
      docStore.getDocuments(n, skip, labelFilter)
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

  def documentCount(labelFilter: Seq[Label]): Future[Int] = {
    Future {
      docStore.getDocumentCount(labelFilter)
    }
  }
}
