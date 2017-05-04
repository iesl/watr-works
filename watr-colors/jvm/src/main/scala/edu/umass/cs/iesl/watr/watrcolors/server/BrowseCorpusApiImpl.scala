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


  def listLabelers(n: Int, skip: Int): Future[Seq[LabelerEntry]] = {
    Future {
      List()
    }
  }
  def listDocuments(n: Int, skip: Int): Future[Seq[DocumentEntry]] = {
    println(s"listDocuments $n, $skip")
    Future {
      docStore.getDocuments(n, skip)
        .map(stableId => DocumentEntry(stableId, stableId.unwrap))
    }

  }

  def documentCount(): Future[Int] = {
    Future {
      docStore.getDocumentCount()
    }
  }
}
