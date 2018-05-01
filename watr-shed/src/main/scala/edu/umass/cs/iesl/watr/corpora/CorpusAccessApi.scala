package edu.umass.cs.iesl.watr
package corpora

import database._
import filesys._
import workflow._

trait CorpusAccessApi {
  def corpusAccessDB: CorpusAccessDB
  def corpus: Corpus

  def docStore: DocumentZoningApi = corpusAccessDB.docStore
  def workflowApi: WorkflowApi = corpusAccessDB.workflowApi
  def corpusLockApi: CorpusLockingApi = corpusAccessDB.corpusLockApi
  def userbaseApi: UserbaseApi = corpusAccessDB.userbaseApi
  def annotApi: DocumentAnnotationApi = corpusAccessDB.annotApi
  def corpusDirectory: DatabaseCorpusDirectory = new DatabaseCorpusDirectory()(corpusAccessDB)

}

object CorpusAccessApi {

  def apply(db: CorpusAccessDB, corpus0: Corpus): CorpusAccessApi =
    new CorpusAccessApi {
      def corpusAccessDB: CorpusAccessDB = db
      def corpus: Corpus = corpus0
    }

}
