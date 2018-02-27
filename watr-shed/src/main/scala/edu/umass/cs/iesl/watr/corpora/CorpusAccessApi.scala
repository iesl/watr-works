package edu.umass.cs.iesl.watr
package corpora

import database._
import filesys._
import workflow._
import corpora.{RelationModel => Rel}
import spindex.MultiPageIndex
import geometry.syntax._
import scalaz.syntax.equal._
import java.nio.{file => nio}
import scalaz.{
  \/, -\/, \/-
}

object RegionImageResponse {
  type RegionImageResponse  = String \/ (Array[Byte] \/ nio.Path)

  def anError(r:String): RegionImageResponse = -\/(r)
  def aByteArray(r:Array[Byte]): RegionImageResponse = \/-(-\/(r))
  def aPath(r:nio.Path): RegionImageResponse = \/-(\/-(r))

  def fold[T](v: RegionImageResponse)(
    onError: String => T,
    onBytes: Array[Byte] => T,
    onPath: nio.Path => T
  ) = v.fold(onError, _.fold(onBytes, onPath))

}


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
