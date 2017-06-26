package edu.umass.cs.iesl.watr
package watrcolors

import labeling._
import scala.concurrent.Future
import watrmarks.Label

// API for calls made from web client to server
trait WatrShellApi {
  def uiRequest(r: UIRequest): Future[UIResponse]
  def fetchDocumentLabeler(labelerRequest: LabelerIdentifier): Future[UIResponse]
  def listWorkflows(): Future[Seq[WorkflowTask]]
}

case class WorkflowTask(
  workflowId: String@@WorkflowID,
  description: String
)

case class DocumentEntry(
  id: String@@DocumentID,
  urlStr: String,
  zoneCounts:Seq[(Label, Int)] // sorting order??
)

trait BrowseCorpusApi {
  def listDocuments(n: Int, skip: Int, labelFilters: Seq[Label]): Future[Seq[DocumentEntry]]
  def documentCount(labelFilters: Seq[Label]): Future[Int]
}

sealed trait ApiCall

final case class RemoteCall(
  path: List[String], args: List[(String, String)]
) extends ApiCall
