package edu.umass.cs.iesl.watr
package watrcolors

import labeling._
import scala.concurrent.Future
import watrmarks.Label

// API for calls made from web client to server
trait WatrShellApi {
  def uiRequest(r: UIRequest): Future[UIResponse]
  def fetchDocumentLabeler(labelerRequest: LabelerIdentifier): Future[UIResponse]
}

// TODO this can just be LabelerIdentifier, remove it when ready
case class LabelerEntry(
  name: String@@LabelingTaskID,
  assignee: Option[String@@Username],
  status: String
)

// Search for docs w/labels like
//  Find doc with all labels present:  '(authors & title & abstract)'
//  Find doc with any label present:  '(authors | title | abstract)'
//  Find doc with at least on missing label:  '!(authors & title & abstract)'
//  Find doc with at least on missing label:  '!(authors | title | abstract)'

// Show labeling info for a given document
//   select count(zone), document from zone where label = 1 group by document;

case class DocumentEntry(
  id: String@@DocumentID,
  urlStr: String,
  zoneCounts:Seq[(Label, Int)] // sorting order??
)

trait BrowseCorpusApi {
  def listLabelers(n: Int, skip: Int): Future[Seq[LabelerEntry]]
  def listDocuments(n: Int, skip: Int): Future[Seq[DocumentEntry]]
  def documentCount(): Future[Int]
}


sealed trait ApiCall

final case class RemoteCall(
  path: List[String], args: List[(String, String)]
) extends ApiCall
