package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import labeling._
import scala.concurrent.Future
// import TypeTags._

// API for calls made from web client to server
trait WatrShellApi {
  def uiRequest(r: UIRequest): Future[UIResponse]
  def onDrawPath(artifactId: String, path: Seq[Point]): Unit
  def createDocumentLabeler(stableId: String@@DocumentID, labelerType: String): Future[(Seq[WidgetPositioning], LabelOptions)]
}


case class LabelerEntry(
  name: String@@LabelingTaskID,
  assignee: Option[String@@Username],
  status: String
)

case class DocumentEntry(
  id: String@@DocumentID,
  urlStr: String
)

trait BrowseCorpusApi {
  def listLabelers(n: Int, skip: Int): Future[Seq[LabelerEntry]]
  def listDocuments(n: Int, skip: Int): Future[Seq[DocumentEntry]]
  def documentCount(): Future[Int]
}






// API for calls made from server to web client
trait WatrColorsApi {
  def clear(): Unit
  def print(level: String, msg: String): Unit
  def echoLabeler(lwidget: Seq[WidgetPositioning], labelOptions: LabelOptions): Unit
}

final case class RemoteCall(
  path: List[String], args: List[(String, String)]
)
