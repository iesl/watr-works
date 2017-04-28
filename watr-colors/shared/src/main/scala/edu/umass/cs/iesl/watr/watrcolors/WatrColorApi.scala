package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import labeling._
import scala.concurrent.Future

// API for calls made from web client to server
trait WatrShellApi {
  def uiRequest(r: UIRequest): Future[UIResponse]
  def onDrawPath(artifactId: String, path: Seq[Point]): Unit
  def fetchDocumentLabeler(labelerRequest: LabelerIdentifier): Future[UIResponse]
}

// TODO this can just be LabelerIdentifier, remove it when ready
case class LabelerEntry(
  name: String@@LabelingTaskID,
  assignee: Option[String@@Username],
  status: String
)
object LabelerEntry {
  import upickle.default._, Aliases._
  import TypeTagPicklers._

  implicit val LabelerEntry_RW: RW[LabelerEntry] = macroRW[LabelerEntry]

}

case class DocumentEntry(
  id: String@@DocumentID,
  urlStr: String
)
object DocumentEntry {
  import upickle.default._, Aliases._
  import TypeTagPicklers._

  implicit val DocumentEntry_RW: RW[DocumentEntry] = macroRW[DocumentEntry]


}

trait BrowseCorpusApi {
  def listLabelers(n: Int, skip: Int): Future[Seq[LabelerEntry]]
  def listDocuments(n: Int, skip: Int): Future[Seq[DocumentEntry]]
  def documentCount(): Future[Int]
}


sealed trait ApiCall
object ApiCall {
  import upickle.default._, Aliases._

  implicit val RemoteCall_RW: RW[RemoteCall] = macroRW[RemoteCall]
}
final case class RemoteCall(
  path: List[String], args: List[(String, String)]
) extends ApiCall
