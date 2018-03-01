package edu.umass.cs.iesl.watr
package workflow

import corpora.{RelationModel => R}
import TypeTags._

/**
  * A Workflow describes the progress of a data curation effort
  *
  * Given a set of pre-existing zones, add new labels within those zones.
  *
  * For example, a curation effort might be:
  *    Given a set of regions labeled "Bibliography", ask users to label individual references
  *
  *    Given the pages of a document, label the titles and abstracts.
  *    For consistency, the "zone" of a full document has the label FullPdf
  *
  * Workflow operates like so:
  *    ∘ Assume that users are present in the user table.
  *    ∘ A new workflow is defined, including a unique name and description, the target zone label to be locked for further labeling, and a label schema.
  *    ∘ One or more zone locks are acquired for a user. The pool of candidate zones for locking include only those that
  *      have no lock status, i.e., have not yet been seen by any user
  *
  *    ∘ When the user is finished adding labels to a particular zone, the lock status is changed to Completed (or Skipped, or another configurable status)
  *    ∘ Users continue to acquire new zone locks until there are none left
  *
  *
  *    ∘ A WorkflowReport includes
  *       a count of as-yet unseen zones,
  *       counts of zones having a particular status
  *       users with assigned zones
  *
  */

import _root_.io.circe, circe._
import circe.generic.semiauto._

import watrmarks._

object WorkflowStatus {
  val OnHold    = StatusCode("OnHold")
  val Active    = StatusCode("Active")
  val Complete  = StatusCode("Complete")
}


case class WorkflowReport(
  statusCounts: Map[String@@StatusCode, Int],
  userAssignmentCounts: Seq[(Int@@UserID, String@@EmailAddr, String@@StatusCode, Int)]
)

object WorkflowReport extends TypeTagCodecs {

  implicit val encMap: Encoder[Map[String@@StatusCode, Int]] =
    Encoder[Map[String, Int]].contramap { m =>
      m.map{case (k, v) => (k.unwrap, v)}
    }

  implicit val encMap2: Encoder[Map[Int@@UserID, Int]] =
    Encoder[Map[Int, Int]].contramap { m =>
      m.map{case (k, v) => (k.unwrap, v)}
    }

  implicit val encMap3: Encoder[Map[Int@@UserID, String@@EmailAddr]] =
    Encoder[Map[Int, String]].contramap { m =>
      m.map{case (k, v) => (k.unwrap, v.unwrap)}
    }

  implicit lazy val enc: Encoder[WorkflowReport] = deriveEncoder

  import textboxing.{TextBoxing => TB}, TB._
  import textboxing.TextBoxingLayouts._

  def prettyPrint(workflows: Seq[(WorkflowReport, R.WorkflowDef)]): TB.Box = {
    vjoins(
      workflows.map { case (report, workflowDef) =>
        // val report = workflowApi.getWorkflowReport(workflowId)
        // val workflowDef = workflowApi.getWorkflow(workflowId)
        val path = workflowDef.targetPath

        val boxLabelSchema = LabelSchemas.labelSchemaToBox(
          workflowDef.labelSchemas
        )

        val userAssignments = hangingIndent("User Assignment Counts",
          report.userAssignmentCounts.toSeq.map{ case (userId, email, status, count) =>
            s"$email / ${status} = ${count}".box
          }
        )

        val statusCounts = hangingIndent("User Assignment Status",
          report.statusCounts.toSeq.map { case (statusCode, num) =>
            s"${statusCode}: ${num}".box
          }
        )


        hangingIndent(s"Workflow ${workflowDef.workflow} in path '${path}'", Seq(
          userAssignments,
          statusCounts,
          boxLabelSchema
        ))
      }
    )
  }
}

trait WorkflowApi {

  def corpusLockingApi(): CorpusLockingApi

  def defineWorkflow(
    slug: String,
    schemaName: String@@LabelSchemaName,
    targetPath: String@@CorpusPath
  ): String@@WorkflowID

  def deleteWorkflow(workflowId:String@@WorkflowID): Unit

  def getWorkflows(): Seq[String@@WorkflowID]
  def getWorkflow(workflowId:String@@WorkflowID): R.WorkflowDef
  def getWorkflowReport(workflowId:String@@WorkflowID): WorkflowReport

  def lockNextDocument(userId: Int@@UserID, workflowId: String@@WorkflowID): Option[Int@@LockID] = {
    val p = getWorkflow(workflowId).targetPath
    corpusLockingApi.acquireLock(userId, p)
  }

}

object CorpusLockStatus {
  val Available    = StatusCode("Available")
  val Locked       = StatusCode("Locked")
  val Completed    = StatusCode("Completed")

  val all = List(Available, Locked, Completed)
}

trait CorpusLockingApi {

  def createLock(docId: Int@@DocumentID, lockPath: String@@CorpusPath): Int@@LockID
  def deleteLock(lockId: Int@@LockID): Unit
  def getLocks(): Seq[Int@@LockID]

  def getLockRecord(lockId: Int@@LockID): Option[R.CorpusLock]

  def getDocumentLocks(docId: Int@@DocumentID): Seq[Int@@LockID]
  def getUserLocks(userId: Int@@UserID): Seq[Int@@LockID]

  def acquireLock(userId: Int@@UserID, lockPath: String@@CorpusPath): Option[Int@@LockID]
  def releaseLock(lockId: Int@@LockID): Unit

}
