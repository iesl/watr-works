package edu.umass.cs.iesl.watr
package watrcolors
package server


import scala.concurrent.Future

import labeling._
import labeling.data._

import corpora._
import corpora.filesys._
import workflow._

import annotate._
import utils.{Debugging => Dbg}
import scala.concurrent.ExecutionContext

class BioArxivServer(
  user: UserData,
  corpusAccessApi: CorpusAccessApi
)(implicit ec: ExecutionContext) extends WatrShellApi {

  val docStore: DocumentZoningApi = corpusAccessApi.docStore
  val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  val userbaseApi: UserbaseApi = corpusAccessApi.userbaseApi
  val corpus: Corpus = corpusAccessApi.corpus

  var activeLabelWidgetIndex: Option[LabelWidgetIndex] = None

  import bioarxiv._

  def doOrDie[T](body: => T): T = {
    try {
      body
    } catch {
      case t: Throwable => Dbg.die(t)
        sys.error("")
    }
  }

  type MakeWidget = LabelerIdentifier => (LabelWidget, LabelerIdentifier)

  def fetchDocumentLabeler(
    reqLabelerId: LabelerIdentifier
  ): Future[UIResponse] = {
    reqLabelerId match {
      case DocumentLabelerIdentifier(stableId, labelerType, _) =>

        val labelColors = HeaderCoarseLabelers.labelerColors()

        val mkWidgetOpt: Option[MakeWidget] = for {
          entry <- corpus.entry(stableId.unwrap)
        } yield doOrDie {
          val rec  = BioArxivOps.getBioarxivJsonArtifact(entry)

          val mkWidget: LabelerIdentifier => (LabelWidget, LabelerIdentifier) =
            labelerIdentifier => {
              val (widget0, labelerId0) = HeaderCoarseLabelers.bioArxivLabeler(labelerIdentifier, rec, docStore)

              val widget1 = LabelWidgetTransforms.addAllZoneIndicators(
                widget0,
                labelColors.toMap,
                docStore
              )

              (widget1, labelerId0)
            }
          mkWidget
        }

        mkWidgetOpt.map { mkWidget =>
          val lwIndex = LabelWidgetIndex.init(
            docStore, reqLabelerId, mkWidget, None, None
          )
          activeLabelWidgetIndex = Some(lwIndex)
          val respState = UIState(
            ByLine,
            labelColors.headOption.map(_._1),
            Seq(),
            lwIndex.labelerIdentifier,
            labelColors.toMap
          )
          Future { UIResponse(respState, Some(lwIndex.getAllMods())) }
        } getOrElse {
          sys.error("TODO unimplemented")
        }

      case  labelerId @  WorkflowLabelerIdentifier(workflowId) =>

        // Hardcoded map keyed off strings
        val labelerBuilder: ServerLabelerBuilder =
          WorkflowServers.servers.get(workflowId)
            .map(_.apply(corpusAccessApi, workflowId))
            .getOrElse { sys.error("todo") }

        val labelColors = labelerBuilder.targetLabels()

        val mkWidget: LabelerIdentifier => (LabelWidget, LabelerIdentifier) =
          labelerIdentifier => {
            val LabelWidgetConfig(_, widget0) = labelerBuilder.createLabelerForUser(user.id)
            val widget1 = LabelWidgetTransforms.addAllZoneIndicators(widget0, labelColors.toMap, docStore)
            (widget1, labelerIdentifier)
          }
        val lwIndex = LabelWidgetIndex.init(
          docStore, reqLabelerId, mkWidget, None, None
        )
        activeLabelWidgetIndex = Some(lwIndex)
        val respState = UIState(
          ByLine,
          labelColors.headOption.map(_._1),
          Seq(),
          lwIndex.labelerIdentifier,
          labelColors.toMap
        )
        Future { UIResponse(respState, Some(lwIndex.getAllMods())) }

      case labelerId =>
        sys.error(s"got some unknown labelerId ${labelerId}")
    }

  }


  def uiRequest(r: UIRequest): Future[UIResponse] = {
    try {
      val UIRequest(
        uiState, //@ UIState(constraint, maybeLabel, selections, currLabeler, labelColors),
        _
      ) = r

      activeLabelWidgetIndex.map { lwIndex =>
        // println(s"got UIRequest ${r}")

        val (uiResponse, modifiedWidgetIndex) = lwIndex.userInteraction(r)

        activeLabelWidgetIndex = Some(modifiedWidgetIndex)

        // println(s"  ==> UIResponse ${uiResponse}")
        Future{ uiResponse }
      } getOrElse {
        Future{ UIResponse(uiState, None) }
      }
    } catch {
      case t: Throwable =>
        Dbg.die(t)
        throw t
    }
  }



  def listWorkflows(): Future[Seq[WorkflowTask]] = {
    Future {
      List()
    }
  }

}
