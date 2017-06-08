package edu.umass.cs.iesl.watr
package watrcolors
package server

import scala.concurrent.Future

import corpora._
import labeling._
import labeling.data._
import docstore._

import utils.{Debugging => Dbg}
import scala.concurrent.ExecutionContext
import corpora.{RelationModel => Rel}


class BioArxivServer(
  user: UserData,
  reflowDB: TextReflowDB,
  corpus: Corpus
)(implicit ec: ExecutionContext) extends WatrShellApi {
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

  lazy private val docStore = reflowDB.docStore

  type MakeWidget = LabelerIdentifier => (LabelWidget, LabelerIdentifier)

  def fetchDocumentLabeler(
    reqLabelerId: LabelerIdentifier
  ): Future[UIResponse] = {
    reqLabelerId match {
      case DocumentLabelerIdentifier(stableId, labelerType, _) =>

        val labelColors = TitleAuthorsLabelers.labelerColors()

        val mkWidgetOpt: Option[MakeWidget] = for {
          entry <- corpus.entry(stableId.unwrap)
          rec   <- BioArxivOps.getBioarxivJsonArtifact(entry)
        } yield doOrDie {

          val mkWidget: LabelerIdentifier => (LabelWidget, LabelerIdentifier) =
            labelerIdentifier => {
              val (widget0, labelerId0) = TitleAuthorsLabelers.bioArxivLabeler(labelerIdentifier, rec, docStore)

              val widget1 = LabelWidgetTransforms.addAllZoneIndicators(
                widget0,
                labelColors,
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
            labelColors
          )
          Future { UIResponse(respState, Some(lwIndex.getAllMods())) }
        } getOrElse {
          sys.error("TODO unimplemented")
        }

      case  labelerId @  WorkflowLabelerIdentifier(workflowId) =>
        def workflowApi: WorkflowApi = docStore.workflowApi

        // Hardcoded map keyed off strings
        val labelerBuilder: LabelerBuilder =
          WorkflowServers.servers.get(workflowId)
            .map(_.apply(docStore, workflowId))
            .getOrElse { sys.error("todo") }

        // def workflowDef: Rel.WorkflowDef = workflowApi.getWorkflow(workflowId)

        val labelColors = labelerBuilder.targetLabels()

        val mkWidget: LabelerIdentifier => (LabelWidget, LabelerIdentifier) =
          labelerIdentifier => {
            // val (widget0, labelerId0) = TitleAuthorsLabelers.bioArxivLabeler(labelerIdentifier, rec, docStore)
            val LabelWidgetConfig(workflowId, widget0) = labelerBuilder.createLabeler(user.id)
            val widget1 = LabelWidgetTransforms.addAllZoneIndicators(widget0, labelColors, docStore)
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
          labelColors
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
