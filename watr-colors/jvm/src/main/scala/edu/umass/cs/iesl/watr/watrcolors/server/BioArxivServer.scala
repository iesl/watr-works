package edu.umass.cs.iesl.watr
package watrcolors
package server

import scala.concurrent.Future

import corpora._
import geometry._
import labeling._
import labeling.data._
import docstore._

import utils.{Debugging => Dbg}
import scala.concurrent.ExecutionContext

class BioArxivServer(
  reflowDB: TextReflowDB,
  corpus: Corpus
)(implicit ec: ExecutionContext) extends WatrShellApi {
  var activeLabelWidgetIndex: Option[LabelWidgetIndex] = None

  import bioarxiv._

  def onDrawPath(artifactId: String, path: Seq[Point]): Unit = {
    println(s"onDrawPath: ")
  }

  def fetchDocumentLabeler(
    labelerRequestx: LabelerIdentifier
  ): Future[UIResponse] = {

    val DocumentLabelerIdentifier(stableId, labelerType, pagination, labelColors) = labelerRequestx

    val hasEntry = corpus.hasEntry(stableId.unwrap)
    println(s"createDocumentLabeler($stableId, ${labelerType}): hasEntry=$hasEntry")
    val docStore = reflowDB.docStore

    val maybeLabeler = for {
      entry <- corpus.entry(stableId.unwrap)
      rec   <- BioArxivOps.getBioarxivJsonArtifact(entry)
    } yield {

      try {
        val (labelWidget, updatedLabelId) = TitleAuthorsLabelers.bioArxivLabeler(labelerRequestx, rec, docStore)

        val mkWidget: LabelerIdentifier => (LabelWidget, LabelerIdentifier) =
          labelerIdentifier => TitleAuthorsLabelers.bioArxivLabeler(labelerIdentifier, rec, docStore)

        val lwIndex = LabelWidgetIndex.init(
          docStore,
          updatedLabelId,
          mkWidget,
          Some(
            LabelWidgetTransforms.addAllZoneIndicators(
              labelWidget,
              updatedLabelId,
              docStore
            )
          ),
          None
        )

        activeLabelWidgetIndex = Some(lwIndex)

        val respState = UIState(
          ByLine,
          labelColors.headOption.map(_._1),
          Seq(),
          updatedLabelId
        )

        Future { UIResponse(respState, lwIndex.getAllMods()) }

      } catch {
        case t: Throwable =>
          Dbg.die(t)
          throw t
      }
    }

    maybeLabeler.getOrElse {
      sys.error("createDocumentLabeler: error")
    }

  }

  def uiRequest(r: UIRequest): Future[UIResponse] = {
    try {
      val UIRequest(
        uiState@ UIState(constraint, maybeLabel, selections, currLabeler),
        gesture
      ) = r

      activeLabelWidgetIndex.map { lwIndex =>
        println(s"got UIRequest ${r}")

        val (uiResponse, modifiedWidgetIndex) = lwIndex.userInteraction(uiState, gesture)

        activeLabelWidgetIndex = Some(modifiedWidgetIndex)

        println(s"  ==> UIResponse ${uiResponse}")
        Future{ uiResponse }
      } getOrElse {
        Future{ UIResponse(uiState, List()) }
      }
    } catch {
      case t: Throwable =>
        Dbg.die(t)
        throw t
    }
  }
}
