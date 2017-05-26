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

    val DocumentLabelerIdentifier(stableId, labelerType, pagination, labelColors) = reqLabelerId

    val mkWidgetOpt: Option[MakeWidget] = labelerType match {
      case "1pg" => doOrDie {
        val mkWidget: LabelerIdentifier => (LabelWidget, LabelerIdentifier) =
          labelerIdentifier => {
            val (widget0, labelerId0) = TitleAuthorsLabelers.singlePageLabeler(docStore)
            val widget1 = LabelWidgetTransforms.addAllZoneIndicators(widget0, labelerId0, docStore)
            (widget1, labelerId0)
          }
        Some(mkWidget)
      }

      case _ =>
        val hasEntry = corpus.hasEntry(stableId.unwrap)
        println(s"createDocumentLabeler($stableId, ${labelerType}): hasEntry=$hasEntry")

        for {
          entry <- corpus.entry(stableId.unwrap)
          rec   <- BioArxivOps.getBioarxivJsonArtifact(entry)
        } yield doOrDie {

          val mkWidget: LabelerIdentifier => (LabelWidget, LabelerIdentifier) =
            labelerIdentifier => {
              val (widget0, labelerId0) = TitleAuthorsLabelers.bioArxivLabeler(labelerIdentifier, rec, docStore)
              val widget1 = LabelWidgetTransforms.addAllZoneIndicators(widget0, labelerId0, docStore)
              (widget1, labelerId0)
            }
          mkWidget
        }

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
        lwIndex.labelerIdentifier
      )
      Future { UIResponse(respState, Some(lwIndex.getAllMods())) }
    } getOrElse {
      sys.error("TODO unimplemented")
    }
  }


  def uiRequest(r: UIRequest): Future[UIResponse] = {
    try {
      val UIRequest(
        uiState@ UIState(constraint, maybeLabel, selections, currLabeler),
        gesture
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
}
