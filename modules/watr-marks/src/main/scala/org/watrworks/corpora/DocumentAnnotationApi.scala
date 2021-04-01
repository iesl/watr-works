package org.watrworks
package corpora

import watrmarks._
import geometry._

import io.circe.generic.JsonCodec

import utils.DoOrDieHandlers._

@JsonCodec
sealed trait AnnotatedLocation {
  def documentId: String@@DocumentID
}


object AnnotatedLocation extends GeometricFigureCodecs {

  @JsonCodec
  case class Location(
    location: StableIdentifier
  ) extends AnnotatedLocation {
    def documentId: String@@DocumentID = location.documentId

  }

  @JsonCodec
  case class Zone(
    regions: Seq[PageRegion]
  ) extends AnnotatedLocation {

    def documentId: String@@DocumentID = {
      val r0 = regions.headOption.orDie("No regions in Zone")
      r0.documentId
    }

  }

}

@JsonCodec
sealed trait AnnotationBody

object AnnotationBody extends GeometricFigureCodecs {

  // Zone = Ordered list of rectangles + Label Name
  //      + (optional) Textgrid
  //      + (optional) Workflow target-path (e.g., 'BioArxiv.PMID.GoldStandard')
  //      + (optional) tags (e.g., SynthesisPara, ExperimentalSection)
  //      + (optional) user-text, notes

  @JsonCodec
  case class TextGrid(
    textGridDef: String
  ) extends AnnotationBody

}

trait DocumentAnnotationApi {
  val Rel = RelationModel

  def createAnnotation(label: Label, labeledRegion: AnnotatedLocation): Int@@AnnotationID

  def deleteAnnotation(annotId: Int@@AnnotationID): Unit
  def getAnnotationRecord(annotId: Int@@AnnotationID): Rel.AnnotationRec

  def assignOwnership(annotId: Int@@AnnotationID, userId: Int@@UserID): Unit

  def setCorpusPath(annotId: Int@@AnnotationID, path: String@@CorpusPath): Unit

  def updateBody(annotId: Int@@AnnotationID, body: AnnotationBody): Unit
  def updateLocation(annotId: Int@@AnnotationID, loc: AnnotatedLocation): Unit

  def listPathAnnotations(path: String@@CorpusPathQuery): Seq[Int@@AnnotationID]
  def listUserAnnotations(userId: Int@@UserID, path: Option[String@@CorpusPathQuery]): Seq[Int@@AnnotationID]
  def listDocumentAnnotations(docId: Int@@DocumentID): Seq[Int@@AnnotationID]

  def createLabelSchema(labelSchema: LabelSchemas): Int@@LabelSchemaID
  def getLabelSchema(schemaId: Int@@LabelSchemaID): LabelSchemas
  def deleteLabelSchema(schemaId: Int@@LabelSchemaID): Unit

}
