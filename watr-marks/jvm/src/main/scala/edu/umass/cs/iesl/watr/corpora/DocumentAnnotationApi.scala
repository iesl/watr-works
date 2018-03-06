package edu.umass.cs.iesl.watr
package corpora


import watrmarks._
import geometry._

import io.circe.generic.JsonCodec


@JsonCodec
sealed trait AnnotationBody

object AnnotationBody extends GeometricFigureCodecs {

  // Zone = Ordered list of rectangles + Label Name
  //      + (optional) Textgrid
  //      + (optional) Workflow target-path (e.g., 'BioArxiv.PMID.GoldStandard')
  //      + (optional) tags (e.g., SynthesisPara, ExperimentalSection)
  //      + (optional) user-text, notes

  @JsonCodec
  case class Zone(
    regions: Seq[PageRegion],
    label: Label,
    textGridDef: Option[String] = None
  ) extends AnnotationBody

}

trait DocumentAnnotationApi {
  val Rel = RelationModel

  def createAnnotation(docId: Int@@DocumentID): Int@@AnnotationID
  def deleteAnnotation(annotId: Int@@AnnotationID): Unit
  def getAnnotationRecord(annotId: Int@@AnnotationID): Rel.AnnotationRec

  def assignOwnership(annotId: Int@@AnnotationID, userId: Int@@UserID): Unit

  def setCorpusPath(annotId: Int@@AnnotationID, path: String@@CorpusPath): Unit

  def updateBody(annotId: Int@@AnnotationID, body: AnnotationBody): Unit

  def listPathAnnotations(path: String@@CorpusPathQuery): Seq[Int@@AnnotationID]
  def listUserAnnotations(userId: Int@@UserID, path: Option[String@@CorpusPathQuery]): Seq[Int@@AnnotationID]
  def listDocumentAnnotations(docId: Int@@DocumentID): Seq[Int@@AnnotationID]

  def createLabelSchema(labelSchema: LabelSchemas): Int@@LabelSchemaID
  def getLabelSchema(schemaId: Int@@LabelSchemaID): LabelSchemas
  def deleteLabelSchema(schemaId: Int@@LabelSchemaID): Unit

}
