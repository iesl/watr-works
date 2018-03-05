package edu.umass.cs.iesl.watr
package corpora


import watrmarks._

sealed trait Annotation

object Annotation {

  // Zone = Ordered list of rectangles + Label Name
  //      + (optional) Textgrid
  //      + (optional) Workflow target-path (e.g., 'BioArxiv.PMID.GoldStandard')
  //      + (optional) tags (e.g., SynthesisPara, ExperimentalSection)
  //      + (optional) user-text, notes

  // Anchor

}
trait DocumentAnnotationApi {
  val Rel = RelationModel

  def createAnnotation(docId: Int@@DocumentID): Int@@AnnotationID
  def deleteAnnotation(annotId: Int@@AnnotationID): Unit
  def getAnnotationRecord(annotId: Int@@AnnotationID): Rel.AnnotationRec

  def assignOwnership(annotId: Int@@AnnotationID, userId: Int@@UserID): Unit

  def setCorpusPath(annotId: Int@@AnnotationID, path: String@@CorpusPath): Unit

  def updateBody(annotId: Int@@AnnotationID, body: String): Unit

  def listAnnotations(path: String@@CorpusPathQuery): Seq[Int@@AnnotationID]
  def listAnnotations(userId: Int@@UserID, path: Option[String@@CorpusPathQuery]): Seq[Int@@AnnotationID]

  def createLabelSchema(labelSchema: LabelSchemas): Int@@LabelSchemaID
  def getLabelSchema(schemaId: Int@@LabelSchemaID): LabelSchemas
  def deleteLabelSchema(schemaId: Int@@LabelSchemaID): Unit

}
