package edu.umass.cs.iesl.watr
package table

import corpora._
import utils.DoOrDieHandlers._
import _root_.io.circe, circe._
import circe.syntax._
import textgrid._
import geometry._

import LabeledSequenceTreeTransforms._
import com.github.tototoshi.csv._
import ammonite.{ops => fs} // , fs._


object AnnotationExporters {

  def writeLabelSpanCsv(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")
    val csvFile = new java.io.File("out.csv")
    val csvWriter = CSVWriter.open(csvFile, append=true)

    val annots = annotApi.listDocumentAnnotations(docId)
    annots.foreach { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)
      annotRec.body.foreach { body =>
        body match {
          case AnnotationBody.TextGrid(textGridDef) =>
            val textGrid = TextGrid.fromJsonStr(textGridDef)
            // Iterate over all labeled spans
            val labelTree = textGridToLabelTree(textGrid)
            val labelSpanTree = labelTreeToSpanTree(labelTree)

            val grid1Cells = textGrid.indexedCells()
            labelSpanTree.flatten.foreach { case (maybeLabel, start, len) =>

              val text = grid1Cells.slice(start, start+len).map(_._1.char).mkString

              grid1Cells.slice(start, start+len).map{ case (gridCell, row, col) =>
                val bbox = gridCell.pageRegion.bbox

                val LTBounds.Doubles(left, top, w, h) = bbox

              }
              val encText = Json.fromString(text).noSpaces
              maybeLabel.foreach { l =>
                csvWriter.writeRow(List(stableId.unwrap, l.fqn, encText))
              }
            }
        }
      }

    }

    csvWriter.close()
  }

  import RelationModel._

  def writeDocumentAnnotations(stableId: String@@DocumentID, rootPath: String)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val annots = annotApi.listDocumentAnnotations(docId)
    val allAnnotJsons = annots.map { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)
      val body = annotRec.body.map { _ match {
        case AnnotationBody.TextGrid(textGridDef) =>
          val textGrid = TextGrid.fromJsonStr(textGridDef)
          textGrid.toJson()
      }} getOrElse( Json.Null )

      Json.obj(
        "id"        := annotRec.id,
        "document"  := annotRec.document,
        "owner"     := annotRec.owner,
        "annotPath" := annotRec.annotPath,
        "created"   := annotRec.created,
        "label"     := annotRec.label,
        "location"  := annotRec.location,
        "body"      := body
      )

    }

    val allannotJson = Json.obj(
      "stableId" := stableId,
      "annotations" := allAnnotJsons
    )

    val outputStr = allannotJson.spaces2

    val outputRoot = fs.pwd / rootPath / stableId.unwrap
    val annotPath = outputRoot / s"${stableId}-annots.json"

    if (!fs.exists(outputRoot)) {
      fs.mkdir(outputRoot)
    }

    fs.write(annotPath, outputStr)
  }
}
