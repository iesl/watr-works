package edu.umass.cs.iesl.watr
package table

import corpora._
import utils.DoOrDieHandlers._
import _root_.io.circe, circe._
import textgrid._

import TextGridFunctions._
import com.github.tototoshi.csv._


object CsvExporter {

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

}
