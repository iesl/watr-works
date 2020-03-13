package edu.umass.cs.iesl.watr
package table

import scalaz.{@@ => _, _} // , Scalaz._

import TypeTags._
import corpora._
import utils.DoOrDieHandlers._
import _root_.io.circe, circe._
import circe.syntax._
import textgrid._
import textgraph._
import annots._
import geometry._

import LabeledSequenceTreeTransforms._
import com.github.tototoshi.csv._
import ammonite.{ops => fs}  , fs._
import RelationModel._
import watrmarks._


object AnnotationExporters {

  def writeAllLabeledTexts(rootPath: String, label: Label)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val docStore = corpusAccessApi.docStore
    val docCount = docStore.getDocumentCount
    val batchSize = 200
    var offset = 0
    while (offset < docCount) {
      println(s"writing docs ${offset} - ${offset+batchSize}")

      val labelTexts = for {
        stableId <- docStore.getDocuments(batchSize, offset)
      } yield {
        (stableId, getAnnotationText(stableId, label))
      }

      val block = labelTexts.map{ case(stableId, docTexts) =>
        docTexts.mkString(s"=== ${stableId} ===\n", "\n\n\n", "\n")
      }.mkString("\n", "\n\n\n", "\n")

      val annotPath = fs.pwd / rootPath / s"export-text-for-${label.fqn}.json"

      fs.write.append(annotPath, block)

      offset = offset + batchSize
    }

  }

  def writeAllAnnotations(rootPath: String)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val docStore = corpusAccessApi.docStore
    val docCount = docStore.getDocumentCount
    val batchSize = 200
    var offset = 0
    while (offset < docCount) {
      println(s"writing docs ${offset} - ${offset+batchSize}")

      val batchExport = for {
        stableId <- docStore.getDocuments(batchSize, offset)
      } yield {
        exportAnnotations(stableId)
      }

      val batchJson = batchExport.asJson
      val annotPath = fs.pwd / rootPath / s"batch-${offset}-annots.json"

      fs.write(annotPath, batchJson.noSpaces)

      offset = offset + batchSize
    }

  }
  def writeDocumentAnnotations(stableId: String@@DocumentID, rootPath: String)(implicit corpusAccessApi: CorpusAccessApi): Unit = {

    val annotJson = exportAnnotations(stableId)
    val outputStr = annotJson.spaces2

    val outputRoot = fs.pwd / rootPath / stableId.unwrap
    val annotPath = outputRoot / s"${stableId}-annots.json"

    if (!fs.exists(outputRoot)) {
      fs.mkdir(outputRoot)
    }

    fs.write(annotPath, outputStr)
  }

  def getAnnotationText(stableId: String@@DocumentID, label: Label)(implicit corpusAccessApi: CorpusAccessApi): Seq[String] = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val annots = annotApi.listDocumentAnnotations(docId)
    val allAnnotStrings = annots
      .toList.map { annotApi.getAnnotationRecord(_) }
      .filter(_.label == label)
      .flatMap{ annotRec =>
        annotRec.body.flatMap { _ match {
          case AnnotationBody.TextGrid(textGridDef) =>
            // val gridAsJson = textGridDef.decodeOrDie[Json]("Not a valid Json object")
            // println(s"gridAsJson \n ${gridAsJson}")
            val textGrid = TextGrid.fromJsonStr(textGridDef)
            Some(textGrid.toText())
          case _ => None
        }}
      }

    allAnnotStrings
  }

  def exportAnnotations(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Json = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val annots = annotApi.listDocumentAnnotations(docId)
    val allAnnotJsons = annots.map { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)
      val body = annotRec.body.map { _ match {
        case AnnotationBody.TextGrid(textGridDef) =>
          val gridAsJson = textGridDef.decodeOrDie[Json]("Not a valid Json object")
          // val textGrid = TextGrid.fromJsonStr(textGridDef)
          // textGrid.toJson()
          gridAsJson
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

    Json.obj(
      "stableId" := stableId,
      "annotations" := allAnnotJsons
    )

  }


}
