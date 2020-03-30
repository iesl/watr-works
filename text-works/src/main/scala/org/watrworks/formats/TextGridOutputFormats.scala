package org.watrworks
package formats

import org.watrworks.watrmarks.WeightedLabeling
import scala.{ collection => sc }
import sc.Seq

import rtrees._
import textboxing.{TextBoxing => TB}, TB._
import segment.{SegmentationLabels => LB}
import segment._
import _root_.io.circe, circe._, circe.syntax._
import circe.generic.auto._
import circe.generic._
import geometry._

object TextGridOutputFormats  {
  /**
    *   {
    *     description: "desc",
    *     documentId: "doc-25-id",
    *     pages: [{
    *       pdfPageBounds: [0, 0, 61200, 79200],
    *       lines: [{
    *         text: "I Ã ffi",
    *         glyphs: [
    *           [1, 2, 3, 4],
    *           [[59, 2, 3, 4], {}],
    *           [[3, 2, 3, 4], {
    *             "gs": [
    *               [[1, 2, 3, 4], { "g": "A" }],
    *               [[1, 2, 3, 4], { "g": "~" }]
    *             ]
    *           }],
    *         ]
    *       }, {
    *         text: "Fe_{3}",
    *         glyphs: [
    *           [11, 2, 3, 4],
    *           [22, 2, 3, 4],
    *           [[53, 2, 3, 4], { "os": [-2, -1, 1] }],
    *           [[54, 2, 3, 4], { "o": 1 }]
    *         ]
    *       }]
    *     }],
    *     labels: [
    *       { name: "HasRefs", id: "L#2", range: [{ unit: "page", at: [7, 2] }] },
    *       { name: "IsGoldLabled", id: "L#3", range: [{ unit: "document" }] },
    *     ]
    *   }
    *
    */

  @JsonCodec
  case class PageDef(
    pdfPageBounds: (Int, Int, Int, Int),
    lines: Json
  )

  @JsonCodec
  case class DocumentTextGrids(
    description: String,
    documentId: String,
    pages: Seq[PageDef],
    // labels: ...
  )

  import java.nio.{file => nio}
  import ammonite.{ops => fs}
  import utils.DoOrDieHandlers._

  def readDocumentJsonDef(f: nio.Path): DocumentTextGrids = {
    val jsonStr = fs.read(fs.Path(f))
    jsonStr.decodeOrDie[DocumentTextGrids]()
  }

  def jsonOutputGridPerPage(docSeg: DocumentSegmentation): JsonObject = {
    val stableId = docSeg.stableId

    val allPageTextGrids = docSeg.pageSegmenters
      .map { pageSegmenter =>
        val textGrid = pageSegmenter.getTextGrid(None)
        val rows = textGrid.rows.length
        val pageJs = textGrid.toJson()


        val LTBounds.IntReps(l, t, w, h) = pageSegmenter.pageGeometry
        val geom = Json.arr(Json.fromInt(l), Json.fromInt(t), Json.fromInt(w), Json.fromInt(h))

        pageJs.deepMerge(Json.obj(
          "pdfPageBounds" := geom,
        ));
      }

    val fontDescriptions = fontDescription(docSeg)

    Json.obj(
      "description" := s"Extracted Pages for ${stableId}",
      "documentId" := stableId.unwrap,
      "pages" := allPageTextGrids
    ).asObject.get
  }


  def fontDescription(docSeg: DocumentSegmentation): Seq[Json] = {

    val fontDefs = docSeg.docScope.fontDefs

    val scaledMetrics = fontDefs.fontProperties.map { fp =>

      val metrics = fp.scaledMetrics.map{ scaledMetrics =>
        // val perPageGlyphCounts = fp.natLangGlyphOccurrenceCounts.column(scaledMetrics.scalingFactor)

        // val perPageList = perPageGlyphCounts.entrySet().asScala.toList
        val perPageList = fp.natLangGlyphOccurrenceCounts.getColumn(scaledMetrics.scalingFactor)

        val sortedByPage = perPageList.map { case(pageNum, glyphCount)  =>
          // val pageNum = entry.getKey()
          // val glyphCount = entry.getValue()
          (pageNum.unwrap, glyphCount)
        }.sorted

        val res = Json.obj(
          "scale" := scaledMetrics.scalingFactor.unwrap,
          "glyphsPerPage" := sortedByPage,
          "heights" := Json.obj(
            // "lowerCaseSmall" := scaledMetrics.heightLowerCaseSmall,
            // "lowerCaseLarge" := scaledMetrics.heightLowerCaseLarge,
            // "upperCase" := scaledMetrics.heightUpperCase
          )
        )
        res
      }

      val name = if (fp.name != null && fp.name.trim().length()>0) {
        fp.name
      } else {
        "font"
      }
      val res = Json.obj(
        "name" := name,
        // "englishBigramEvidence" := fp.bigramEvidence.count(_ > 0),
        "metrics" := metrics
      )

      res
    }

    scaledMetrics
  }
}
