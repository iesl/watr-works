package org.watrworks
package formats

import org.watrworks.watrmarks.WeightedLabeling
import scala.{collection => sc}
import sc.Seq

import rtrees._
import textboxing.{TextBoxing => TB}, TB._
import segment.{SegmentationLabels => LB}
import segment._
import _root_.io.circe, circe._, circe.syntax._
import circe.generic.auto._
import circe.generic._
import geometry._
import transcripts._
import java.nio.{file => nio}
import ammonite.{ops => fs}
import utils.DoOrDieHandlers._
import org.watrworks.textgrid.TextGrid

object TextGridOutputFormats {

  def jsonOutputGridPerPage(docSeg: DocumentSegmentation): Json = {
    val stableId = docSeg.stableId

    val pages = docSeg.pageSegmenters.map { pageSegmenter =>
      val textGrid = pageSegmenter.getTextGrid(None)

      val lines = textGrid.rows().map(row => {
        val text = row.toText()

        val glyphs = row.cells().map(_ match {

          case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>

            val props = if (tailItems.isEmpty) None else {
              val tailGlyphs = tailItems.map(pageItem => {
                Transcript.Glyph(
                  pageItem.bbox,
                  props = Some(Transcript.GlyphProps(
                  ))
                )
              }).toList

              Some(Transcript.GlyphProps(
                gs = Some(tailGlyphs)
              ))
            }

            Transcript.Glyph(
              cell.pageRegion.bbox,
              props
            )

          case cell@ TextGrid.InsertCell(char, insertAt) =>
            Transcript.Glyph(
              cell.pageRegion.bbox,
              props = Some(Transcript.GlyphProps(
              )))
        }).toList

        Transcript.Line(text, glyphs)
      }).toList

      Transcript.Page(
        pageSegmenter.pageGeometry,
        lines
      )
    }

    Transcript(
      s"Extracted Pages for ${stableId}",
      stableId,
      pages.toList,
      labels = List()
    ).asJson
  }

  def fontDescription(docSeg: DocumentSegmentation): Seq[Json] = {

    val fontDefs = docSeg.docScope.fontDefs

    val scaledMetrics = fontDefs.fontProperties.map { fp =>
      val metrics = fp.scaledMetrics.map { scaledMetrics =>
        // val perPageGlyphCounts = fp.natLangGlyphOccurrenceCounts.column(scaledMetrics.scalingFactor)

        // val perPageList = perPageGlyphCounts.entrySet().asScala.toList
        val perPageList =
          fp.natLangGlyphOccurrenceCounts.getColumn(scaledMetrics.scalingFactor)

        val sortedByPage = perPageList.map {
          case (pageNum, glyphCount) =>
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

      val name = if (fp.name != null && fp.name.trim().length() > 0) {
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
