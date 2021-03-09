package org.watrworks
package formats

import segment._
import _root_.io.circe, circe._, circe.syntax._
import circe.generic.auto._

object TextGridOutputFormats {

  def fontDescription(docSeg: DocumentSegmentation): Seq[Json] = {

    val fontDefs = docSeg.docScope.fontDefs

    val scaledMetrics = fontDefs.fontProperties.map { fp =>
      val metrics = fp.scaledMetrics().map { scaledMetrics =>
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

    scaledMetrics.to(Seq)
  }
}
