package edu.umass.cs.iesl.watr
package formats

import edu.umass.cs.iesl.watr.watrmarks.WeightedLabeling

import rtrees._
import textboxing.{TextBoxing => TB}, TB._
import segment.{SegmentationLabels => LB}
import segment._
import _root_.io.circe, circe._, circe.syntax._
import circe.generic.auto._
import circe.generic._
import geometry._


object TextGraphIO {

  def jsonOutputGridPerPage(docSeg: DocumentSegmentation): Json = {
    val stableId = docSeg.stableId

    val allPageTextGrids = docSeg.pageSegmenters
      .map { pageSegmenter =>
        val textGraph = pageSegmenter.getTextGraph()
        // val rows = textGraph.rows.length
        val gridJs = textGraph.asJson


        val LTBounds.IntReps(l, t, w, h) = pageSegmenter.pageGeometry
        val geom = Json.arr(Json.fromInt(l), Json.fromInt(t), Json.fromInt(w), Json.fromInt(h))

        Json.obj(
          "pageGeometry" := geom,
          "textgrid" := gridJs
        )
      }

    // val fontDescriptions = fontDescription(docSeg)

    val outputDoc = Json.obj(
      "description" := s"Extracted Pages for ${stableId}",
      "documentId" := stableId.unwrap,
      "pages" := allPageTextGrids,
      // "fonts" := fontDescriptions
    )

    outputDoc
  }


}
