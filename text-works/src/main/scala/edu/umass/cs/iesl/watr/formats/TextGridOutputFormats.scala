package edu.umass.cs.iesl.watr
package formats

import edu.umass.cs.iesl.watr.watrmarks.WeightedLabeling
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
    * ** Text
    *      Text with left-margin BIO labels
    *      Text with underscored BIO labels
    *
    *  ** Json Output Structure
    *      Page Dimensions for each mentioned page  (required)
    *      Document-wide font summary (optional)
    *         font names per scaling factor
    *         occurrence counts per page or across document
    *      Per-line:
    *         Text as String (optional)
    *         Glyphs (glyph-type, char, page, bbox), where glyph-type==(insert|glyph)  (required)
    *      Preamble major/minor/git version info
    *      BIO Label key
    *
    *
    *  Font format
    *      List[(font-id, begin-index, length)]
    *
    *  BIO Examples: paragraph, section, title, abstract, image, caption, table, inset-math,...
    *
    *  Label BIO format
    *      Named Tree Structure:
    *         Authors = [(Author, beg, len), [children]]
    *         e.g.,
    *         Authors = [(Author, beg, len), [
    *                       [(FN, beg, len)]
    *                       [(MN, beg, len)]
    *                       [(LN, beg, len)]
    *                   ]]
    *         LineLayout = [
    *                        [(Sup, beg, len)],
    *                        [(Sup/Sub, b, l), [
    *                           [(Sup, b, l)],
    *                           [(Sub, b, l)]
    *                        ]],  ...]
    *
    *
    *         "fontDescriptions": {
    *             "namedFonts": [
    *                 {
    *                     "name": "ACOONO+AdvOptima-b",
    *                     "naturalBigrams" : 15
    *                     "scaled": [
    *                         [104, {"heights" : {"lcSmall" : 0.0, "lcLarge" : 0.0, "uc" : 0.0 } }],
    *                         [208, {"heights" : {"lcSmall" : 0.0, "lcLarge" : 0.0, "uc" : 0.0 } }]
    *                     ]
    *                 },
    *             ],
    *             "scaledFonts": [
    *                 [0, "ACOONO+AdvOptima-b", 104],
    *                 [1, "ACOONO+AdvOptima-b", 208],
    *                 [2, "ACOONO+AdvOptima-b", 104]
    *             ]
    *        }
    *
    *
    *
    *        "fonts" : [
    *         {
    *             "name" : "ACOONO+AdvOptima-b",
    *             "bigrams" : 15,
    *             "metrics" : [
    *                  {
    *                     "scale" : 104,
    *                     "heights" : {
    *                         "lowerCaseSmall" : 0.0,
    *                         "lowerCaseLarge" : 0.0,
    *                         "upperCase" : 0.0
    *                     }
    *                 }, {
    *                     "scale" : 208,
    *                     "heights" : {"lowerCaseSmall" : 7.266,
    *                         "lowerCaseLarge" : 10.979999999999999,
    *                         "upperCase" : 10.055714285714286
    *                     }
    *                 }]
    *
    *
    *
    *
    */

  @JsonCodec
  case class PageDef(
    pageGeometry: (Int, Int, Int, Int),
    textgrid: Json
  )

  @JsonCodec
  case class DocumentTextGrids(
    description: String,
    pages: Seq[PageDef]
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
        val gridJs = textGrid.toJson()


        val LTBounds.IntReps(l, t, w, h) = pageSegmenter.pageGeometry
        val geom = Json.arr(Json.fromInt(l), Json.fromInt(t), Json.fromInt(w), Json.fromInt(h))

        Json.obj(
          "pageGeometry" := geom,
          "textgrid" := gridJs
        )
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
        // println(s"scaledMetrics: ")
        // println(s" perPageList: ${perPageList}")
        // println(s" perPageGlyphCounts: ${perPageGlyphCounts}")
        // println(s" sortedByPage: ${sortedByPage}")
        // println(s" inner final: ${res}")
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

      // println(s" final fp: ${fp.name}")
      // println(s" final bi: ${fp.bigramEvidence.toList}")
      // println(s" final: ${res}")

      res
    }

    scaledMetrics

  }

  // def documentToStructuredPlaintext(mshapeIndex: MultiPageIndex): String = {
  //   val allText = for {
  //     pageNum      <- mshapeIndex.getPages
  //     shapeIndex    <- List(mshapeIndex.getPageIndex(pageNum))
  //   } yield {
  //     val textCol = for {
  //       (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(shapeIndex).toList
  //       lineCC <- lineCCs
  //     } yield {

  //       val lineText = shapeIndex.getComponentText(lineCC, LB.VisualLine).map(_.toText().take(40).mkString)
  //       lineText.getOrElse("<no text>").box
  //     }

  //     val pinCol = for {
  //       (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(shapeIndex).toList
  //       lineCC <- lineCCs
  //     } yield {
  //       val lineWeights = shapeIndex.getAttribute[WeightedLabeling](lineCC.id, watrmarks.Label("LineGrouping")).get
  //       val linePins = lineWeights.countedPins()
  //       if (linePins.nonEmpty) {
  //         val maxPin = linePins.maxBy(_._2)._1

  //         val pinrep = {
  //           maxPin.isBegin.option[String]("^")
  //             .orElse { maxPin.isInside.option[String]("|") }
  //             .orElse { maxPin.isUnit.option[String]("#") }
  //             .orElse { maxPin.isLast.option[String]("$") }
  //             .getOrElse { "?" }
  //         }

  //         val pincol = maxPin.label match {
  //           case LB.Para =>
  //             val pincls = {
  //               maxPin.isBegin.option[String]("¶")
  //                 .getOrElse { " " }
  //             }
  //             // '¶' ; '⁋' '§'
  //             s"${pincls}${pinrep}"

  //           case _ =>
  //             s"${pinrep} "
  //         }

  //         pincol.box

  //       } else {
  //         "~".box
  //       }
  //     }
  //     val groupings = hjoin(
  //       vjoins(TB.right, pinCol),
  //       "  ",
  //       vjoins(TB.left, textCol)
  //     )

  //     groupings
  //   }

  //   vjoins(left,
  //     allText
  //   ).toString()
  // }




}
