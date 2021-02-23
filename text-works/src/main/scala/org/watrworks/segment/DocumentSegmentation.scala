package org.watrworks
package segment

import ammonite.{ops => fs}, fs._

import extract._
import utils.Timer.time
import utils.ExactFloats._
import textgrid._
import utils.QuickNearestNeighbors._

import TypeTags._
import org.watrworks.transcripts.Transcript

trait DocumentLevelFunctions
    extends DocumentScopeSegmenter
    with FontAndGlyphMetricsDocWide
    with MarginalMatterDetectionDocScope
    with TrapezoidAnalysis

trait DocumentSegmentation extends DocumentLevelFunctions { self =>

  protected def outputTableData(): Unit = {

    val allFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang = true) ++ docScope.fontDefs
      .getFontIdentifiers(isNatLang = false)
    val scaledFontIDs = allFontIds.sorted
    val dbg = scaledFontIDs.mkString("{\n  ", "\n  ", "\n}")
    println(s" Font IDs: ${dbg}")

    val pagewiseLineWidthTable = getPagewiseLinewidthTable()

    val widthRangeCentroidDisplay = pagewiseLineWidthTable.map { widths =>
      val widthClusters = qnn(widths, tolerance = 1.0)
        .filter(_.size() > 1)
        .sortBy(_.size())
        .reverse
        .headOption
        .map { bin =>
          bin.toCentroidRangeString()
        } getOrElse { "-" }

      widthClusters
    }

    println("Most Common Widths / ranges\n\n")
    println(widthRangeCentroidDisplay.toReportBox())

    val widthRangeCentroids = pagewiseLineWidthTable.map { widths =>
      val widthClusters = qnn(widths, tolerance = 1.0)
        .filter(_.size() > 1)
        .sortBy(_.size())
        .reverse
        .headOption
        .map { bin => bin.size() } getOrElse { 0 }

      widthClusters
    }

    val marginalSizes = widthRangeCentroids.mapColumns(0) { case (acc, e) => acc + e }

    val marginalSizesStr = marginalSizes.mkString("\n  ", "\n  ", "\n")

    println(s"Marginal Sizes ${}")
    println(marginalSizesStr)

    println(docScope.fontDefs.report())
  }

  def runDocumentSegmentation(): Unit = {
    // TODO pass in extraction features:
    //   e.g., Per-page text, super/subscript escapes, dehyphenation, ...

    docScope.docTraceLogs.trace { boxText(docScope.fontDefs.report()) }

    docScope.docTraceLogs.trace {
      fontSummaries(
        docScope.fontDefs.getFontSummaries()
      )
    }

    docScope.docStats.initTable[Int @@ PageNum, String @@ ScaledFontID, Int @@ FloatRep](
      "PagewiseLineWidths"
    )

    time("findContiguousGlyphSpans") {
      pageSegmenters.foreach { p =>
        p.findContiguousGlyphSpans()
      }
    }

    time("computeScaledFontHeightMetrics") {
      computeScaledFontHeightMetrics(LB.CharRunFontBaseline)
    }

    time("computeScaledSymbolicFontMetrics") {
      computeScaledSymbolicFontMetrics()
    }

    // outputTableData();

    time("findLineShapesFromFontBaselines") {
      pageSegmenters.foreach { p =>
        p.findTextLineShapesFromFontBaselines()
      }
    }

    time("createColumnClusters") {
      pageSegmenters.foreach { p =>
        p.createColumnClusters()
      }
    }

    time("findContiguousBlocks") {
      pageSegmenters.foreach { p =>
        p.findContiguousBlocks(LB.BaselineMidriseBand)
      }
    }

    time("setTextForReprShapes") {
      pageSegmenters.foreach { p =>
        p.setTextForReprShapes()
      }
    }

    time("buildLinePairTrapezoids") {
      pageSegmenters.foreach { p =>
        p.buildLinePairTrapezoids()
      }
      self.createFeatureVectors()
    }

    time("classifyLines") {
      pageSegmenters.foreach { p =>
        p.classifyLines()
      }
    }

    // time("findRepeatedMarginalLines") {
    //   findRepeatedMarginalLines()
    // }
    time("pageStanzaConstruction") {
      // TODO pass in features for stanzas
      pageSegmenters.foreach { p =>
        p.createPageStanzas()
      }
    }

  }

  def createTranscript(): Transcript = {
    val stableId = self.stableId
    val pages = self.pageAtomsAndGeometry.map {
      case (pageItems, pageBounds) => {
        // val glyphBuf = new ArrayBuffer[Transcript.Glyph](pageItems.length)

        val glyphs = pageItems.map(pageItem => {
          Transcript.Glyph(
            pageItem.strRepr(),
            GlyphID(pageItem.id.unwrap),
            pageItem.minBBox,
            None
          )
        })

        Transcript.Page(
          pageBounds.pageNum,
          pageBounds.bounds,
          glyphs.to(List)
        )
      }
    }

    val stanzas = pageSegmenters.map { pageSegmenter =>
      // TODO this should get a list of text grids, one per stanza
      val textGrid = pageSegmenter.getTextGrid(None)

      // TODO refactor textGridToStanza()

      val lines =
        textGrid
        .rows()
        .map(row => {
          val text = row.toText()

          val glyphRefs = row
            .cells()
            .map(_ match {

              case cell @ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
                // val props = if (tailItems.isEmpty) None else {
                //   val tailGlyphs = tailItems.map(pageItem => {
                //     Transcript.Glyph(
                //       pageItem.bbox,
                //       props = Some(Transcript.GlyphProps(
                //       ))
                //     )
                //   }).toList

                //   Some(Transcript.GlyphProps(
                //     gs = Some(tailGlyphs)
                //   ))
                // }

                Transcript.GlyphRef.I(headItem.id.unwrap)

              case cell @ TextGrid.InsertCell(char, insertAt) =>
                // Transcript.Glyph(
                //   cell.pageRegion.bbox,
                //   props = Some(Transcript.GlyphProps(
                //   )))

                Transcript.GlyphRef.S(char.toString())
            })
            .toList

          Transcript.StanzaLine(text, glyphRefs)
        })
        .toList

      Transcript.Stanza(
        StanzaID(0),
        lines,
        labels = List()
      )
    }

    Transcript(
      stableId,
      pages.to(List),
      labels = List(),
      stanzas.to(List)
    )
  }

}

object DocumentSegmenter {

  def createSegmenter(
    stableId0: String @@ DocumentID,
    pdfPath: Path
  ): DocumentSegmentation = {

    val (pages, fontDefs0) = PdfBoxTextExtractor.extractPages(stableId0, pdfPath)

    val segmenter = new DocumentSegmentation {
      override val pageAtomsAndGeometry = pages
      override val fontDefs = fontDefs0
      override val stableId = stableId0
      override val docStats: DocumentLayoutStats = new DocumentLayoutStats()
    }

    segmenter
  }
}
