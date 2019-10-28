package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}, fs._


import corpora.DocumentZoningApi
import extract._
import rtrees._
import utils.Timer.time
import utils.ExactFloats._
import textgrid._
import utils.QuickNearestNeighbors._

import TypeTags._

trait DocumentLevelFunctions
    extends DocumentScopeSegmenter
    with FontAndGlyphMetricsDocWide
    with MarginalMatterDetectionDocScope

trait DocumentSegmentation extends DocumentLevelFunctions { self =>

  protected[segment] def init(): Unit = {
    initLabeledShapeIndexes()
  }

  private def initLabeledShapeIndexes(): Unit = {
    pageAtomsAndGeometry.foreach { case (extractedItems, pageGeometry) =>
      val pageId = docStore.addPage(docId, pageGeometry.pageNum)
      docStore.setPageGeometry(pageId, pageGeometry.bounds)
    }
  }


  protected def outputTableData(): Unit = {

    val allFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang=true) ++ docScope.fontDefs.getFontIdentifiers(isNatLang=false)
    val scaledFontIDs = allFontIds.sorted
    val dbg = scaledFontIDs.mkString("{\n  ", "\n  ", "\n}")
    println(s" Font IDs: ${dbg}")

    val pagewiseLineWidthTable = getPagewiseLinewidthTable()

    val widthRangeCentroidDisplay = pagewiseLineWidthTable.map{ widths =>
      val widthClusters = qnn(widths, tolerance=1.0)
        .filter( _.size() > 1 )
        .sortBy(_.size())
        .reverse
        .headOption
        .map{ bin =>
          bin.toCentroidRangeString()
        } getOrElse { "-" }

      widthClusters
    }

    println("Most Common Widths / ranges\n\n")
    println(widthRangeCentroidDisplay.toReportBox())

    val widthRangeCentroids = pagewiseLineWidthTable.map{ widths =>
      val widthClusters = qnn(widths, tolerance=1.0)
        .filter( _.size() > 1 )
        .sortBy(_.size())
        .reverse
        .headOption
        .map{ bin => bin.size() } getOrElse { 0 }

      widthClusters
    }

    val marginalSizes = widthRangeCentroids.mapColumns(0) { case (acc, e) => acc + e  }

    val marginalSizesStr = marginalSizes.mkString("\n  ", "\n  ", "\n")

    println(s"Marginal Sizes ${}")
    println(marginalSizesStr)

    println(docScope.fontDefs.report())
  }

  def runDocumentSegmentation(): Unit = {

    docScope.docTraceLogs.trace { boxText(docScope.fontDefs.report()) }

    docScope.docTraceLogs.trace {
      fontSummaries(
        docScope.fontDefs.getFontSummaries()
      )
    }

    docScope.docStats.initTable[Int@@PageNum, String@@ScaledFontID, Int@@FloatRep]("PagewiseLineWidths")

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
    }

    time("classifyLines") {
      pageSegmenters.foreach { p =>
        p.classifyLines()
      }
    }

    // time("findRepeatedMarginalLines") {
    //   findRepeatedMarginalLines()
    // }

  }

}

object DocumentSegmenter {
  import rtrees._

  def createSegmenter(
    stableId0: String@@DocumentID,
    pdfPath: Path,
    docStore0: DocumentZoningApi
  ): DocumentSegmentation = {

    val (pages, fontDefs0) = PdfBoxTextExtractor.extractPages(stableId0, pdfPath)

    val segmenter = new DocumentSegmentation {
      override val pageAtomsAndGeometry = pages
      override val fontDefs = fontDefs0

      override val docStore: DocumentZoningApi = docStore0
      override val stableId = stableId0
      override val docId = docStore0.addDocument(stableId0)

      override val docStats: DocumentLayoutStats = new DocumentLayoutStats()

    }

    segmenter.init()
    segmenter
  }
}
