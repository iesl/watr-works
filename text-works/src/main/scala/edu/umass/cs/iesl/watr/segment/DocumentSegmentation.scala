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

trait DocumentLevelFunctions extends DocumentScopeSegmenter
    with FontAndGlyphMetricsDocWide

trait DocumentSegmentation extends DocumentLevelFunctions { self =>


  protected[segment] def init(): Unit = {
    initLabeledShapeIndexes()
  }

  def getNumberedPages(): Seq[(Int@@PageID, Int@@PageNum)] =
    docStore.getPages(docId).zipWithIndex.map {
      case (a, b) => (a, PageNum(b))
    }

  private def initLabeledShapeIndexes(): Unit = {
    pageAtomsAndGeometry.foreach { case (extractedItems, pageGeometry) =>
      val pageId = docStore.addPage(docId, pageGeometry.pageNum)
      docStore.setPageGeometry(pageId, pageGeometry.bounds)
    }
  }

  lazy val pageSegmenters = {

    def createPageSegmenters(): Seq[PageSegmenter] = for {
      (pageId, pageNum) <- getNumberedPages()
    } yield PageSegmenter(pageId, pageNum, self)

    createPageSegmenters()
  }


  // protected def joinPageTextGrids(): TextGrid = {
  //   val textGrids = pageSegmenters.map {
  //     _.getPageTextGrid()
  //   }
  //   val allRows = textGrids.map(_.rows).flatten
  //   TextGrid.fromRows(docScope.stableId,  allRows)
  // }

  protected def outputTableData(): Unit = {

    val scaledFontIDs = docScope.fontDefs.getFontIdentifiers.sorted
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

    docScope.docStats.initTable[Int@@PageNum, String@@ScaledFontID, Int@@FloatRep]("PagewiseLineWidths")

    time("findContiguousGlyphSpans") {
      pageSegmenters.foreach { p =>
        p.findContiguousGlyphSpans()
      }
    }

    time("findLineLayoutMetrics") {
      findLineLayoutMetrics(LB.CharRunFontBaseline)
    }

    // outputTableData();

    time("generatePageRules") {
      pageSegmenters.foreach { p =>
        p.generatePageRules(LB.CharRunFontBaseline, LB.CapDescenderBand)
      }
    }

    time("findContiguousBlocks") {
      pageSegmenters.foreach { p =>
        p.findContiguousBlocks(LB.CapDescenderBand)
      }
    }

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
