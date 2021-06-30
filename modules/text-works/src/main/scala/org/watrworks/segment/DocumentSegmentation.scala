package org.watrworks
package segment

import ammonite.{ops => fs}, fs._

import geometry._
import utils.Timer.time
import textgrid._
import utils.QuickNearestNeighbors._
import utils.ExactFloats._
import extract._
import utils._
import utils.IdGenerator
import rsearch._

import TypeTags._
import org.watrworks.transcripts.Transcript

object DocumentSegmenter {

  def createSegmenter(
    documentId0: String @@ DocumentID,
    pdfPath: Path
  ): DocumentSegmenter = {

    val (pages, fontDefs0) = PdfBoxTextExtractor.extractPages(documentId0, pdfPath)

    val segmenter = new DocumentSegmenter {
      override val pageAtomsAndGeometry          = pages
      override val fontDefs                      = fontDefs0
      override val documentId                    = documentId0
      override val docStats: DocumentLayoutStats = new DocumentLayoutStats()
      override def scopeTags                     = Nil
    }

    segmenter
  }
}

trait DocumentSegmenter
  extends FontAndGlyphMetricsDocWide
  with TrapezoidAnalysis
  with GlyphTreeDocScope { self =>

  def runDocumentSegmentation(): Unit = {
    // TODO pass in extraction features:
    //   e.g., Per-page text, super/subscript escapes, dehyphenation, ...

    // TODO it would be useful to have font info written somewhere
    // docScope.docTraceLogs.trace { boxText(docScope.fontDefs.report()) }

    docScope.docStats.initTable[Int @@ PageNum, String @@ ScaledFontID, Int @@ FloatRep](
      "PagewiseLineWidths"
    )
    var stepNum = 0
    def stepTag(msg: String): String = {
      stepNum += 1
      s"step:${stepNum}. ${msg}"
    }

    def withPageSegmenters(stepName: String, f: PageSegmenter => Unit): Unit = {
      val tags = stepTag(stepName)
      time(stepName) {
        pageSegmenters.foreach { p =>
          p.traceLog.pushTag(tags)
          f(p)
          p.traceLog.popTag()
        }
      }
    }

    def withDocumentSegmenter(stepName: String, f: => Unit): Unit = {
      val tags = stepTag(stepName)
      time(stepName) {
        self.pushTag(tags)
        val _ = f
        self.popTag()
      }
    }

    withPageSegmenters("findContiguousGlyphSpans", _.findContiguousGlyphSpans())

    withDocumentSegmenter(
      "computeFontMetrics", {
        computeScaledFontHeightMetrics(LB.CharRunFontBaseline)
        computeScaledSymbolicFontMetrics()
      }
    )

    withPageSegmenters("findTextLineShapes", _.findTextLineShapesFromFontBaselines())

    withPageSegmenters("findMonoFontBlocks", _.findMonoFontBlocks())

    withDocumentSegmenter("collectMonoFontFeatures", { self.collectMonoFontFeatures() })

    withPageSegmenters("connectMonoFontBlocks", _.connectMonoFontBlocks())

    // withPageSegmenters("createColumnClusters", _.createColumnClusters())
    // withPageSegmenters("findContiguousBlocks", _.findContiguousBlocks(LB.BaselineMidriseBand))
    // withPageSegmenters("setTextForReprShapes", _.setTextForReprShapes())
    // withPageSegmenters("buildLinePairTrapezoids", _.buildLinePairTrapezoids())
    // withDocumentSegmenter("createFeatureVectors", { self.createFeatureVectors() })
    // withPageSegmenters("classifyLines", _.classifyLines())

    withPageSegmenters("pageStanzaConstruction", _.createPageStanzas())

  }
}

trait BaseDocumentSegmenter extends DocumentScopeTracing { self =>

  lazy val docScope = self

  val shapeIdGenerator = IdGenerator[ShapeID]()
  def pageAtomsAndGeometry: Seq[(Seq[ExtractedItem], PageGeometry)]

  // All extracted items across all pages, indexed by id, which equals extraction order
  lazy val extractedItemArray: Array[ExtractedItem] = {
    val extractedItemCount = (0 +: pageAtomsAndGeometry.map(_._1.length)).sum
    val itemArray          = new Array[ExtractedItem](extractedItemCount + 1)
    pageAtomsAndGeometry.foreach { case (items, _) =>
      items.foreach { item => itemArray(item.id.unwrap) = item }
    }
    itemArray
  }

  lazy val shapeIndexes: Map[Int @@ PageNum, ShapeIndex] = {
    pageAtomsAndGeometry.map { case (items @ _, pageGeometry) =>
      (
        pageGeometry.pageNum,
        LabeledShapeIndex.empty[GeometricFigure, DocSegShape[GeometricFigure]](
          shapeIdGenerator
        )
      )
    }.toMap
  }

  def getNumberedPages(): Seq[Int @@ PageNum] =
    pageAtomsAndGeometry.zipWithIndex.map { case (a @ _, b) =>
      PageNum(b)
    }

  lazy val pageSegmenters = {

    def createPageSegmenters(): Seq[PageSegmenter] = for {
      pageNum <- getNumberedPages()
    } yield PageSegmenter(pageNum, self)

    createPageSegmenters()
  }

  def getLabeledShapeIndex(pageNum: Int @@ PageNum) = shapeIndexes(pageNum)

  def getPageGeometry(p: Int @@ PageNum) = pageAtomsAndGeometry(p.unwrap)._2

  def fontDefs: FontDefs

  def docStats: DocumentLayoutStats

  def documentId: String @@ DocumentID

  def getPagewiseLinewidthTable()
    : TabularData[Int @@ PageNum, String @@ ScaledFontID, List[Int @@ FloatRep], Unit, Unit] = {
    docScope.docStats.getTable[Int @@ PageNum, String @@ ScaledFontID, List[Int @@ FloatRep]](
      "PagewiseLineWidths"
    )
  }

  def getFontsWithDocwideOccurrenceCounts(): Seq[(String @@ ScaledFontID, Int)] = {
    fontDefs.fontProperties.to(Seq).flatMap { fontProps =>
      if (fontProps.isNatLangFont()) {
        fontProps.getScalingFactors().map { scalingFactor =>
          val docWideCount = fontProps.totalGlyphOccurrenceCounts
            .computeColMarginals(0)(_ + _)
            .getColMarginal(scalingFactor)
            .getOrElse(0)

          (fontProps.getFontIdentifier(scalingFactor), docWideCount)
        }
      } else List[(String @@ ScaledFontID, Int)]()
    }
  }

  import scalaz.Scalaz.stringInstance

  protected def printScaledFontTableData(): Unit = {

    val allFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang = true) ++ docScope.fontDefs
      .getFontIdentifiers(isNatLang = false)
    val scaledFontIDs = allFontIds.sorted
    val dbg           = scaledFontIDs.mkString("{\n  ", "\n  ", "\n}")
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

    import GuavaHelpers._
    println("Most Common Widths / ranges\n\n")
    println(widthRangeCentroidDisplay.showBox())

    val widthRangeCentroids = pagewiseLineWidthTable.map { widths =>
      val widthClusters = qnn(widths, tolerance = 1.0)
        .filter(_.size() > 1)
        .sortBy(_.size())
        .reverse
        .headOption
        .map { bin => bin.size() } getOrElse { 0 }

      widthClusters
    }

    val marginalSizes = widthRangeCentroids.foldLeftColumns(List[Int]()) { case (acc, e) =>
      acc :+ e
    }

    val marginalSizesStr = marginalSizes.mkString("\n  ", "\n  ", "\n")

    println("Marginal Sizes")
    println(marginalSizesStr)

    println(docScope.fontDefs.report())
  }

  def createTranscript(): Transcript = {
    val documentId = self.documentId
    // val pages = self.pageAtomsAndGeometry.map {
    //   case (pageItems, pageBounds) => {

    //     val glyphs = pageItems.map(pageItem => {
    //       Transcript.Glyph(
    //         pageItem.strRepr(),
    //         GlyphID(pageItem.id.unwrap),
    //         pageItem.minBBox,
    //         None
    //       )
    //     })

    //     Transcript.Page(
    //       pageBounds.pageNum,
    //       pageBounds.bounds,
    //       glyphs.to(List)
    //     )
    //   }
    // }

    val pages = pageSegmenters.map { pageSegmenter =>
      val pageItems  = pageSegmenter.pageItems
      val pageBounds = pageSegmenter.pageGeom
      val pageLabels = pageSegmenter.traceLog.getLogsAsLabels()

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
        glyphs.to(List),
        pageLabels.to(List)
      )
    }

    val stanzas = pageSegmenters.map { pageSegmenter =>
      // TODO this should get a list of text grids, one per stanza
      val textGrid = pageSegmenter.getTextGrid(None)

      // TODO refactor textGridToStanza()

      val lines = textGrid
        .rows()
        .map(row => {
          val text = row.toText()

          val glyphRefs = row
            .cells()
            .map(_ match {
              case _ @TextGrid.PageItemCell(headItem, _, _, _) =>
                Transcript.GlyphRef.I(headItem.id.unwrap)

              case _ @TextGrid.InsertCell(char, _) =>
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

    val docScopeLogs = docScope.getLogsAsLabels()

    Transcript(
      documentId,
      pages.to(List),
      labels = docScopeLogs.to(List),
      stanzas.to(List)
    )
  }
}
