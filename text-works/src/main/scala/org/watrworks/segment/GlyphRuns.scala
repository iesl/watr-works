package org.watrworks
package segment

import geometry._
import geometry.syntax._
import extract._
import utils.ExactFloats._
import watrmarks._
import utils.SlicingAndDicing._

import TypeTags._

trait GlyphRuns extends PageScopeSegmenter with FontAndGlyphMetrics { self =>

  import SegmentationSystem._

  def findContiguousGlyphSpans(): Unit = {
    recordNatLangCharSpans(LB.CharRunFontBaseline, findNatLangBaselineRuns())

    indexPathRegions()

    indexImageRegionsAndDeleteOverlaps()

    val symbolicLangCharRuns = findSymbolicCharRuns()

    symbolicLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeAndSetItems(item.minBBox, LB.SymbolicGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }
    }

    // traceLog.trace { labeledShapes(LB.NatLangGlyph) tagged "All NatLang Glyph Rects" }
    // traceLog.trace { labeledShapes(LB.SymbolicGlyph) tagged "All Symbolic Glyph Rects" }

    initSymbolicCharSpans(symbolicLangCharRuns)

    // trace.snapshot(LB.NatLangGlyph, LB.SymbolicGlyph)
  }

  protected def findNatLangBaselineRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filter(_.fontProps.isNatLangFont())
      .groupByPairs { case (item1, item2) =>
        item2.glyphProps.prevSimilar == item1.id
      }
  }

  protected def findSymbolicCharRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    val charRuns = pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filterNot(_.fontProps.isNatLangFont())
      .groupByPairs { case (item1, item2) =>
        lazy val consecutive = itemsAreConsecutive(item1, item2)
        lazy val leftToRight = item1.minBBox.left < item2.minBBox.left
        lazy val colinear    = item1.minBBox.isNeitherAboveNorBelow(item2.minBBox)

        consecutive && leftToRight && colinear
      }

    charRuns
  }

  private def indexPathRegions(): Unit = {
    val pathShapes =
      pageScope.pageItems.toSeq
        .filter { _.isInstanceOf[ExtractedItem.PathItem] }
        .map { item => indexShape(item.minBBox, LB.PathBounds) }

    traceLog.trace { shape(pathShapes: _*) tagged "Path Line Bounds" }
  }

  private def initSymbolicCharSpans(symbolicRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {
    symbolicRuns.map { charRun =>
      val xSorted = charRun.sortBy { _.minBBox.left }
      val p1      = xSorted.head.minBBox.toPoint(Dir.Center)
      val p2      = xSorted.last.minBBox.toPoint(Dir.Center)

      val baseLine = Line(p1, p2)

      val symbolicGlyphLine = indexShape(baseLine, LB.SymbolicGlyphLine)

      setExtractedItemsForShape(symbolicGlyphLine, charRun)

      symbolicGlyphLine
    }.asLineShapes

    traceLog.trace { labeledShapes(LB.SymbolicGlyphLine) }
  }

  private def recordNatLangCharSpans(
    spanLabel: Label,
    natLangCharRuns: Seq[Seq[ExtractedItem.CharItem]]
  ): Unit = {

    natLangCharRuns.foreach { charItems =>

      // Index each glyph as both NatLangGlyph & Glyph
      val glyphShapes = charItems.map { item =>
        indexShapeAndSetItems(item.minBBox, LB.NatLangGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }

      // Create a line corresponding to font bbox underline
      val baseLine = createCharRunFontBaseline(charItems)

      val baselineShape = indexShape(baseLine, spanLabel)

      glyphShapes.foreach { headGlyphShape =>
        setLinkedShape(headGlyphShape, LB.CharRunFontBaseline, baselineShape)
      }

      val fontIds = charItems.map { _.scaledFontId }.toSet

      setFontsForShape(baselineShape, fontIds)

      setExtractedItemsForShape(baselineShape, charItems)

      baselineShape
    }

    traceLog.trace { labeledShapes(spanLabel) tagged "Initial Font Baselines" }
  }

  private def createCharRunFontBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted    = charRun.sortBy { _.minBBox.left }
    val runBeginPt = Point(xSorted.head.minBBox.left, xSorted.head.fontBbox.bottom)
    val runEndPt   = Point(xSorted.last.minBBox.right, xSorted.last.fontBbox.bottom)
    Line(runBeginPt, runEndPt)
  }

  private def indexImageRegionsAndDeleteOverlaps(): Unit = {
    val deletedShapes = pageScope.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .flatMap { imageItem =>
        indexShape(imageItem.minBBox, LB.Image)
        val baseLines: Seq[LineShape] = searchForLines(imageItem.minBBox, LB.CharRunFontBaseline)
        deleteShapes(baseLines)
        baseLines
      }

    traceLog.trace { shape(deletedShapes: _*) tagged "Deleted Intersect Image Bounds" }
    traceLog.trace { labeledShapes(LB.Image) tagged "Image Regions" }
  }

}
