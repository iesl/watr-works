package org.watrworks
package segment

import scala.{ collection => sc }
import sc.Seq

import geometry._
import geometry.syntax._
import extract._
import utils.ExactFloats._
import watrmarks._
// import textboxing.{TextBoxing => TB}, TB._
import utils.SlicingAndDicing._

import TypeTags._

trait GlyphRuns extends PageScopeSegmenter
    with FontAndGlyphMetrics { self =>

  import SegmentationSystem._

  def findContiguousGlyphSpans(): Unit = {

    recordNatLangCharSpans(
      LB.CharRunFontBaseline,
      findNatLangBaselineRuns(retainNatLang=true)
    )

    // combineCombiningMarks()

    indexPathRegions()

    indexImageRegionsAndDeleteOverlaps()

    val symbolicLangCharRuns = findSymbolicCharRuns()

    symbolicLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeAndSetItems(item.minBBox, LB.SymbolicGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }
    }

    traceLog.trace { labeledShapes(LB.NatLangGlyph) tagged "All NatLang Glyph Rects" }
    traceLog.trace { labeledShapes(LB.SymbolicGlyph) tagged "All Symbolic Glyph Rects" }

    initSymbolicCharSpans(symbolicLangCharRuns)

  }



  private def findNatLangBaselineRuns(retainNatLang: Boolean): Seq[Seq[ExtractedItem.CharItem]] = {
    pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filter(_.fontProps.isNatLangFont() == retainNatLang)
      .groupByPairs { case (item1, item2) =>
        item2.glyphProps.prevSimilar == item1.id
      }
  }


  private def findSymbolicCharRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    val charRuns = pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filterNot(_.fontProps.isNatLangFont())
      .groupByPairs {
        case (item1, item2) =>
          lazy val consecutive = itemsAreConsecutive(item1, item2)
          lazy val leftToRight = item1.minBBox.left < item2.minBBox.left
          lazy val colinear = item1.minBBox.isNeitherAboveNorBelow(item2.minBBox)

          consecutive && leftToRight && colinear
      }

    charRuns
  }



  // private def combineCombiningMarks(): Unit = {
  //   val combiningMarks = pageScope.pageItems.toSeq
  //     .collect { case item: ExtractedItem.CombiningMark => item }

  //   combiningMarks.foreach { combiningMark =>
  //     val cmShape = indexShapeAndSetItems(combiningMark.minBBox, LB.Glyph, combiningMark)
  //     traceLog.trace { shape(cmShape) }
  //   }
  // }


  private def indexPathRegions(): Unit = {
    val pathShapes =
      pageScope.pageItems.toSeq
        .filter { _.isInstanceOf[ExtractedItem.PathItem] }
        .map { item => indexShape(item.minBBox, LB.PathBounds)}

    traceLog.trace { shape(pathShapes:_*) tagged "Path Line Bounds" }
  }



  private def initSymbolicCharSpans(symbolicRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {
    symbolicRuns.map { charRun =>
      val xSorted = charRun.sortBy { _.minBBox.left }
      val p1 = xSorted.head.minBBox.toPoint(Dir.Center)
      val p2 = xSorted.last.minBBox.toPoint(Dir.Center)

      val baseLine = Line(p1, p2)

      val symbolicGlyphLine = indexShape(baseLine, LB.SymbolicGlyphLine)

      setExtractedItemsForShape(symbolicGlyphLine, charRun)

      symbolicGlyphLine
    }.asLineShapes

    traceLog.trace { labeledShapes(LB.SymbolicGlyphLine) }
  }

  private def recordNatLangCharSpans(spanLabel: Label, natLangCharRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    natLangCharRuns.foreach { charRun =>
      val charItems = charRun.map(_.asInstanceOf[ExtractedItem.CharItem])

      val glyphShapes = charItems.map { item =>
        indexShapeAndSetItems(item.minBBox, LB.NatLangGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }

      val baseLine = createCharRunFontBaseline(charItems)

      val baselineShape = indexShape(baseLine, spanLabel)

      glyphShapes.foreach { headGlyphShape =>
        setLinkedShape(headGlyphShape, LB.CharRunFontBaseline, baselineShape)
      }

      val fontIds = charItems.map{ _.scaledFontId }.toSet

      setFontsForShape(baselineShape, fontIds)

      setExtractedItemsForShape(baselineShape, charRun)

      baselineShape
    }

    traceLog.trace { labeledShapes(spanLabel) tagged "Initial Font Baselines" }
  }

  private def createCharRunFontBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted = charRun.sortBy { _.minBBox.left }
    val runBeginPt =  Point(xSorted.head.minBBox.left, xSorted.head.fontBbox.bottom)
    val runEndPt = Point(xSorted.last.minBBox.right, xSorted.last.fontBbox.bottom)
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

    traceLog.trace { shape(deletedShapes:_*) tagged "Deleted Intersect Image Bounds" }
    traceLog.trace { labeledShapes(LB.Image) tagged "Image Regions" }
  }

}
