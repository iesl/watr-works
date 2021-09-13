package org.watrworks
package segment

import geometry._
import geometry.syntax._
import extract._
import utils.ExactFloats._
import watrmarks._
import utils.SlicingAndDicing._
import TypeTags._

trait GlyphRuns extends BasePageSegmenter with FontAndGlyphMetrics { self =>


  def findContiguousGlyphSpans(): Unit = {
    labelHomogenousFontRuns()
    indexPathRegions()
    indexImageRegions()
  }

  private def labelHomogenousFontRuns(): Unit = {
    traceLog.startTask("LabelFontRuns")

    val natLangRuns = findNatLangBaselineRuns()
    applyLabelToRun(LB.CharRunFontBaseline, findFontBaseline, natLangRuns)

    val symbolCharRuns = findSymbolicCharRuns()
    applyLabelToRun(LB.SymbolicGlyphLine, findCenterLine, symbolCharRuns)

    traceLog.endTask()
  }

  private def findNatLangBaselineRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filter(_.fontProps.isNatLangFont())
      .groupByPairs { case (item1, item2) =>
        val sameScaledFont   = item1.scaledFontId == item2.scaledFontId
        val sameFontBaseline = item1.fontBbox.bottom == item2.fontBbox.bottom
        sameScaledFont && sameFontBaseline

      }
  }

  private def findSymbolicCharRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filterNot(_.fontProps.isNatLangFont())
      .groupByPairs { case (item1, item2) =>
        val sameScaledFont   = item1.scaledFontId == item2.scaledFontId
        lazy val consecutive = itemsAreConsecutive(item1, item2)
        lazy val leftToRight = item1.minBBox.left < item2.minBBox.left
        lazy val colinear    = item1.minBBox.isNeitherAboveNorBelow(item2.minBBox)

        sameScaledFont && consecutive && leftToRight && colinear
      }
  }

  private def applyLabelToRun(
    spanLabel: Label,
    makeLine: Seq[ExtractedItem.CharItem] => Line,
    natLangCharRuns: Seq[Seq[ExtractedItem.CharItem]]
  ): Unit = {
    traceLog.startTask(spanLabel.fqn)

    natLangCharRuns.foreach { charItems =>
      val line     = makeLine(charItems)
      val baseline = indexShape(line, spanLabel)

      charItems.foreach { item =>
        val glyphShape = indexShape(item.minBBox, LB.Glyph)
        glyphShape.setAttr(ExtractedChar)(item)
      }

      val fontIds = charItems.map { _.scaledFontId }.toSet

      if (fontIds.size != 1) {
        println(
          s"""Warning: Run has multiple fonts (${fontIds.size}) = ${fontIds.mkString(", ")}"""
        )
      }

      fontIds.headOption.foreach(primaryFontId => {
        baseline.setAttr(PrimaryFont)(primaryFontId)
      })

      baseline.setAttr(Fonts)(fontIds)
      baseline.setAttr(ExtractedChars)(charItems)

      traceLog.trace {
        val fontIdStr = fontIds.mkString(";")
        createLabelOn(
          spanLabel.fqn,
          line
        ).withProp("Fonts", fontIdStr)
      }
    }

    traceLog.endTask()
  }

  private def findCenterLine(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted = charRun.sortBy { _.minBBox.left }
    val p1      = xSorted.head.minBBox.toPoint(M3.Center)
    val p2      = xSorted.last.minBBox.toPoint(M3.Center)
    Line(p1, p2)
  }

  private def findFontBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted    = charRun.sortBy { _.minBBox.left }
    val runBeginPt = Point(xSorted.head.minBBox.left, xSorted.head.fontBbox.bottom)
    val runEndPt   = Point(xSorted.last.minBBox.right, xSorted.last.fontBbox.bottom)
    Line(runBeginPt, runEndPt)
  }

  private def indexPathRegions(): Unit = {
    traceLog.startTask("LabelPathRegions")

    pageScope.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.PathItem] }
      .foreach { item => indexShape(item.minBBox, LB.PathBounds) }

    traceLog.trace { labeledShapes(LB.PathBounds) }
    traceLog.endTask()
  }

  // private def indexImageRegionsAndDeleteOverlaps(): Unit = {
  private def indexImageRegions(): Unit = {
    traceLog.startTask("LabelImageRegions")
    // val deletedShapes =
    pageScope.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .foreach { imageItem =>
        indexShape(imageItem.minBBox, LB.Image)
      // val baseLines: Seq[LineShape] = searchForLines(imageItem.minBBox, LB.CharRunFontBaseline)
      // deleteShapes(baseLines)
      // baseLines
      }

    // traceLog.trace { shape(deletedShapes: _*) tagged "Deleted Intersect Image Bounds" }
    // traceLog.trace { labeledShapes(LB.Image) tagged "Image Regions" }
    traceLog.endTask()
  }

}
