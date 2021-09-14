package org.watrworks
package segment

import geometry._
import geometry.syntax._
import pdfex._
import utils.ExactFloats._
import utils.FunctionalHelpers._
import watrmarks._

trait LineLayout extends BasePageSegmenter with FontAndGlyphMetrics with TextBlockGrouping { self =>

  protected def segmentSuperSubScripts(): Unit = {
    val textlineReprShapes = getLabeledRects(LB.BaselineMidriseBand)
    textlineReprShapes.foreach { textlineReprShape =>
      findSuperSubScriptsInLine(textlineReprShape)
    }
  }

  private def findSuperSubScriptsInLine(
    midriseBaselineBand: RectShape
  ): Unit = {

    val primaryFontId = midriseBaselineBand.getAttr(PrimaryFont).get

    // val primaryFontId = getPrimaryFontForShape(midriseBaselineBand).get
    val midriseBaselineRect = midriseBaselineBand.shape
    val reprShapeLeft       = midriseBaselineRect.left
    val reprShapeRight      = midriseBaselineRect.right
    // val capDescentRect  = midriseBaselineBand.shape

    def clipped(slice: Rect): Option[Rect] = {
      slice.clipLeftRight(reprShapeLeft, reprShapeRight)
    }

    for {
      // offsetsAtLine  <- getFontOffsetsForShape(midriseBaselineBand)
      offsetsAtLine <- midriseBaselineBand.getAttr(FontOffsets)
      ascentDescentRect <- offsetsAtLine
                             .sliceBetween(
                               _.ascentLine,
                               _.descentLine,
                               pageGeometry
                             )
                             .flatMap(clipped(_))

      baselineMidriseHeight   = offsetsAtLine.distanceBetween(_.baseLine, _.midriseLine)
      baselineMidriseMidpoint = offsetsAtLine.midriseLine + (baselineMidriseHeight / 2)

      topMidriseBand     <- ascentDescentRect.splitHorizontal(baselineMidriseMidpoint)._1
      baselineBottomBand <- ascentDescentRect.splitHorizontal(offsetsAtLine.baseLine)._2

      // (_, maybeBaselineBottomBand) <- midriseBaselineRect.splitHorizontal(offsetsAtLine.baseLine)

      (_, maybeMidriseBaselineCenterBand) =
        midriseBaselineRect.splitHorizontal(baselineMidriseMidpoint)

      midriseToplineHeight         = offsetsAtLine.midriseLine - offsetsAtLine.topLine
      midriseToplineMidpoint       = offsetsAtLine.topLine + midriseToplineHeight
      (maybeMidriseToplineBand, _) = ascentDescentRect.splitHorizontal(midriseToplineMidpoint)

      midriseBaselineCenterBand <- maybeMidriseBaselineCenterBand
      midriseToplineCenterBand  <- maybeMidriseToplineBand
    } {

      // val topMidriseBand = maybeTopMidriseBand.orDie(s"Could not split CapDescenderBand (${ascentDescentRect}) into top-midrise / __ at ${offsetsAtLine.midriseLine}; ${offsetsAtLine}")
      // val baselineBottomBand = maybeBaselineBottomBand.orDie(s"Could not split CapDescenderBand (${ascentDescentRect}) into __ / baseline-bottom at ${offsetsAtLine.baseLine}; ${offsetsAtLine}")
      // val midriseBaselineCenterBand = maybeMidriseBaselineCenterBand.orDie(s"Could not split MidriseBaselineRect (${midriseBaselineRect}) into __ / midrise-baseline-center at ${baselineMidriseMidpoint.pp()}; height:${baselineMidriseHeight.pp()} ${offsetsAtLine}")
      // val midriseToplineCenterBand = maybeMidriseToplineBand.orDie(s"Could not split CapDescenderBand (${ascentDescentRect}) into midrise-topline-center / __ at ${midriseToplineMidpoint}; ${offsetsAtLine}")

      traceLog.trace {
        traceLog.figure(topMidriseBand) tagged s"Top-Midrise Band - Strictly above Subscript"
      }
      traceLog.trace {
        traceLog.figure(baselineBottomBand) tagged s"Baseline-Bottom Band - Intersects Subscript"
      }
      // traceLog.trace {
      //   traceLog.figure(midriseBaselineCenterBand) tagged s"Midrise-Baseline Center -> Bottom Band - Strictly below Superscript"
      // }
      // traceLog.trace {
      //   traceLog.figure(midriseToplineCenterBand) tagged s"Midrise-Topline Center -> Topline Band - Intersects Superscript"
      // }

      val charsInBand = getCharsForShape(midriseBaselineBand)

      // superscript = strictly above the midrise-baseline center line && intersects midrise-topline center -> topline band
      val eitherSuperScriptOrNot = collectSpanEither[ExtractedItem.CharItem](
        charsInBand,
        { c =>
          val charFontMidpoint    = c.fontBbox.toPoint(M3.Center)
          val fontIsAboveBaseline = offsetsAtLine.baseLine > c.fontBbox.bottom
          val isRaised            = midriseBaselineCenterBand.isStrictlyBelow(charFontMidpoint.y)
          val intersects          = midriseToplineCenterBand.intersects(c.fontBbox)

          (c.scaledFontId != primaryFontId
          && isRaised
          && intersects
          && fontIsAboveBaseline
          && !c.isLigature)
        }
      )

      val superScriptChars = eitherSuperScriptOrNot.collect { case Right(i) => i }

      superScriptChars.foreach { charSpan =>
        val minBounds = charSpan.map(_.minBBox).reduce(_ union _)
        traceLog.trace {
          traceLog.figure(minBounds) tagged s"Superscript MinBounds"
        }
      }

      // subscript = strictly below midriseLine && intersect (baseline+delta)-descender band
      val eitherSubScriptOrNot = collectSpanEither[ExtractedItem.CharItem](
        charsInBand,
        { c =>
          // val charFontOffsets = docScope.fontDefs.getScaledFontOffsets(c.scaledFontId)
          // val charOffsetsAtBaseline = charFontOffsets.forFontBoxBottom(c.fontBbox.bottom)
          // val charMidriseLine = charOffsetsAtBaseline.midriseLine

          // val charMidpoint = c.minBBox.toPoint(M3.Center)
          val charMidpoint        = c.fontBbox.toPoint(M3.Center)
          val fontIsBelowBaseline = offsetsAtLine.baseLine < c.fontBbox.bottom
          // val isLowered = c.minBBox.isStrictlyBelow(topMidriseBand)
          val isLowered                = topMidriseBand.isStrictlyAbove(charMidpoint.y)
          val intersectsBaselineBottom = c.minBBox.intersects(baselineBottomBand)
          (c.scaledFontId != primaryFontId
          && isLowered
          && intersectsBaselineBottom
          && fontIsBelowBaseline
          && !c.isLigature)
        }
      )

      import utils.intervals._

      def beginLenInterval(b: Int, l: Int, label: Label): Interval[Int, Label] = {
        Interval.bounded.create.leftClosedRightOpen(b, b + l).withAttr[Label](label)
      }

      val init = (0, List[Interval[Int, Label]]())

      val (_, superScriptSpansRev) = eitherSuperScriptOrNot.foldLeft(init) {
        case ((offset, spans), e) =>
          e match {
            case Right(v) =>
              val sp        = beginLenInterval(offset, v.length, LB.Sup) :: spans
              val newOffset = offset + v.length
              (newOffset, sp)
            case Left(v) =>
              val newOffset = offset + v.length
              (newOffset, spans)
          }
      }

      val superScriptSpans = superScriptSpansRev.reverse

      val (_, subScriptSpansRev) = eitherSubScriptOrNot.foldLeft(init) {
        case ((offset, spans), e) =>
          e match {
            case Right(v) =>
              val sp        = beginLenInterval(offset, v.length, LB.Sub) :: spans
              val newOffset = offset + v.length
              (newOffset, sp)
            case Left(v) =>
              val newOffset = offset + v.length
              (newOffset, spans)
          }
      }
      val subScriptSpans = subScriptSpansRev.reverse

      val scriptSpans =
        (subScriptSpans ++ superScriptSpans).sorted(Interval.defaultIntervalOrdering[Int, Label]())
      if (scriptSpans.nonEmpty) {
        midriseBaselineBand.setAttr(LabeledIntervals)(scriptSpans)
      }
      val subScriptChars = eitherSubScriptOrNot.collect { case Right(i) => i }

      subScriptChars.foreach { charSpan =>
        val minBounds = charSpan.map(_.minBBox).reduce(_ union _)
        traceLog.trace {
          traceLog.figure(minBounds) tagged s"SubScript MinBounds"
        }
      }
    }

  }

}
