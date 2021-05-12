package org.watrworks
package segment

import geometry._
import geometry.syntax._
import extract._
import utils.ExactFloats._
import watrmarks._

import utils.SlicingAndDicing._

import TypeTags._

object LineSegmentLabels {
  val Baseline       = Label.auto
  val SupScript      = Label.auto
  val SubScript      = Label.auto
  val StackedScript  = Label.auto
  val CombiningChars = Label.auto
  val MathsInline    = Label.auto
  val MathsCloud     = Label.auto
}

trait LineSegmentation
  extends BasePageSegmenter
  with LineLayout
  with FontAndGlyphMetrics
  with TextBlockGrouping { self =>

  def findTextLineShapesFromFontBaselines(): Unit = {
    joinFontBaselinesViaPageBands(LB.CharRunFontBaseline, LB.BaselineMidriseBand)
    reindexShapes(LB.Glyph)
    segmentSuperSubScripts()
  }

  private def getAscentDescentPageSlice(fontOffsets: FontBaselineOffsets): Option[Rect] = {
    fontOffsets.sliceBetween(
      _.ascentLine,
      _.descentLine,
      pageGeometry
    )
    val sliceHeight = fontOffsets.descentLine - fontOffsets.ascentLine
    pageHorizontalSlice(
      fontOffsets.ascentLine.asDouble(),
      sliceHeight.asDouble()
    )
  }

  private def joinFontBaselinesViaPageBands(startingShapeLabel: Label, outputLabel: Label): Unit = {

    def _loop(
      scaledFontIds: List[String @@ ScaledFontID],
      lineShapes: Seq[LineShape],
      depth: Int = 0
    ): Unit = scaledFontIds match {

      case headFontId :: tailFontIds =>
        val (linesForFont, others) = lineShapes.partition { lineShape =>
          lineShape.getAttr(Fonts).exists(_.contains(headFontId))
        }

        val allAdjustedOffsets = linesForFont.map { lineShape =>
          val line        = lineShape.shape
          val fontOffsets = docScope.fontDefs.getScaledFontOffsets(headFontId)
          (lineShape, fontOffsets.forFontBoxBottom(line.p1.y))
        }

        val linesAndOffsetsAndHeadChar = allAdjustedOffsets.map { case (lineShape, offsetsAtLine) =>
          val lineChars = getCharsForShape(lineShape)
          (lineShape, offsetsAtLine, lineChars.head)
        }

        linesAndOffsetsAndHeadChar.foreach { case (_, offsetsAtLine, headChar) =>
          getAscentDescentPageSlice(offsetsAtLine).foreach { slice =>
            findLineCharsInPageBand(slice, headChar, outputLabel)
          }
        }

        _loop(tailFontIds, others, depth + 1)

      case Nil =>
    }

    val startingLines = getLabeledLines(startingShapeLabel)

    val fontsByMostOccuring = getFontsSortedByHighestOccurrenceCount()

    _loop(fontsByMostOccuring.toList, startingLines)
  }

  private def findLineCharsInPageBand(
    pageSlice: Rect,
    rootChar: ExtractedItem.CharItem,
    outputLabel: Label
  ): Option[RectShape] = {
    val glyphsInBand = searchForRects(pageSlice, LB.Glyph)

    val glyphsWithChar = glyphsInBand.map { g =>
      // (g, getCharsForShape(g).head)
      (g, g.getAttr(ExtractedChar).get)
    }

    val orderedById = glyphsWithChar.sortBy { case (glyphShape @ _, charItem) =>
      charItem.id
    }

    val consecutiveById = orderedById.groupByPairs { case ((_, char1), (_, char2)) =>
      itemsAreConsecutive(char1, char2)
    }

    val indexedSets = consecutiveById.zipWithIndex

    val setWithRootChar = indexedSets.filter { case (charSet, setNum @ _) =>
      charSet.map(_._2.id).contains(rootChar.id)
    }

    if (setWithRootChar.nonEmpty) {
      val charSetWithRootChar = setWithRootChar.flatMap(_._1.map(_._2))
      val orderedLeftToRight  = charSetWithRootChar.sortBy { _.minBBox.left }
      val sameOrderByIdAndByLeftToRight = charSetWithRootChar
        .zip(orderedLeftToRight)
        .forall { case (char1, char2) => char1 == char2 }

      val allItemsAreConsecutive = consecutiveById.length == 1

      if (sameOrderByIdAndByLeftToRight || allItemsAreConsecutive) {
        val rootFontOffsets = docScope.fontDefs
          .getScaledFontOffsets(rootChar.scaledFontId)
          .forFontBoxBottom(rootChar.fontBbox.bottom)

        val fontIds    = charSetWithRootChar.map(_.scaledFontId).toSet
        val charBounds = charSetWithRootChar.map(_.minBBox).reduce(_ union _)

        rootFontOffsets
          .sliceBetween(_.baseLine, _.midriseLine, pageGeometry)
          .flatMap { baselineMidriseSlice =>
            val baselineMidriseRect = clipRectBetween(
              charBounds.left,
              charBounds.right,
              baselineMidriseSlice
            )

            setWithRootChar.foreach { case (charSet, setNum @ _) =>
              charSet.foreach { case (glyphShape, charItem @ _) =>
                unindexShape(glyphShape)
              }
            }

            baselineMidriseRect.map { baselineMidrise =>
              val pageBand = indexShape(baselineMidrise, outputLabel).asRectShape

              traceLog.trace {
                shape(pageBand)
              }

              pageBand.setAttr(ExtractedChars)(charSetWithRootChar)
              pageBand.setAttr(Fonts)(fontIds)
              pageBand.setAttr(PrimaryFont)(rootChar.scaledFontId)
              pageBand.setAttr(FontOffsets)(rootFontOffsets)

              pageBand
            }
          }
      } else None
    } else None
  }

}
