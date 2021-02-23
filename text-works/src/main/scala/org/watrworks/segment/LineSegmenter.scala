package org.watrworks
package segment

import scala.{collection => sc}
import sc.Seq

import geometry._
import geometry.syntax._
import extract._
import utils.ExactFloats._
import watrmarks._
import textboxing.{TextBoxing => TB}, TB._
import utils.SlicingAndDicing._

import TypeTags._

trait LineSegmentation
  extends PageScopeSegmenter
  with LineLayout
  with FontAndGlyphMetrics
  with TextBlockGrouping { self =>

  import SegmentationSystem._

  def findTextLineShapesFromFontBaselines(): Unit = {
    joinFontBaselinesViaPageBands(LB.CharRunFontBaseline, LB.BaselineMidriseBand)
    reindexShapes(LB.Glyph)
    segmentSuperSubScripts()
  }

  private def getBaselineMidrisePageSlice(fontOffsets: FontBaselineOffsets): Option[LTBounds] = {
    fontOffsets.sliceBetween(
      _.baseLine,
      _.midriseLine,
      pageGeometry
    )
  }

  private def getAscentDescentPageSlice(fontOffsets: FontBaselineOffsets): Option[LTBounds] = {
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
          getFontsForShape(lineShape).contains(headFontId)
        }

        val allAdjustedOffsets = linesForFont.map { lineShape =>
          val line = lineShape.shape
          val fontOffsets = docScope.fontDefs.getScaledFontOffsets(headFontId)
          (lineShape, fontOffsets.forFontBoxBottom(line.p1.y))
        }

        val linesAndOffsetsAndHeadChar = allAdjustedOffsets.map { case (lineShape, offsetsAtLine) =>
          val lineChars = getCharsForShape(lineShape)
          (lineShape, offsetsAtLine, lineChars.head)
        }

        linesAndOffsetsAndHeadChar.foreach { case (lineShape, offsetsAtLine, headChar) =>
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
    pageSlice: LTBounds,
    rootChar: ExtractedItem.CharItem,
    outputLabel: Label
  ): Option[RectShape] = {
    val glyphsInBand = searchForRects(pageSlice, LB.Glyph)

    val glyphsWithChar = glyphsInBand.map { g =>
      (g, getCharsForShape(g).head)
    }

    val orderedById = glyphsWithChar.sortBy { case (glyphShape, charItem) =>
      charItem.id
    }

    val consecutiveById = orderedById.groupByPairs { case ((_, char1), (_, char2)) =>
      itemsAreConsecutive(char1, char2)
    }

    val indexedSets = consecutiveById.zipWithIndex

    val setWithRootChar = indexedSets.filter { case (charSet, setNum) =>
      charSet.map(_._2.id).contains(rootChar.id)
    }

    if (setWithRootChar.nonEmpty) {
      val setNum = setWithRootChar.head._2
      val charSetWithRootChar = setWithRootChar.flatMap(_._1.map(_._2))
      val orderedLeftToRight = charSetWithRootChar.sortBy { _.minBBox.left }
      val sameOrderByIdAndByLeftToRight = charSetWithRootChar
        .zip(orderedLeftToRight)
        .forall { case (char1, char2) => char1 == char2 }

      val allItemsAreConsecutive = consecutiveById.length == 1

      // println(s"ord.by.id  : ${orderedById.map(_._2.char).mkString}")
      // println(s" (w/root)  : ${charSetWithRootChar.map(_.char).mkString}")
      // println(s"ord.l->r   : ${orderedLeftToRight.map(_.char).mkString}")

      if (sameOrderByIdAndByLeftToRight || allItemsAreConsecutive) {
        val rootFontOffsets = docScope.fontDefs
          .getScaledFontOffsets(rootChar.scaledFontId)
          .forFontBoxBottom(rootChar.fontBbox.bottom)

        val fontIds = charSetWithRootChar.map(_.scaledFontId).toSet
        val charBounds = charSetWithRootChar.map(_.minBBox).reduce(_ union _)
        val charSetText = charSetWithRootChar.map(_.char).mkString

        rootFontOffsets
          .sliceBetween(_.baseLine, _.midriseLine, pageGeometry)
          .flatMap { baselineMidriseSlice =>
            val baselineMidriseRect = clipRectBetween(
              charBounds.left,
              charBounds.right,
              baselineMidriseSlice
            )

            setWithRootChar.foreach { case (charSet, setNum) =>
              charSet.foreach { case (glyphShape, charItem) =>
                unindexShape(glyphShape)
              }
            }

            baselineMidriseRect.map { baselineMidrise =>
              val pageBand = indexShape(baselineMidrise, outputLabel).asRectShape

              traceLog.traceAll {

                val allFontIds = charSetWithRootChar.map(_.scaledFontId)
                val fontSpansOverChars = allFontIds
                  .map(_.unwrap)
                  .groupByPairs(_ == _)
                  .map { fontIds =>
                    (fontIds.head, fontIds.length)
                  }
                  .foldLeft((List[(String, Int, Int)](), 0)) {
                    case ((accList, accBegin), (fontId, spanLen)) =>
                      ((fontId, accBegin, spanLen) :: accList, accBegin + spanLen)
                  }

                val fontRanges = fontSpansOverChars._1.reverse

                // val rel = relation("TextLineFontRanges")
                //   .field(pageBand)
                //   .col("FontRanges", fontRanges)

                // List(shape(pageBand), rel)
                List(shape(pageBand) )
              }

              setExtractedItemsForShape(pageBand, charSetWithRootChar)
              setFontsForShape(pageBand, fontIds)
              setPrimaryFontForShape(pageBand, rootChar.scaledFontId)
              setFontOffsetsForShape(pageBand, rootFontOffsets)
              pageBand
            }
          }
      } else None
    } else None
  }

}
