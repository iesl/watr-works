package org.watrworks
package segment

import geometry._
import geometry.syntax._
import extract._
import watrmarks._

import utils.SlicingAndDicing._

import TypeTags._

trait LineSegmentation
  extends BasePageSegmenter
  // with LineLayout
  with FontAndGlyphMetrics { self =>

  def findTextLineShapesFromFontBaselines(): Unit = {
    // joinFontBaselinesViaPageBands(LB.CharRunFontBaseline, LB.BaselineMidriseBand)
    // reindexShapes(LB.Glyph)
    // segmentSuperSubScripts()
    createCharRunBands(LB.CharRunFontBaseline)
  }

  protected def createCharRunBands(startingShapeLabel: Label): Unit = {
    val startingLines = getLabeledLines(startingShapeLabel)
    val sorted        = sortShapesByFontOccurrence(startingLines)

    for {
      (linesForFont, fontId) <- sorted
      lineShape              <- linesForFont
      lineY        = lineShape.asLineShape.shape.p1.y
      _fontOffsets = docScope.fontDefs.getScaledFontOffsets(fontId)
      fontOffsets  = _fontOffsets.forFontBoxBottom(lineY)
      lineChars    = getCharsForShape(lineShape)
      charMinX     = lineChars.minBy(_.minBBox.left).minBBox.left
      charMaxX     = lineChars.maxBy(_.minBBox.right).minBBox.right

      ascentDescentBand <- fontOffsets.sliceBetween(_.ascentLine, _.descentLine, pageGeometry)
      ascentDescentRect <- ascentDescentBand.clipLeftRight(charMinX, charMaxX)

      baselineMidriseBand <- fontOffsets.sliceBetween(_.baseLine, _.midriseLine, pageGeometry)
      baselineMidriseRect <- baselineMidriseBand.clipLeftRight(charMinX, charMaxX)

    } {

      def indexAndInit(rect: Rect, label: Label): RectShape = {
        val shape = indexShape(rect, label)
        shape.setAttr(ExtractedChars)(lineChars)
        shape.setAttr(Fonts)(Set(fontId))
        shape.setAttr(PrimaryFont)(fontId)
        shape.setAttr(FontOffsets)(fontOffsets)

        traceLog.trace {
          createLabelOn(label.fqn, rect)
            .withProp("Font", fontId.toString())
            .withProp("Chars", lineChars.map(_.char).mkString)

          createLabelOn(s"${label.fqn}ForFont${fontId}", rect)
            .withProp("Font", fontId.toString())
            .withProp("Chars", lineChars.map(_.char).mkString)
        }
        shape
      }

      indexAndInit(ascentDescentRect, LB.AscentDescentBand)
      indexAndInit(baselineMidriseRect, LB.BaselineMidriseBand)
    }

  }

  protected def joinFontBaselinesViaPageBands(
    startingShapeLabel: Label,
    outputLabel: Label
  ): Unit = {
    val startingLines = getLabeledLines(startingShapeLabel)
    val sorted        = sortShapesByFontOccurrence(startingLines)
    sorted.foreach({ case (linesForFont, fontId) =>
      val allAdjustedOffsets = linesForFont.map { lineShape =>
        val line        = lineShape.asLineShape.shape
        val fontOffsets = docScope.fontDefs.getScaledFontOffsets(fontId)
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
    })
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
            val baselineMidriseRect = baselineMidriseSlice.clipLeftRight(
              charBounds.left,
              charBounds.right
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

  protected def getAscentDescentPageSlice(
    fontOffsets: FontBaselineOffsets
  ): Option[Rect] = {
    pageGeometry.clipTopBottom(
      fontOffsets.ascentLine,
      fontOffsets.descentLine
    )
  }

}
