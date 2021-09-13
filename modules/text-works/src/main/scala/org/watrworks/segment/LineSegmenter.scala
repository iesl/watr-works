package org.watrworks
package segment

import geometry._
import geometry.syntax._
import watrmarks._
import TypeTags._

trait LineSegmentation
  extends BasePageSegmenter
  with FontAndGlyphMetrics { self =>

  def findTypographicUnits(): Unit = {
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
      lineChars    = lineShape.getAttr(ExtractedChars).getOrElse(Nil)
      charMinX     = lineChars.minBy(_.minBBox.left).minBBox.left
      charMaxX     = lineChars.maxBy(_.minBBox.right).minBBox.right

      ascentDescentBand <- fontOffsets.sliceBetween(_.ascentLine, _.descentLine, pageGeometry)
      ascentDescentRect <- ascentDescentBand.clipLeftRight(charMinX, charMaxX)

      baselineMidriseBand <- fontOffsets.sliceBetween(_.baseLine, _.midriseLine, pageGeometry)
      baselineMidriseRect <- baselineMidriseBand.clipLeftRight(charMinX, charMaxX)

    } {

      def indexAndInit(rect: Rect, label: Label): RectShape = {
        traceLog.startTask(s"Init/${label.fqn}")
        val shape = indexShape(rect, label)
        shape.setAttr(ExtractedChars)(lineChars)
        shape.setAttr(Fonts)(Set(fontId))
        shape.setAttr(PrimaryFont)(fontId)
        shape.setAttr(FontOffsets)(fontOffsets)

        traceLog.trace {
          createLabelOn(label.fqn, rect)
            .withProp("Font", fontId.toString())
            .withProp("Chars", lineChars.map(_.char).mkString)

        }
        traceLog.endTask()
        shape
      }

      indexAndInit(ascentDescentRect, LB.AscentDescentBand)
      indexAndInit(baselineMidriseRect, LB.BaselineMidriseBand)
    }

  }


}
