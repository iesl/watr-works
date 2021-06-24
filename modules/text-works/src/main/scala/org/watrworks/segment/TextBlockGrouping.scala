package org.watrworks
package segment

import watrmarks._
import geometry.syntax._
import utils.ExactFloats._
import utils.FunctionalHelpers._
import utils.SlicingAndDicing._

sealed trait TextStructure[A]

object TextStructure {
  case class TextLine(repr: AnyShape) extends TextStructure[Nothing]
}


trait TextBlockGrouping extends BasePageSegmenter { self =>
  lazy val textBlockGrouping = self


  def findContiguousBlocks(label: Label): Unit = {

    val fontsByMostOccuring = getFontsSortedByHighestOccurrenceCount()

    val sortedLines = getLabeledShapes(label).sortBy { lineShape =>
      getCharsForShape(lineShape).head.id.unwrap
    }

    def groupEverything(
      scaledFontIds: List[String@@ScaledFontID],
      lineShapes: Seq[AnyShape],
      depth: Int = 0
    ): Seq[Seq[AnyShape]] = scaledFontIds match {
      case headFontId :: tailFontIds =>
        val markedLineSpans = collectSpanEither[AnyShape](lineShapes, { lineShape =>
          lineShape.getAttr(Fonts).exists(_.contains(headFontId))
        })

        traceLog.traceAll {
          markedLineSpans.map { _ match {
            case Right(lines) =>
              val groupBounds = lines.map(_.shape.minBounds).reduce(_ union _)
              figure(groupBounds) tagged s"Shared Font#${depth}. ${headFontId}"

            case Left(lines)  =>
              val groupBounds = lines.map(_.shape.minBounds).reduce(_ union _)
              figure(groupBounds) tagged s"Excluded From Font#${depth}. ${headFontId}"
          }}
        }

        markedLineSpans.flatMap{ _ match {
          case Right(lines) => doLineGrouping(lines)
          case Left(lines)  => groupEverything(tailFontIds, lines, depth+1)
        }}

      case Nil => lineShapes.map(List(_))
    }

    groupEverything(fontsByMostOccuring.toList, sortedLines)

  }

  private def doLineGrouping(sortedLines: Seq[AnyShape]): Seq[Seq[AnyShape]] = {
    val lineGroups = sortedLines.groupByWindow { case (prevs, currLine) =>

      val lastLine = prevs.last

      val lastLineItems = getCharsForShape(lastLine)
      val currLineItems = getCharsForShape(currLine)

      lazy val currLineText = currLineItems.map(_.char).mkString

      val item1 = lastLineItems.last
      val item2 = currLineItems.head
      val line1EndId = item1.id.unwrap
      val line2StartId = item2.id.unwrap
      val consecutive = line2StartId == line1EndId + 1

      lazy val inOrderTopToBottom = item1.minBBox.bottom < item2.minBBox.bottom

      lazy val prevWindowBounds = prevs.map(_.shape.minBounds).reduce(_ union _)

      lazy val combinedWindowBounds = prevWindowBounds union currLine.shape.minBounds

      traceLog.trace {
        figure(combinedWindowBounds) tagged s"Window Bounds"
      }

      lazy val expansionBounds = lastLine.shape.minBounds union currLine.shape.minBounds

      lazy val noLeftOverlaps = prevWindowBounds.withinRegion(combinedWindowBounds)
        .adjacentRegion(M3.Left)
        .map{ adjacentRegion =>
          val shavedRegion = adjacentRegion
            .shave(M3.Right, FloatExact.epsilon)
            .shave(M3.Top, FloatExact.epsilon)
            .shave(M3.Bottom, FloatExact.epsilon)
          hasNoOverlaps(shavedRegion)
        }
        .getOrElse(true)

      lazy val noRightOverlaps = prevWindowBounds.withinRegion(combinedWindowBounds)
        .adjacentRegion(M3.Right)
        .map{ adjacentRegion =>
          val shavedRegion = adjacentRegion
            .shave(M3.Left, FloatExact.epsilon)
            .shave(M3.Top, FloatExact.epsilon)
            .shave(M3.Bottom, FloatExact.epsilon)

          hasNoOverlaps(shavedRegion)
        }
        .getOrElse(true)

      lazy val noLateralOverlaps = noLeftOverlaps && noRightOverlaps

      lazy val glyphAndLineCountsMatch  = expansionBounds.withinRegion(combinedWindowBounds)
        .adjacentRegions(M3.Left, M3.Center, M3.Right)
        .map { expansionRect =>

          val queryRect = expansionRect.shave(M3.Top, FloatExact.epsilon * 5)

          val foundGlyphs: Seq[RectShape] = searchForRects(queryRect, LB.Glyph)

          val glyphCountsMatch = currLineItems.length + lastLineItems.length == foundGlyphs.length

          val noNonTextOverlaps = hasNoNonTextOverlaps(queryRect)

          noNonTextOverlaps && (glyphCountsMatch || {

            lazy val foundItemIds = foundGlyphs.flatMap{ g =>
              g.getAttr(ExtractedChars).getOrElse(Nil).map(_.id.unwrap)
            }

            lazy val lastLineIds = lastLineItems.map(_.id.unwrap)
            lazy val commonIds = foundItemIds.intersect(lastLineIds)
            lazy val adjustedFoundGlyphCount = foundGlyphs.length - commonIds.length

            traceLog.trace {
              val commonItems = lastLineItems.filter(item => commonIds.contains(item.id.unwrap))
              val commonItemBounds = commonItems.map(_.minBBox)
              figures(commonItemBounds) tagged s"Common Glyphs ${currLineText}"
            }

            currLineItems.length == adjustedFoundGlyphCount
          })

        } getOrElse { false }

      consecutive && inOrderTopToBottom && glyphAndLineCountsMatch && noLateralOverlaps
    }

    traceLog.traceAll {
      lineGroups.flatMap { group =>
        val groupBounds = group.map(_.shape.minBounds).reduce(_ union _)
        val groupBoundsShape = initShape(groupBounds, LB.TextLineGroup)
        List(
          shape(groupBoundsShape) tagged "Grouped Text Blocks",
          // relation("TextLineGroups").field(groupBoundsShape).field(group)
        )
      }
    }

    lineGroups

  }
}
