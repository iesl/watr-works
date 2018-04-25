package edu.umass.cs.iesl.watr
package segment

import watrmarks._
import geometry.syntax._
import segment.{SegmentationLabels => LB}
import utils.ExactFloats._
import utils.{RelativeDirection => Dir}
import utils.FunctionalHelpers._
import utils.SlicingAndDicing._

trait TextBlockGrouping extends PageScopeSegmenter { self =>
  lazy val textBlockGrouping = self


  protected def findContiguousBlocks(label: Label): Unit = {

    val fontsByMostOccuring = docScope.getFontsWithOccuranceCounts()
      .sortBy(_._2).reverse.map(_._1)

    val sortedLines = getLabeledLines(label).sortBy { lineShape =>
      getCharsForShape(lineShape).head.id.unwrap
    }

    def groupEverything(
      scaledFontIds: List[String@@ScaledFontID],
      lineShapes: Seq[LineShape],
      depth: Int = 0
    ): Seq[Seq[LineShape]] = scaledFontIds match {
      case headFontId :: tailFontIds =>
        val markedLineSpans = collectSpanEither[LineShape](lineShapes, { lineShape =>
          getFontsForShape(lineShape).contains(headFontId)
        })

        traceLog.traceAll {
          markedLineSpans.map { _ match {
            case Right(lines) =>
              val groupBounds = lines.map(_.shape.bounds()).reduce(_ union _)
              figure(groupBounds) tagged s"Shared Font#${depth}. ${headFontId}"

            case Left(lines)  =>
              val groupBounds = lines.map(_.shape.bounds()).reduce(_ union _)
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

  private def doLineGrouping(sortedLines: Seq[LineShape]): Seq[Seq[LineShape]] = {
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

      lazy val topToBottom = item1.minBBox.bottom < item2.minBBox.bottom
      lazy val inOrder = topToBottom

      lazy val prevWindowBounds = prevs.map(_.shape.bounds()).reduce(_ union _)

      lazy val combinedWindowBounds = prevWindowBounds union currLine.shape.bounds()

      traceLog.trace {
        figure(combinedWindowBounds) tagged s"Window Bounds ${currLineText} "
      }

      lazy val expansionBounds = lastLine.shape.bounds() union currLine.shape.bounds()

      lazy val noLeftOverlaps = prevWindowBounds.withinRegion(combinedWindowBounds)
        .adjacentRegion(Dir.Left)
        .map(hasNoOverlaps(_))
        .getOrElse(true)

      lazy val noRightOverlaps = prevWindowBounds.withinRegion(combinedWindowBounds)
        .adjacentRegion(Dir.Right)
        .map(hasNoOverlaps(_))
        .getOrElse(true)

      lazy val noLateralOverlaps = noLeftOverlaps && noRightOverlaps

      lazy val glyphAndLineCountsMatch = expansionBounds.withinRegion(combinedWindowBounds)
        .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
        .map{ expansionRect =>

          val queryRect = expansionRect.shave(Dir.Top, FloatExact.epsilon * 5)


          val foundGlyphs: Seq[RectShape] = searchForRects(queryRect, LB.Glyph)

          traceLog.trace {
            figure(queryRect) tagged s"Expansion Rect ${currLineText}"
          }

          traceLog.trace {
            shape(foundGlyphs:_*) tagged s"Expansion Glyphs ${currLineText}"
          }

          val glyphCountsMatch = currLineItems.length == foundGlyphs.length

          val noNonTextOverlaps = hasNoNonTextOverlaps(queryRect)

          noNonTextOverlaps && (glyphCountsMatch || {

            lazy val foundItemIds = foundGlyphs.flatMap{ g =>
              getExtractedItemsForShape(g).map(_.id.unwrap)
            }

            lazy val lastLineIds = lastLineItems.map(_.id.unwrap)
            lazy val commonIds = foundItemIds.intersect(lastLineIds)
            lazy val adjustedFoundGlyphCount = foundGlyphs.length - commonIds.length

            traceLog.trace {
              val commonItems = lastLineItems.filter(item => commonIds.contains(item.id.unwrap))
              val commonItemBounds = commonItems.map(_.minBBox)
              figure(commonItemBounds:_*) tagged s"Common Glyphs ${currLineText}"
            }

            currLineItems.length == adjustedFoundGlyphCount
          })

        } getOrElse { false }

      consecutive && inOrder && glyphAndLineCountsMatch && noLateralOverlaps
    }

    traceLog.trace {
      // val groupBounds = lineGroups.filter(_.length > 1)
      val groupBounds = lineGroups.map { group =>
        if (group.length==1) {
          group.head.shape
        } else {
          group.map(_.shape.bounds()).reduce(_ union _)
        }
      }

      figure(groupBounds:_*) tagged "Grouped Text Blocks"
    }

    lineGroups

  }
}
