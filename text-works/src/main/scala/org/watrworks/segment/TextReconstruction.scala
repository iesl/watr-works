package org.watrworks
package segment

import scala.{ collection => sc }
import sc.Seq
import textgrid._
import geometry._
import geometry.syntax._
import utils.ExactFloats._
import utils.QuickNearestNeighbors._
import extract.ExtractedItem
import TypeTags._
import scalaz.{@@ => _, _} //, Scalaz._
import utils.intervals._
import watrmarks._
import utils.SlicingAndDicing._
import scalaz.syntax.std.list._



trait TextReconstruction extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val textReconstruction = self

  private def guessWordbreakWhitespaceThreshold(sortedLineCCs: Seq[PageItem]): FloatExact =  {
    if (sortedLineCCs.isEmpty) 0d.toFloatExact() else {

      val charSpacings = qnn(
        pairwiseItemDistances(sortedLineCCs), 0.5d
      )

      if (charSpacings.length == 1) {
        val charWidths = sortedLineCCs.map(_.bbox.width)
        if (charWidths.isEmpty) {
          println(s"sortedLineCCs: ${sortedLineCCs}")
          println(s"charSpacings: ${charSpacings}")
        }
        charWidths.max
      } else if (charSpacings.length > 1) {
        val mostCommonSpacingBin = charSpacings.head
        val mostCommonSpacing = mostCommonSpacingBin.maxValue
        val largerSpacings = charSpacings.filter(b => b.centroid.value > mostCommonSpacing*2)
        if (largerSpacings.nonEmpty) {
          val nextCommonSpacing = largerSpacings.head.centroid.value
            (mostCommonSpacing + nextCommonSpacing) / 2
        } else {
          mostCommonSpacing + 1.0
        }
      } else {
        0.toFloatExact()
      }
    }
  }



  private def clipTextRow(textRow: TextGrid.Row, clipTo: LTBounds): TextGrid.Row = {
    val filtered = textRow.cells().filter { cell =>
      cell.pageRegion.bbox.intersects(clipTo)
    }
    TextGrid.Row.fromCells(filtered)
  }
  private def textRowFromReprShape(reprShape: AnyShape, maybeClipTo: Option[LTBounds]): TextGrid.Row = {

    val items = getExtractedItemsForShape(reprShape).sortBy(_.minBBox.left)

    val extractedItems =  maybeClipTo.map{ clipTo =>
      items.filter { _.minBBox.intersects(clipTo) }
    }.getOrElse(items)

    new TextGrid.MutableRow { self =>
      val init = extractedItems.map{
        case item: ExtractedItem.CharItem =>
          val cells = item.char.headOption.map{ char =>
            val charAtom = PageItem.CharAtom(
              item.id,
              PageRegion(
                StablePage(
                  docScope.stableId,
                  pageNum
                ),
                item.minBBox
              ),
              item.char
            )
            val cell = TextGrid.PageItemCell(charAtom, Seq(), char)

            val continuations = item.char.tail.map { cn =>
              cell.createInsert(cn)
            }

            val allCells: Seq[TextGrid.GridCell] = cell +: continuations

            allCells
          }
          cells.getOrElse(Seq())

        case item =>
          // TODO this is skipping over text represented as paths (but I have to figure out sup/sub script handling to make it work)
          Seq()
      }

      labelTargets.appendAll(init.flatten)
    }
  }


  private def insertEscapeCodesInRow(textRow: TextGrid.Row, labeledIntervals: Seq[Interval[Int, Label]]):  Unit =  {
    val cells = textRow.cells()
    labeledIntervals.foreach { interval =>
      val label = interval.attr
      val start = interval.start.get
      val end = interval.end.get - 1
      label match {
        case LB.Sub =>
          cells(start).prepend('{')
          cells(start).prepend('_')
          cells(end).append('}')
        case LB.Sup =>
          cells(start).prepend('{')
          cells(start).prepend('^')
          cells(end).append('}')

        case _ =>
      }
    }

  }

  private def insertSpacesInRow(textRow: TextGrid.Row): Unit =  {

    val lineCCs = textRow.cells.collect{
      case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
        headItem
    }

    val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

    val wordGroups = textRow.cells().groupByPairs { case (cell1, cell2) =>
      val pairwiseDist = cell2.pageRegion.bbox.left - cell1.pageRegion.bbox.right
      pairwiseDist < splitValue
    }

    wordGroups.drop(1).foreach { group =>
      group.head.prepend(' ')
    }

    //   val groupedWhitespace = lastCursor.toList().groupByPairs{ case (cell1, cell2) =>
    //     cell1.char == ' ' && cell2.char == ' '
    //   }
    //   val hasDuplicateWS = groupedWhitespace.exists(_.length > 1)
    //   val dedupedWhitespace = groupedWhitespace.map(_.head)
    //   TextGrid.Row.fromCells(dedupedWhitespace)

  }

  val textLineReprShape = LB.BaselineMidriseBand

  def setTextForReprShapes(): Unit = {
    val lineReprShapes = getLabeledRects(textLineReprShape)
    lineReprShapes.foreach { reprShape =>
      val shapeChars = getCharsForShape(reprShape).map(_.id.unwrap)

      if (shapeChars.nonEmpty) {
        val minId  = shapeChars.min

        val textRow = textRowFromReprShape(reprShape, None)
        insertSpacesInRow(textRow)
        setTextForShape(reprShape, textRow)
      }
    }
  }

  def getTextGrid(maybeClipTo: Option[LTBounds]): TextGrid = {
    val clipRegion = maybeClipTo.getOrElse{ pageGeometry }
    val lines = searchForRects(clipRegion, textLineReprShape)

    val rows1 = lines.flatMap { reprShape =>
      val shapeChars = getCharsForShape(reprShape).map(_.id.unwrap)

      if (shapeChars.nonEmpty) {
        val minId  = shapeChars.min

        val textRow = textRowFromReprShape(reprShape, maybeClipTo)


        getLabeledIntervalsForShape(reprShape).map{ labeledIntervals =>
          insertEscapeCodesInRow(textRow, labeledIntervals)
        }
        insertSpacesInRow(textRow)

        val clippedRow =  maybeClipTo.map{ clipTo =>
          clipTextRow(textRow, clipTo)
        }.getOrElse(textRow)

        Some((minId, textRow))
      } else None
    }

    val rows = rows1.sortBy(_._1).map(_._2)

    val rows2 = rows.map(_.expand())

    TextGrid.fromRows(docScope.stableId,  rows2)
  }

}
