package edu.umass.cs.iesl.watr
package segment

import textgrid._
import textgraph._
  // import watrmarks._
import geometry._
import geometry.syntax._
import utils.ExactFloats._
import utils.QuickNearestNeighbors._
import extract.ExtractedItem
import TypeTags._
import scalaz.{@@ => _, _} //, Scalaz._


trait TextReconstruction extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val textReconstruction = self

  def getTextGraph(): TextGraphJvm = {
    val textGraph = TextGraphJvm.create(docScope.stableId)

    val textLineReprShape = LB.CapDescenderBand
    val rows1 = getLabeledShapes(textLineReprShape)
      .flatMap { reprShape =>
        val shapeChars = getCharsForShape(reprShape).map(_.id.unwrap)
        if (shapeChars.nonEmpty) {
          val minId  = shapeChars.min

          val textRow = insertSpacesInGraphRow(
            textGraphRowFromReprShape(reprShape)
          )
          Some((minId, textRow))
        } else None
      }


    val rows = rows1.sortBy(_._1).map(_._2)

    rows.foreach { row =>
      textGraph.appendRow(row)
    }

    textGraph
  }





  private def textGraphRowFromReprShape(reprShape: AnyShape): Seq[TextGraph.GridCell] = {

    val extractedItems = getExtractedItemsForShape(reprShape).sortBy(_.minBBox.left)

    extractedItems.flatMap{
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
          val cell = TextGraph.GlyphCell(char, charAtom, Seq())

          // val continuations = item.char.tail.map { cn =>
          //   cell.createInsert(cn)
          // }
          // val allCells: Seq[TextGrid.GridCell] = cell +: continuations
          cell
        }
        cells

      case item =>
        // TODO this is skipping over text represented as paths (but I have to figure out sup/sub script handling to make it work)
        Seq()
    }
  }


  import utils.SlicingAndDicing._
  import scalaz.syntax.std.list._

  private def insertSpacesInGraphRow(cells: Seq[TextGraph.GridCell]): Seq[TextGraph.GridCell] =  {

    val glyphCells: Seq[TextGraph.GlyphCell] = cells.collect{
      case c: TextGraph.GlyphCell => c
    }

    val splitValue = guessWordbreakWhitespaceThreshold(glyphCells.map(_.headItem))

    val wordGroups: Seq[Seq[TextGraph.GridCell]] = glyphCells.groupByPairs ((e1, e2) => {
      val pairwiseDist = e2.pageRegion.bbox.left - e1.pageRegion.bbox.right
      pairwiseDist < splitValue
    })

    wordGroups.toList.intersperse(List(TextGraph.InsertCell(' '))).flatten.toList
  }



  private def guessWordbreakWhitespaceThreshold(sortedLineCCs: Seq[PageItem]): FloatExact =  {
    val charSpacings = qnn(
      pairwiseItemDistances(sortedLineCCs), 0.5d
    )

    if (charSpacings.length == 1) {
      val charWidths = sortedLineCCs.map(_.bbox.width)
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

  private def insertSpacesInRow(textRow: TextGrid.Row): TextGrid.Row =  {

    val lineCCs = textRow.cells.collect{
      case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
        headItem
    }

    val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

    textRow.toCursor().map{ cursor =>

      val lastCursor = cursor.unfold { nextCursor =>

        val wordWin = nextCursor.toWindow.slurpRight{ case (win, cell) =>
          val pairwiseDist = cell.pageRegion.bbox.left - win.last.pageRegion.bbox.right
          val willGroup = pairwiseDist < splitValue
          willGroup
        }

        if (!wordWin.atEnd) {
          val space = wordWin.cells.last.createInsert(' ')
          wordWin.extendRight(space).closeWindow.some
        } else None
      }
      val groupedWhitespace = lastCursor.toList().groupByPairs{ case (cell1, cell2) =>
        cell1.char == ' ' && cell2.char == ' '
      }
      val hasDuplicateWS = groupedWhitespace.exists(_.length > 1)
      if (hasDuplicateWS) {
        println(s"duplicate whitespace detected: ")
      }

      val dedupedWhitespace = groupedWhitespace.map(_.head)


      TextGrid.Row.fromCells(dedupedWhitespace)

    } getOrElse { textRow }
  }

  def getTextGrid(maybeClipTo: Option[LTBounds]): TextGrid = {
    val textLineReprShape = LB.CapDescenderBand
    val clipRegion = maybeClipTo.getOrElse{ pageGeometry }
    val lines = searchForRects(clipRegion, textLineReprShape)

    val rows1 = lines.flatMap { reprShape =>
      // val shapeChars = getCharsForShape(reprShape)
      val shapeChars = getCharsForShape(reprShape).map(_.id.unwrap)
      // shapeChars.filter

      if (shapeChars.nonEmpty) {
        val minId  = shapeChars.min

        val textRow = insertSpacesInRow(
          textRowFromReprShape(reprShape, maybeClipTo)
        )
        Some((minId, textRow))
      } else None
    }

    val rows = rows1.sortBy(_._1).map(_._2)

    TextGrid.fromRows(docScope.stableId,  rows)
  }

}
