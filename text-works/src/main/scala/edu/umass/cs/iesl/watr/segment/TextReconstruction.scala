package edu.umass.cs.iesl.watr
package segment

import segment.{SegmentationLabels => LB}
import textgrid._
import watrmarks._
import geometry._
import utils.ExactFloats._
import utils.QuickNearestNeighbors._
import extract.ExtractedItem
import TypeTags._


trait TextReconstruction extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val textReconstruction = self

  /**
    *  Returns final reading-ordered text
    */
  def getTextGrid(): TextGrid = {

    val textLineReprShape = LB.CharRunFontBaseline

    val rows1 = getLabeledLines(textLineReprShape)
      .map { baselineShape =>
        val minId  = getCharsForShape(baselineShape).map(_.id.unwrap).min
        val textRow = createTextRowFromVisualLine(baselineShape)
        (minId, textRow)
      }


    // val rows2 = getLabeledLines(LB.SymbolicGlyphLine)
    //   .map { symbolicGlyphLine =>
    //     val textRow = createTextRowFromVisualLine(symbolicGlyphLine)
    //     val minId  = getCharsForShape(symbolicGlyphLine).map(_.id.unwrap).min
    //     (minId, textRow)
    //   }


    // val rows = (rows1 ++ rows2).sortBy(_._1).map(_._2)
    val rows = rows1.sortBy(_._1).map(_._2)


    TextGrid.fromRows(docScope.stableId,  rows)
  }


  protected def createTextRowFromVisualLine(visualBaseline: LineShape): TextGrid.Row = {

    val textRow = textRowFromComponents(visualBaseline)

    // Convert consecutive sup/sub to single label
    def relabel(c: GridCursor, l: Label): Unit = {
      c.findNext(_.hasLabel(l))
        .foreach{ cur =>
          val win = cur.slurpRight(_.hasLabel(l))
          win.removePins(l)
          win.addLabel(l)
          win.nextCursor().foreach(relabel(_, l))
        }
    }

    textRow.toCursor().foreach { c => relabel(c, LB.Sub) }
    textRow.toCursor().foreach { c => relabel(c, LB.Sup) }


    val spacedRow = insertSpacesInRow(textRow)

    // val supSubLabeledRow = spacedRow.toCursor()
    //   .map { cursor =>
    //     cursor.unfoldCursorToRow { c =>
    //       val focus = c.focus

    //       if      (focus.hasPin(LB.Sub.B))  { c.insertCharLeft ('₍').next }
    //       else if (focus.hasPin(LB.Sub.L))  { c.insertCharRight('₎').some }
    //       else if (focus.hasPin(LB.Sub.U))  { c.insertCharLeft('₍').next.get.insertCharRight('₎').some }
    //       else if (focus.hasPin(LB.Sup.B))  { c.insertCharLeft ('⁽').next }
    //       else if (focus.hasPin(LB.Sup.L))  { c.insertCharRight('⁾').some }
    //       else if (focus.hasPin(LB.Sup.U))  { c.insertCharLeft('⁽').next.get.insertCharRight('⁾').some }
    //       else { c.some }

    //     }
    //   }.getOrElse { spacedRow }

    // pageIndex.components.setComponentText(visualLineClusterCC, LB.VisualLine, supSubLabeledRow)
    // supSubLabeledRow
    spacedRow
  }

  private def textRowFromComponents(visualBaseline: LineShape): TextGrid.Row = {

    val extractedItems = getExtractedItemsForShape(visualBaseline).sortBy(_.minBBox.left)

    val (topIntersects, bottomIntersects) = findLineAtomScriptPositions(visualBaseline)

    val visualLineModalBounds: LTBounds = LTBounds.empty
    new TextGrid.MutableRow { self =>
      val init = extractedItems.map{
        case item: ExtractedItem.CharItem =>
          val intersectsTop = topIntersects.contains(item.id)
          val intersectsBottom = bottomIntersects.contains(item.id)

          val cells = item.char.headOption.map{ char =>
            val charAtom = CharAtom(
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

            if (item.minBBox.bottom == visualLineModalBounds.bottom) {
              // Center-text
            } else if (intersectsTop && !intersectsBottom) {
              allCells.foreach{ _.addLabel(LB.Sup) }
            } else if (!intersectsTop && intersectsBottom) {
              allCells.foreach{ _.addLabel(LB.Sub) }
            } else {
            }


            allCells
          }
          cells.getOrElse(Seq())

        case item =>
          // TODO this is skipping over text represented as paths (but I have to figure out sup/sub script handling to make it work)
          Seq()

      }

      cells.appendAll(init.flatten)
    }
  }

  private def insertSpacesInRow(textRow: TextGrid.Row): TextGrid.Row =  {

    val lineCCs = textRow.cells.collect{
      case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
        headItem
    }

    val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

    val res = textRow.toCursor().map{ cursor =>
      val finalRow = cursor.unfoldCursorToRow { nextCursor =>

        val wordWin = nextCursor.toWindow.slurpRight{ case (win, cell) =>

          val pairwiseDist = cell.pageRegion.bbox.left - win.last.pageRegion.bbox.right
          val willGroup = pairwiseDist < splitValue


          willGroup
        }

        if (!wordWin.atEnd) {
          wordWin.extendRight(' ').closeWindow.some
        } else None

      }

      finalRow

    } getOrElse { textRow }

    res
  }

  private def findLineAtomScriptPositions(visualBaseline: LineShape): (Seq[Int@@CharID], Seq[Int@@CharID]) = {
    val extractedItems = getExtractedItemsForShape(visualBaseline)
    // val mostCommonHeight = extractedItems.map(_.bbox.height).sorted
    //   .groupByPairs(_ == _)
    //   .head.head
    val (onBaseline, offBaseline) = extractedItems
      .partition { item =>
        item.location.y == visualBaseline.shape.p1.y
      }

    val (aboveBaseline, belowBaseline) = offBaseline
      .partition { item =>
        item.location.y < visualBaseline.shape.p1.y
      }


    (aboveBaseline.map(_.id), belowBaseline.map(_.id))
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


}
