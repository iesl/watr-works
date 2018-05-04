package edu.umass.cs.iesl.watr
package segment

import textgrid._
// import watrmarks._
import geometry._
import utils.ExactFloats._
import utils.QuickNearestNeighbors._
import extract.ExtractedItem
import TypeTags._


trait TextReconstruction extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val textReconstruction = self
  // Quick and dirty TextGrid construction
  def getTextGrid(): TextGrid = {
    val textLineReprShape = LB.CapDescenderBand
    val rows1 = getLabeledShapes(textLineReprShape)
      .flatMap { reprShape =>
        val shapeChars = getCharsForShape(reprShape).map(_.id.unwrap)
        if (shapeChars.nonEmpty) {
          val minId  = shapeChars.min

          val textRow = insertSpacesInRow(
            textRowFromReprShape(reprShape)
          )
          Some((minId, textRow))
        } else None
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

  def constructFinalTextGrid(): TextGrid = {
    /**
      *
      * AsideText
      * BodyText
      *   BlockRepr: LB.TextBlock
      *     LineRepr: CapDescenderBand/region + ordered glyphs
      *   InsetRepr: InsetMaths|Table|Image
      *
      **/


    ???
  }



  private def textRowFromReprShape(reprShape: AnyShape): TextGrid.Row = {

    val extractedItems = getExtractedItemsForShape(reprShape).sortBy(_.minBBox.left)

    new TextGrid.MutableRow { self =>
      val init = extractedItems.map{
        case item: ExtractedItem.CharItem =>
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

      TextGrid.Row.fromCells(lastCursor.toList)

    } getOrElse { textRow }
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
