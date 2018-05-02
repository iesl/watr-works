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

  def getTextGrid(): TextGrid = {

    val textLineReprShape = LB.CapDescenderBand

    val rows1 = getLabeledShapes(textLineReprShape)
      .flatMap { baselineShape =>
        val shapeChars = getCharsForShape(baselineShape).map(_.id.unwrap)
        if (shapeChars.nonEmpty) {
          val minId  = shapeChars.min
          val textRow = createTextRowFromVisualLine(baselineShape)
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


  protected def createTextRowFromVisualLine(visualBaseline: AnyShape): TextGrid.Row = {
    // println(s"createTextRowFromVisualLine")
    val textRow = textRowFromComponents(visualBaseline)

    val row = insertSpacesInRow(textRow)
    row
  }

  private def textRowFromComponents(visualBaseline: AnyShape): TextGrid.Row = {

    val extractedItems = getExtractedItemsForShape(visualBaseline).sortBy(_.minBBox.left)

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

      val res = textRow.toCursor().map{ cursor =>
        // println(s"insertSpacesInRow: focus=${cursor.focus.char}")

        val finalRow = cursor.unfoldCursorToRow { nextCursor =>
          // println(s"             : nextCursor=${nextCursor.focus.char}")

          val wordWin = nextCursor.toWindow.slurpRight{ case (win, cell) =>

            // val winStr = win.map(_.char).mkString
            // val c = cell.char

            val pairwiseDist = cell.pageRegion.bbox.left - win.last.pageRegion.bbox.right
            val willGroup = pairwiseDist < splitValue
            // println(s"             + win=[${winStr}] + ${c}  willGroup=${willGroup}")


            willGroup
          }

          if (!wordWin.atEnd) {
            wordWin.extendRight(' ').closeWindow.some
          } else None

        }

        // println(s"             : unfold complete")


        finalRow

      } getOrElse { textRow }

    res
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
