package edu.umass.cs.iesl.watr
package segment


import watrmarks._
import geometry._
import extract.ExtractedItem
import segment.{SegmentationLabels => LB}
import textgrid._
import utils.ExactFloats._
import TypeTags._
// import utils.SlicingAndDicing._

trait LineSegmentation extends PageScopeSegmenter { self =>
  lazy val lineSegmenter = self

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

    val supSubLabeledRow = spacedRow.toCursor()
      .map { cursor =>
        cursor.unfoldCursorToRow { c =>
          val focus = c.focus

          if      (focus.hasPin(LB.Sub.B))  { c.insertCharLeft ('₍').next }
          else if (focus.hasPin(LB.Sub.L))  { c.insertCharRight('₎').some }
          else if (focus.hasPin(LB.Sub.U))  { c.insertCharLeft('₍').next.get.insertCharRight('₎').some }
          else if (focus.hasPin(LB.Sup.B))  { c.insertCharLeft ('⁽').next }
          else if (focus.hasPin(LB.Sup.L))  { c.insertCharRight('⁾').some }
          else if (focus.hasPin(LB.Sup.U))  { c.insertCharLeft('⁽').next.get.insertCharRight('⁾').some }
          else { c.some }

        }
      }.getOrElse { spacedRow }

    // pageIndex.components.setComponentText(visualLineClusterCC, LB.VisualLine, supSubLabeledRow)
    // supSubLabeledRow
    spacedRow
  }

  private def textRowFromComponents(visualBaseline: LineShape): TextGrid.Row = {
    // val visualLineModalCC = pageIndex.components.getRelation(visualLineClusterCC, LB.VisualLineModal).head
    // val visualLineCC = pageIndex.components.getRelation(visualLineClusterCC, LB.VisualLine).head

    val extractedItems = getExtractedItemsForShape(visualBaseline)

    val (topIntersects, bottomIntersects) = findLineAtomScriptPositions(visualBaseline)

    val visualLineModalBounds: LTBounds = LTBounds.empty
    // println("textRowFromComponents:")
    new TextGrid.MutableRow { self =>
      val init = extractedItems.map{
        case item: ExtractedItem.CharItem =>
          // print(s" ${item.strRepr()};")
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
                item.bbox
              ),
              item.char,
              item.wonkyCharCode
            )

            val cell = TextGrid.PageItemCell(charAtom, Seq(), char)

            val continuations = item.char.tail.map { cn =>
              cell.createRightExpansion(cn)
            }

            val allCells: Seq[TextGrid.GridCell] = cell +: continuations

            if (item.bbox.bottom == visualLineModalBounds.bottom) {
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

    val lineText = textRow.cells.collect{
      case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
        if (tailItems.nonEmpty) {
          val ts = tailItems.mkString(", ")
          s"[${char}::: ${ts}]"
        } else {
          char
        }
    }

    println(s"lineText =>${lineText.mkString}")

    val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

    val res = textRow.toCursor().map{ cursor =>
      val finalRow = cursor.unfoldCursorToRow { nextCursor =>

        val wordWin = nextCursor.toWindow.slurpRight{ case (win, cell) =>

          val pairwiseDist = cell.pageRegion.bbox.left - win.last.pageRegion.bbox.right
          val willGroup = pairwiseDist < splitValue

          willGroup
        }

        if (!wordWin.atEnd) {
          wordWin.extendRight(' ').toLastCursor.some
        } else None

      }

      finalRow

    } getOrElse { textRow }

    res
  }

  // import utils.SlicingAndDicing._
  // Group line atoms into center/sub/superscript bins
  private def findLineAtomScriptPositions(
    visualBaseline: LineShape
  ): (Seq[Int@@CharID], Seq[Int@@CharID]) = {
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

    //     val visualLineBounds = visualLineCC.bounds()

    //     // top 1/3  & bottom 1/3 ==> centered

    //     val slices = visualLineBounds.getHorizontalSlices(3)
    //     val Seq(topSlice, _, bottomSlice) = slices

    //     val topIntersections = pageIndex.components.searchOverlapping(topSlice, LB.PageAtomGrp)
    //     val bottomIntersections = pageIndex.components.searchOverlapping(bottomSlice, LB.PageAtomGrp)
    //     // val middleIntersections = pageIndex.components.searchOverlapping(middleSlice, LB.PageAtomGrp)

    //     val topIntersects = topIntersections.map(_.id)
    //     val bottomIntersects = bottomIntersections.map(_.id)


    (aboveBaseline.map(_.id), belowBaseline.map(_.id))
  }

  // private def findModalBoundingRect(): LTBounds = {
  //   // val visualLineBounds = visualLineCC.bounds()

  //   // val modalBaselineI = modalValue(visualLineAtoms, _.bounds().bottom.unwrap)
  //   //   .getOrElse(visualLineBounds.bottom.unwrap)

  //   // val modalBaseline = FloatRep(modalBaselineI)

  //   // val modalToplineI = modalValue(visualLineAtoms, _.bounds.top.unwrap)
  //   //   .getOrElse(visualLineBounds.top.unwrap)

  //   // val modalTopline = FloatRep(modalToplineI)

  //   // val height = modalBaseline-modalTopline

  //   // val visualLineModalBounds = if (height > 0) {
  //   //   visualLineBounds.copy(
  //   //     top=modalTopline, height=modalBaseline-modalTopline
  //   //   )
  //   // } else {
  //   //   visualLineBounds
  //   // }

  //   // // gifBuilder.indicate("Modal-base/top VisualLine", visualLineModalBounds)
  //   // visualLineModalBounds
  //   ???
  // }

  private def pairwiseItemDistances(sortedLineCCs: Seq[PageItem]): Seq[FloatExact] = {
    val cpairs = sortedLineCCs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => (c2.bbox.left - c1.bbox.right)
      case _  => 0d.toFloatExact()
    })

    dists :+ 0d.toFloatExact()
  }

  private def guessWordbreakWhitespaceThreshold(sortedLineCCs: Seq[PageItem]): FloatExact =  {
    val charSpacings = QuickNearestNeighbors.qnn(
      pairwiseItemDistances(sortedLineCCs), 0.5d
    )

    println("guessWordbreakWhitespaceThreshold: ")
    println(charSpacings.mkString("\n  ", "\n  ", "\n"))

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
