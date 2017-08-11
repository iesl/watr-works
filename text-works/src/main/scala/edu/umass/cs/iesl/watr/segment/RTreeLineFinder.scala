package edu.umass.cs.iesl.watr
package segment

import spindex._

import watrmarks.{StandardLabels => LB, _}

import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._

import textreflow.data._
import scala.concurrent.duration._
import textgrid._
import textboxing.{TextBoxing => TB}, TB._

class LineFinder(
  mpageIndex: MultiPageIndex,
  pageId: Int@@PageID,
  pageNum: Int@@PageNum
) extends PageSegmenter(pageId, pageNum, mpageIndex) {


  def determineLines(): Unit = {
    val components = mpageIndex.getPageAtoms(pageNum)

    def stdLabels(lls: Label*) = List(
      LB.Image, LB.HLinePath, LB.VLinePath, LB.LinePath, LB.WhitespaceCol, LB.ReadingBlock, LB.VisualLineModal
    ) ++ lls.toList

    approximateLineBins(components)

    vis.writeRTreeImage("01-lineHashing", LB.LineByHash, stdLabels():_*)

    splitLinesWithOverlaps()

    vis.writeRTreeImage("02-splitLineHashing", LB.LineByHash, stdLabels():_*)

    findCandidateWhitespaceCols(components)

    vis.writeRTreeImage("03-colCandidates", LB.LineByHash, stdLabels(LB.LeftAlignedCharCol, LB.WhitespaceColCandidate):_*)

    combineCandidateWhitespaceCols()

    vis.writeRTreeImage("04-colsCombined", LB.LineByHash, stdLabels():_*)

    splitLinesOnWhitespaceColumns()

    vis.writeRTreeImage("05-LinesSplitByCols", LB.LineByHash, stdLabels():_*)

    val orderedRegions = findReadingOrder(pageGeometry)

    // vis.writeRTreeImage("06-ReadingOrder", LB.LineByHash, stdLabels():_*)

    // TODO extract path objects in groups, rather than singly, to avoid trying to rewrite large drawn shapes
    rewritePathObjects(orderedRegions)

    findVisualLines(orderedRegions)

    vis.writeRTreeImage("06-VisualLines", LB.VisualLine, stdLabels(LB.PageAtom):_*)

    // reorder visual lines within and across reading blocks
    import scala.collection.mutable

    val alreadySeen: mutable.Set[Int@@ComponentID] = mutable.Set.empty
    val lines: mutable.ArrayBuffer[Component] = mutable.ArrayBuffer.empty

    for {
      regionBounds <- orderedRegions
      vlineRoots = pageIndex.rtreeSearchHasAllLabels(regionBounds, LB.VisualLine, LB.Canonical) // TODO this search picks up some lines multiple times
      line <- vlineRoots.sortBy(_.bounds.bottom)
    }  {
      if (!alreadySeen.contains(line.id)) {
        alreadySeen.add(line.id)
        lines.append(line)
      }
    }

    pageIndex.addCluster(LB.ReadingOrder, lines)
  }


  def findVisualLines(orderedTextBlocks: Seq[LTBounds]): Unit = {
    val gifBuilder = vis.gifBuilder("findVisualLines", 500.millis)
    pageIndex.getComponentsWithLabel(LB.PageAtom)
      .foreach{ a => pageIndex.addLabel(a, LB.PageAtomTmp) }
    gifBuilder.addFrame("Starting", LB.PageAtomTmp)
    for { textBlock <- orderedTextBlocks } {
      gifBuilder.indicate("Examining Block", textBlock, LB.PageAtomTmp)
      val sortedHashedLinesWithChars = (for {
        hashedLineCC <- pageIndex.rtreeSearch(textBlock, LB.LineByHash)
      } yield {
        val charsInRegion = pageIndex.rtreeSearchHasLabel(hashedLineCC.bounds, LB.PageAtomTmp)
        (hashedLineCC, charsInRegion)
      }).sortBy { case (_, chars) => chars.length }.reverse
      sortedHashedLinesWithChars.zipWithIndex
        .foreach{ case ((hashLineCC, hashedChars), index) =>
          val remainingChars = hashedChars.filter(_.hasLabel(LB.PageAtomTmp)).nonEmpty
          if (remainingChars) {
            gifBuilder.indicate(s"Processing Hash-Line ${index} of ${sortedHashedLinesWithChars.length}", hashLineCC.bounds(), LB.PageAtomTmp)
            val extendedLineRegion = hashLineCC.bounds.withinRegion(textBlock)
              .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
              .getOrElse { hashLineCC.bounds }
            // Progressively scan from center out to find super/subs
            val height = extendedLineRegion.height
            val top = extendedLineRegion.top

            val centerLine = extendedLineRegion
              .copy(height = height/2, top=top+height/4)


            gifBuilder.indicate("Search Line Region", centerLine, LB.PageAtomTmp)

            // Now find all chars in queryRegion and string them together into a single visual line
            val visualLineAtoms = pageIndex.rtreeSearchHasLabel(centerLine, LB.PageAtomTmp)

            if (visualLineAtoms.nonEmpty) {
              val xSortedAtoms = visualLineAtoms.sortBy(_.bounds.left).toSeq

              val visualLineBBox = xSortedAtoms.map(_.bounds).reduce { (c1, c2) => c1 union c2 }

              gifBuilder.indicate("Found VLine Atoms", visualLineBBox, LB.PageAtomTmp)

              xSortedAtoms.foreach { cc =>
                pageIndex.removeLabel(cc, LB.PageAtomTmp)
                pageIndex.addLabel(cc, LB.PageAtomGrp)
              }

              val visualLineCC = labelRegion(visualLineBBox, LB.VisualLine)

              createTextRowFromVisualLine(visualLineCC, xSortedAtoms)

              xSortedAtoms.foreach { cc =>
                pageIndex.removeLabel(cc, LB.PageAtomGrp)
              }
            }

          }
        }
    }

    gifBuilder.finish()

  }

  // def findVisualLines(orderedTextBlocks: Seq[LTBounds]): Unit = {

  //   val atomSingletons = pageIndex.initClustering(LB.VisualLines, _.hasLabel(LB.PageAtom))

  //   atomSingletons.foreach{ a => a.addLabel(LB.NotClustered) }

  //   for { textBlock <- orderedTextBlocks } {

  //     // Find all hashed-lines in text block, sort into descending order by # of chars
  //     val sortedHashedLinesWithChars = (for {
  //       hashedLineCC <- pageIndex.rtreeSearch(textBlock, LB.LineByHash)
  //     } yield {
  //       val charsInRegion = pageIndex.rtreeSearchHasLabel(hashedLineCC.bounds, LB.PageAtom)
  //       (hashedLineCC, charsInRegion)
  //     }).sortBy { case (_, chars) => chars.length }.reverse


  //     sortedHashedLinesWithChars.zipWithIndex
  //       .foreach{ case ((hashLineCC, hashedChars), index) =>

  //         val horizontalStripeRegion = hashLineCC.bounds.withinRegion(textBlock)
  //           .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
  //           .getOrElse { hashLineCC.bounds }

  //         val height = horizontalStripeRegion.height
  //         val top = horizontalStripeRegion.top

  //         val centerLine = horizontalStripeRegion
  //           .copy(height = height/2, top=top+height/4)

  //         // Now find all chars in queryRegion and string them together into a single visual line
  //         val visualLineAtoms = pageIndex.rtreeSearchHasAllLabels(centerLine, LB.PageAtom, LB.NotClustered)

  //         if (visualLineAtoms.nonEmpty) {
  //           val xSortedAtoms = visualLineAtoms.sortBy(_.bounds.left).toSeq
  //           val visualLineBBox = xSortedAtoms.map(_.bounds).reduce { (c1, c2) => c1 union c2 }
  //           val visualLineCC = labelRegion(visualLineBBox, LB.VisualLine)

  //           xSortedAtoms.foreach { _.removeLabel(LB.NotClustered) }

  //           pageIndex.unionAll(LB.VisualLines, xSortedAtoms)

  //           createTextRowFromVisualLine(visualLineCC, xSortedAtoms)

  //         }
  //       }
  //   }
  // }



  def createTextRowFromVisualLine(visualLineCC: Component, visualLineAtoms: Seq[Component]): Unit = {

    val visualLineBounds = visualLineCC.bounds()

    val gifBuilder = vis.gifBuilder(
      s"createTextRowsFromVisualLines-${visualLineBounds.left}-${visualLineBounds.bottom}",
      1.seconds
    )


    gifBuilder.indicate(
      s"Text From VisualLine ${visualLineBounds.left}-${visualLineBounds.bottom}",
      visualLineBounds, LB.PageAtomGrp, LB.PageAtomTmp
    )


    def shrinkVisualLineToModalTopAndBottom(): LTBounds = {

      val modalBaselineI = modalValue(visualLineAtoms, _.bounds().bottom.unwrap)
        .getOrElse(visualLineBounds.bottom.unwrap)

      val modalBaseline = FloatRep(modalBaselineI)

      val modalToplineI = modalValue(visualLineAtoms, _.bounds.top.unwrap)
        .getOrElse(visualLineBounds.top.unwrap)

      val modalTopline = FloatRep(modalToplineI)

      val height = modalBaseline-modalTopline

      val visualLineModalBounds = if (height > 0) {
        visualLineBounds.copy(
          top=modalTopline, height=modalBaseline-modalTopline
        )
      } else {
        visualLineBounds
      }

      gifBuilder.indicate("Modal-base/top VisualLine", visualLineModalBounds)

      visualLineModalBounds
    }

    if (visualLineAtoms.nonEmpty) {
      val visualLineModalBounds = shrinkVisualLineToModalTopAndBottom()

      gifBuilder.indicate(s"VisualLine Bounds", visualLineBounds)
      gifBuilder.indicate(s"ModalVisualLine Bounds", visualLineModalBounds)

      val topLine = visualLineModalBounds.toLine(Dir.Top) // .translate(y=0.5)
      val bottomLine =  visualLineModalBounds.toLine(Dir.Bottom) // .translate(y = -0.5)

      val topIntersections = pageIndex.rtreeSearchLineHasLabel(topLine, LB.PageAtomGrp)
      val bottomIntersections = pageIndex.rtreeSearchLineHasLabel(bottomLine, LB.PageAtomGrp)

      val topIntersects = topIntersections.map(_.id)
      val bottomIntersects = bottomIntersections.map(_.id)

      gifBuilder.indicate(s"Top intersection Line", topLine.bounds(), LB.PageAtomGrp)
      gifBuilder.indicate(s"Bottom intersection Line", bottomLine.bounds(), LB.PageAtomGrp)
      gifBuilder.indicate(s"Top Atoms", topIntersections.map(_.bounds()), LB.PageAtomGrp)
      gifBuilder.indicate(s"Bottom Atoms", bottomIntersections.map(_.bounds()), LB.PageAtomGrp)


      var dbgGrid = TB.Grid.widthAligned(
        (1, AlignLeft),  // join indicator
        (2, AlignLeft),  // char(s)
        (6, AlignRight), // char.top
        (1, AlignLeft),  // space
        (6, AlignRight), // char.bottom
        (1, AlignLeft),  // space
        (6, AlignLeft), // labels
        (1, AlignLeft)  // space
      )

      DocumentSegmenter.vtrace.ifTrace({
        dbgGrid = dbgGrid.addRow(
          "J",
          "",
          "Top|||",
          "",
          "Bottm|",
          "",
          "pins||",
          ""
        )
        dbgGrid = dbgGrid.addRow(" ", "  ", "      ", " ", "      ", " ", "      ", " ")
      })


      def fromComponents(ccs: Seq[Component]): TextGrid.Row = {
        new TextGrid.MutableRow { self =>
          val init = ccs.map{
            case cc@ AtomicComponent(id, charAtom, roleLabel) =>
              val intersectsTop = topIntersects.contains(cc.id)
              val intersectsBottom = bottomIntersects.contains(cc.id)

              val cells = charAtom.char.headOption.map{ char =>
                val cell = TextGrid.PageItemCell(charAtom, Seq(), char)

                val continuations = charAtom.char.tail.map { cn =>
                  cell.createRightExpansion(cn)
                }

                val allCells: Seq[TextGrid.GridCell] = cell +: continuations

                if (cc.bounds.bottom == visualLineModalBounds.bottom) {
                  // Center-text
                } else if (intersectsTop && !intersectsBottom) {
                  gifBuilder.indicate(s"SuperScript", cc.bounds(), LB.PageAtomGrp)
                  allCells.foreach{ _.addLabel(LB.Sup) }
                } else if (!intersectsTop && intersectsBottom) {
                  gifBuilder.indicate(s"SubScript", cc.bounds(), LB.PageAtomGrp)
                  allCells.foreach{ _.addLabel(LB.Sub) }
                } else {
                  gifBuilder.indicate(s"???Script", cc.bounds(), LB.PageAtomGrp)
                }


                DocumentSegmenter.vtrace.ifTrace {

                  val pinChars = cell.pins.toList.map(_.pinChar).sorted.mkString

                  dbgGrid = dbgGrid.addRow(
                    " ",
                    cell.char.toString(),
                    cell.pageRegion.bbox.top.pp,
                    "~",
                    cell.pageRegion.bbox.bottom.pp,
                    "~",
                    pinChars,
                    "."
                  )
                }

                allCells
              }
              cells.getOrElse(Seq())

            case c@ RegionComponent(id, roleLabel, pageRegion, text) =>
              Seq()

          }

          cells.appendAll(init.flatten)
        }
      }

      val textRow = fromComponents(visualLineAtoms)

      DocumentSegmenter.vtrace.ifTrace {
        println(dbgGrid.toBox().transpose())
      }

      // Convert consecutive sup/sub to single label
      textRow.groupBy { case (cell1, cell2) =>
        cell1.labels.contains(LB.Sub) && cell2.labels.contains(LB.Sub)
      }.foreach { nCursor =>
        nCursor.foreach{ c =>
          val hasSubLabel = c.focus.exists { _.labels.contains(LB.Sub) }
          if (hasSubLabel) {
            c.focus.foreach { fcell =>
              fcell.pins.remove(LB.Sub.U)
            }
            c.addLabel(LB.Sub)
          }
        }
      }

      textRow.groupBy { case (cell1, cell2) =>
        cell1.labels.contains(LB.Sup) && cell2.labels.contains(LB.Sup)
      }.foreach { nCursor =>
        nCursor.foreach{ c =>
          val hasSubLabel = c.focus.exists { _.labels.contains(LB.Sup) }
          if (hasSubLabel) {
            c.focus.foreach { fcell =>
              fcell.pins.remove(LB.Sup.U)
            }
            c.addLabel(LB.Sup)
          }
        }
      }


      val spacedRow = insertSpacesInRow(textRow)

      val visualLineClusterCC = pageIndex.addCluster(LB.VisualLine, visualLineAtoms)

      pageIndex.setComponentText(visualLineClusterCC, LB.VisualLine, spacedRow)

      val textReflow = convertTextRowToTextReflow(spacedRow)

      docStore.labelRegions(LB.VisualLine, Seq(visualLineCC.pageRegion)).foreach { zoneId =>
        docStore.setTextReflowForZone(zoneId, textReflow)
      }
    }
    gifBuilder.finish()
  }


  def convertTextRowToTextReflow(textRow: TextGrid.Row): TextReflow = {

    val textReflowAtoms: Seq[TextReflow] = textRow.cells.map{ _ match {
      case cell@ TextGrid.LeftExpansionCell(char, root)  => Some(insert(char.toString()))
      case cell@ TextGrid.RightExpansionCell(char, root) => Some(insert(char.toString()))
      case cell@ TextGrid.LeftInsertCell(char, loc)      => Some(insert(char.toString()))
      case cell@ TextGrid.RightInsertCell(char, loc)     => Some(insert(char.toString()))

      case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
        headItem match {
          case charAtom : CharAtom =>
            val tr = if (cell.labels.nonEmpty) {
              labeled(cell.labels.toSet, atom(charAtom))
            } else {
              atom(charAtom)
            }
            Some(tr)
          case _ => None
        }

    }}.flatten

    flows(textReflowAtoms)
  }


  def approximateLineBins(charBoxes: Seq[AtomicComponent]): Unit = {
    charBoxes
      .groupBy{ _.bounds.bottom.unwrap }
      .toSeq
      .map { case (bottomY, charBoxes) =>
        charBoxes.sortBy(_.bounds.left)
      }.foreach{ lineBin =>
        mpageIndex.labelRegion(lineBin, LB.LineByHash)
      }
  }
}
