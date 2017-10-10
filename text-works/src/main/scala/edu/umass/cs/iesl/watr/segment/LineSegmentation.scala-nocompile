package edu.umass.cs.iesl.watr
package segment

// import spindex._

// import watrmarks._

// import geometry._
// import geometry.syntax._

// import utils.{RelativeDirection => Dir}
// import TypeTags._
// import utils.ExactFloats._
// import utils.EnrichNumerics._
// import segment.{SegmentationLabels => LB}

// import textreflow.data._
// import textgrid._
// import textboxing.{TextBoxing => TB}, TB._

// import scala.collection.mutable
// import utils.SlicingAndDicing._

// import shapeless._

trait LineFinding extends ColumnFinding { self =>
}

//   lazy val lineFinding = self

//   def runLineSegmentation(): Unit = {
//     val components = pageIndex.components.getPageAtoms

//     approximateLineBins(components)

//     splitLinesWithOverlaps()

//     columnFinder.runColumnFinder()

//     splitLinesOnWhitespaceColumns()

//     createLog("ShowReadingOrder")

//     val orderedRegions = findReadingOrder(pageGeometry)()


//     val readingBlocks = orderedRegions.map { labelRegion(_, LB.ReadingBlock) }

//     pageIndex.components.setOrdering(LB.ReadingBlocks, readingBlocks)


//     // TODO extract path objects in groups, rather than singly, to avoid trying to rewrite large drawn shapes
//     // rewritePathObjects(orderedRegions)

//     // pageIndex.reportClusters()

//     findVisualLines(orderedRegions)


//     setVisualLineOrdering()

//     // Group visual lines into text blocks, s.t. each each block is semantically meaningful unit, e.g., part of a paragraph, a chart/table/figure+caption, or footnote
//     // Group text blocks into paragraphs (within single page)


//   }


//   private def setVisualLineOrdering(): Seq[Component] = {
//     // reorder visual lines within and across reading blocks

//     val allPageLines: mutable.ArrayBuffer[Component] = mutable.ArrayBuffer.empty

//     val vlineClusterRepLabel = LB.VisualLine.qualifiedAs("cluster").qualifiedAs("rep")

//     pageIndex.components.getComponentsWithLabel(vlineClusterRepLabel)
//       .foreach{ a =>
//         pageIndex.components.addLabel(a, LB.Tmp)
//       }

//     for {
//       readingBlock <-  pageIndex.components.getOrdering(LB.ReadingBlocks)
//       vlineRoots   <- Seq(pageIndex.components.searchOverlapping(readingBlock.bounds(), vlineClusterRepLabel, LB.Tmp)) // TODO this search picks up some lines multiple times
//       if vlineRoots.nonEmpty


//       sortedLines   = vlineRoots.sortBy(_.bounds.bottom)
//       vlineCluster   = pageIndex.components.addCluster(LB.ReadingBlockLines, sortedLines)

//       // Tie together the readingBlock and the canonicalReadingBlockLine
//       _                = pageIndex.components.addRelation(readingBlock, LB.HasVisualLines, vlineCluster)
//       vlineClusterTmp  = pageIndex.components.getRelation(readingBlock, LB.HasVisualLines)

//       line         <- sortedLines
//     }  {
//       pageIndex.components.removeLabel(line, LB.Tmp)
//       allPageLines.append(line)
//     }

//     if (allPageLines.nonEmpty) {
//       pageIndex.components.setOrdering(LB.PageLines, allPageLines)
//     }

//     allPageLines
//   }


//   private def findVisualLines(orderedTextBlocks: Seq[LTBounds]): Unit =  {
//     // val gifBuilder = vis.gifBuilder("findVisualLines", 500.millis)

//     pageIndex.components.getComponentsWithLabel(LB.PageAtom)
//       .foreach{ a => pageIndex.components.addLabel(a, LB.PageAtomTmp) }

//     // gifBuilder.addFrame("Starting", LB.PageAtomTmp)

//     for { textBlock <- orderedTextBlocks } {
//       // gifBuilder.indicate("Examining Block", textBlock, LB.PageAtomTmp)
//       val sortedHashedLinesWithChars = (for {
//         hashedLineCC <- pageIndex.components.searchIntersecting(textBlock, LB.LineByHash)
//       } yield {
//         val charsInRegion = pageIndex.components.searchOverlapping(hashedLineCC.bounds, LB.PageAtomTmp)
//         (hashedLineCC, charsInRegion)
//       }).sortBy { case (_, chars) => chars.length }.reverse


//       sortedHashedLinesWithChars.zipWithIndex
//         .foreach{ case ((hashLineCC, hashedChars), index) =>
//           val remainingChars = hashedChars.filter(_.hasLabel(LB.PageAtomTmp)).nonEmpty
//           if (remainingChars) {
//             // gifBuilder.indicate(s"Processing Hash-Line ${index} of ${sortedHashedLinesWithChars.length}", hashLineCC.bounds(), LB.PageAtomTmp)
//             val extendedLineRegion = hashLineCC.bounds.withinRegion(textBlock)
//               .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
//               .getOrElse { hashLineCC.bounds }
//             // Progressively scan from center out to find super/subs
//             val height = extendedLineRegion.height
//             val top = extendedLineRegion.top

//             val centerLine = extendedLineRegion
//               .copy(height = height/2, top=top+height/4)


//             // Now find all chars in queryRegion and string them together into a single visual line
//             val visualLineAtoms = pageIndex.components.searchOverlapping(centerLine, LB.PageAtomTmp)

//             if (visualLineAtoms.nonEmpty) {
//               val xSortedAtoms = visualLineAtoms.sortBy(_.bounds.left).toSeq

//               val visualLineBBox = xSortedAtoms.map(_.bounds).reduce { (c1, c2) => c1 union c2 }

//               // gifBuilder.indicate("Found VLine Atoms", visualLineBBox, LB.PageAtomTmp)

//               xSortedAtoms.foreach { cc =>
//                 pageIndex.components.removeLabel(cc, LB.PageAtomTmp)
//                 pageIndex.components.addLabel(cc, LB.PageAtomGrp)
//               }

//               val visualLineCC = labelRegion(visualLineBBox, LB.VisualLine)

//               createTextRowFromVisualLine(visualLineCC, xSortedAtoms)

//               xSortedAtoms.foreach { cc =>
//                 pageIndex.components.removeLabel(cc, LB.PageAtomGrp)
//               }
//             }

//           }
//         }
//     }

//     // gifBuilder.finish()

//   }

//   private def findModalBoundingRect(visualLineCC: Component, visualLineAtoms: Seq[Component]): LTBounds = {
//     val visualLineBounds = visualLineCC.bounds()

//     val modalBaselineI = modalValue(visualLineAtoms, _.bounds().bottom.unwrap)
//       .getOrElse(visualLineBounds.bottom.unwrap)

//     val modalBaseline = FloatRep(modalBaselineI)

//     val modalToplineI = modalValue(visualLineAtoms, _.bounds.top.unwrap)
//       .getOrElse(visualLineBounds.top.unwrap)

//     val modalTopline = FloatRep(modalToplineI)

//     val height = modalBaseline-modalTopline

//     val visualLineModalBounds = if (height > 0) {
//       visualLineBounds.copy(
//         top=modalTopline, height=modalBaseline-modalTopline
//       )
//     } else {
//       visualLineBounds
//     }

//     // gifBuilder.indicate("Modal-base/top VisualLine", visualLineModalBounds)

//     visualLineModalBounds
//   }


//   private def textRowFromComponents(visualLineClusterCC: Component, visualLineAtoms: Seq[Component]): TextGrid.Row = {


//     val visualLineModalCC = pageIndex.components.getRelation(visualLineClusterCC, LB.VisualLineModal).head
//     val visualLineCC = pageIndex.components.getRelation(visualLineClusterCC, LB.VisualLine).head

//     val visualLineModalBounds = visualLineModalCC.bounds()

//     val (topIntersects, bottomIntersects) = findLineAtomScriptPositions(visualLineCC, visualLineAtoms)


//     new TextGrid.MutableRow { self =>
//       val init = visualLineAtoms.map{
//         case cc@ AtomicComponent(id, charAtom, roleLabel) =>
//           val intersectsTop = topIntersects.contains(cc.id)
//           val intersectsBottom = bottomIntersects.contains(cc.id)

//           val cells = charAtom.char.headOption.map{ char =>
//             val cell = TextGrid.PageItemCell(charAtom, Seq(), char)

//             val continuations = charAtom.char.tail.map { cn =>
//               cell.createRightExpansion(cn)
//             }

//             val allCells: Seq[TextGrid.GridCell] = cell +: continuations

//             if (cc.bounds.bottom == visualLineModalBounds.bottom) {
//               // Center-text
//             } else if (intersectsTop && !intersectsBottom) {
//               // gifBuilder.indicate(s"SuperScript", cc.bounds(), LB.PageAtomGrp)
//               allCells.foreach{ _.addLabel(LB.Sup) }
//             } else if (!intersectsTop && intersectsBottom) {
//               // gifBuilder.indicate(s"SubScript", cc.bounds(), LB.PageAtomGrp)
//               allCells.foreach{ _.addLabel(LB.Sub) }
//             } else {
//               // gifBuilder.indicate(s"???Script", cc.bounds(), LB.PageAtomGrp)
//             }


//             tracer.printLog {
//               val pinChars = cell.pins.toList.map(_.pinChar).sorted.mkString
//               dbgGrid = dbgGrid.map { grid =>
//                  grid.addRow(
//                   "-",
//                   cell.char.toString(),
//                   cell.pageRegion.bbox.top.pp,
//                   "~",
//                   cell.pageRegion.bbox.bottom.pp,
//                   "~",
//                   pinChars,
//                   "."
//                 )
//               }
//             }

//             allCells
//           }
//           cells.getOrElse(Seq())

//         case c@ RegionComponent(id, roleLabel, pageRegion, maybeText) =>
//           // TODO this is skipping over text represented as paths (but I have to figure out sup/sub script handling to make it work)
//           Seq()

//       }

//       cells.appendAll(init.flatten)
//     }
//   }

//   var dbgGrid = Lazy[TB.Grid]{ TB.Grid.widthAligned() }

//   // Group line atoms into center/sub/superscript bins
//   private def findLineAtomScriptPositions(visualLineCC: Component, visualLineAtoms: Seq[Component]): (Seq[Int@@ComponentID], Seq[Int@@ComponentID]) = {

//     val visualLineBounds = visualLineCC.bounds()

//     // top 1/3  & bottom 1/3 ==> centered

//     val slices = visualLineBounds.getHorizontalSlices(3)
//     val Seq(topSlice, _, bottomSlice) = slices

//     val topIntersections = pageIndex.components.searchOverlapping(topSlice, LB.PageAtomGrp)
//     val bottomIntersections = pageIndex.components.searchOverlapping(bottomSlice, LB.PageAtomGrp)
//     // val middleIntersections = pageIndex.components.searchOverlapping(middleSlice, LB.PageAtomGrp)

//     val topIntersects = topIntersections.map(_.id)
//     val bottomIntersects = bottomIntersections.map(_.id)

//     // gifBuilder.indicate(s"Top intersection Line", topLine.bounds(), LB.PageAtomGrp)
//     // gifBuilder.indicate(s"Bottom intersection Line", bottomLine.bounds(), LB.PageAtomGrp)
//     // gifBuilder.indicate(s"Top Atoms", topIntersections.map(_.bounds()), LB.PageAtomGrp)
//     // gifBuilder.indicate(s"Bottom Atoms", bottomIntersections.map(_.bounds()), LB.PageAtomGrp)



//     tracer.printLog {
//       dbgGrid = dbgGrid.map { grid =>
//         var g = TB.Grid.widthAligned(
//           (1, AlignLeft),  // join indicator
//           (2, AlignLeft),  // line text (char(s))
//           (6, AlignRight), // char.top
//           (1, AlignLeft),  // space
//           (6, AlignRight), // char.bottom
//           (1, AlignLeft),  // space
//           (6, AlignLeft), // labels
//           (1, AlignLeft)  // space
//         )

//         g = g.addRow(
//           "J",
//           "",
//           "Top|||",
//           "",
//           "Bottm|",
//           "",
//           "pins||",
//           ""
//         )
//         g = g.addRow(" ", "  ", "      ", " ", "      ", " ", "      ", " ")
//         g
//       }
//     }



//     (topIntersects, bottomIntersects)
//   }


//   private def createTextRowFromVisualLine(visualLineCC: Component, visualLineAtoms: Seq[Component]): Unit = {

//     if (visualLineAtoms.nonEmpty) {
//       // Associate visualLine bounds (modal, normal) w/visual line cluster
//       val visualLineModalBounds = findModalBoundingRect(visualLineCC, visualLineAtoms)
//       val visualLineModalCC = labelRegion(visualLineModalBounds, LB.VisualLineModal)
//       val visualLineClusterCC = pageIndex.components.addCluster(LB.VisualLine, visualLineAtoms)

//       // pageIndex.components.addRelation(visualLineCC, LB.VisualLineModal, visualLineModalCC)
//       pageIndex.components.addRelation(visualLineClusterCC, LB.VisualLineModal, visualLineModalCC)
//       pageIndex.components.addRelation(visualLineClusterCC, LB.VisualLine, visualLineCC)

//       // gifBuilder.indicate(s"VisualLine Bounds", visualLineBounds)
//       // gifBuilder.indicate(s"ModalVisualLine Bounds", visualLineModalBounds)

//       val textRow = textRowFromComponents(visualLineClusterCC, visualLineAtoms)


//       // tracer.printLog {
//       //   println(dbgGrid.toBox().transpose())
//       // }

//       // Convert consecutive sup/sub to single label
//       def relabel(c: GridCursor, l: Label): Unit = {
//          c.findNext(_.hasLabel(l))
//            .foreach{ cur =>
//                val win = cur.slurpRight(_.hasLabel(l))
//                win.removePins(l)
//                win.addLabel(l)
//                win.nextCursor().foreach(relabel(_, l))
//             }
//       }

//       textRow.toCursor().foreach { c => relabel(c, LB.Sub) }
//       textRow.toCursor().foreach { c => relabel(c, LB.Sup) }


//       val spacedRow = insertSpacesInRow(textRow)

//       val supSubLabeledRow = spacedRow.toCursor()
//         .map { cursor =>
//           cursor.unfoldCursorToRow { c =>
//             val focus = c.focus

//             if      (focus.hasPin(LB.Sub.B))  { c.insertCharLeft ('₍').next }
//             else if (focus.hasPin(LB.Sub.L))  { c.insertCharRight('₎').some }
//             else if (focus.hasPin(LB.Sub.U))  { c.insertCharLeft('₍').next.get.insertCharRight('₎').some }
//             else if (focus.hasPin(LB.Sup.B))  { c.insertCharLeft ('⁽').next }
//             else if (focus.hasPin(LB.Sup.L))  { c.insertCharRight('⁾').some }
//             else if (focus.hasPin(LB.Sup.U))  { c.insertCharLeft('⁽').next.get.insertCharRight('⁾').some }
//             else { c.some }

//           }
//         }.getOrElse { spacedRow }




//       pageIndex.components.setComponentText(visualLineClusterCC, LB.VisualLine, supSubLabeledRow)

//       val textReflow = convertTextRowToTextReflow(supSubLabeledRow)

//       docStore.labelRegions(LB.VisualLine, Seq(visualLineCC.pageRegion)).foreach { zoneId =>
//         docStore.setTextReflowForZone(zoneId, textReflow)
//       }
//     }

//     // gifBuilder.finish()

//   }

//   private def convertTextRowToTextReflow(textRow: TextGrid.Row): TextReflow = {

//     val textReflowAtoms: Seq[TextReflow] = textRow.cells.map{ _ match {
//       case  TextGrid.LeftExpansionCell(char, root)  => Some(insert(char.toString()))
//       case  TextGrid.RightExpansionCell(char, root) => Some(insert(char.toString()))
//       case  TextGrid.LeftInsertCell(char, _)      => Some(insert(char.toString()))
//       case  TextGrid.RightInsertCell(char, loc@_)     => Some(insert(char.toString()))

//       case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
//         headItem match {
//           case charAtom : CharAtom =>
//             val tr = if (cell.labels.nonEmpty) {
//               labeled(cell.labels.toSet, atom(charAtom))
//             } else {
//               atom(charAtom)
//             }
//             Some(tr)
//           case _ => None
//         }

//     }}.flatten

//     flows(textReflowAtoms)
//   }


//   private def approximateLineBins(charBoxes: Seq[AtomicComponent]): Unit = {

//     implicit val log = createLog("approximateLineBins")

//     charBoxes
//       .groupBy{ _.bounds.bottom.unwrap }
//       .toSeq
//       .map { case (bottomY, charBoxes) =>
//         charBoxes.sortBy(_.bounds.left)
//       }.foreach{ lineBin =>
//         mpageIndex.labelRegion(lineBin, LB.LineByHash)
//       }

//     tracer.jsonAppend {
//       val showableItems = pageIndex.components.componentRTree.getItems
//         .filter { _.hasLabel(LB.LineByHash) }
//         .map { _.bounds() }

//       showRegions(s"Hashed Lines", showableItems)
//     }

//   }

//   private def findReadingOrder(initRegion: LTBounds)(level: Int=0): Seq[LTBounds] = {


//     tracer.jsonLog {
//       if (level==0) {
//         createLog("ShowAllWSColumns")

//         val wsCols = pageIndex.components.searchOverlapping(pageGeometry, LB.WhitespaceCol)
//           .sortBy(cc => (cc.bounds.top, cc.bounds.left))
//           .map(_.bounds())

//         appendLog("ShowAllWSColumns", {
//           showRegions(s"W.S. Columns", wsCols)
//         })
//       }
//     }

//     val maybeCol = pageIndex.components.searchOverlapping(initRegion, LB.WhitespaceCol)
//       .sortBy(cc => (cc.bounds.top, cc.bounds.left))
//       .headOption

//     // Flash all Whitespace Cols found:...

//     maybeCol.map{ wsCol =>
//       val colBounds = wsCol.bounds.withinRegion(initRegion)
//       val upperRegion = colBounds.adjacentRegions(Dir.TopLeft, Dir.Top, Dir.TopRight)
//       val lowerRegion = colBounds.adjacentRegions(Dir.BottomLeft, Dir.Bottom, Dir.BottomRight)

//       val leftRegion = colBounds.adjacentRegion(Dir.Left)
//       val rightRegion = colBounds.adjacentRegion(Dir.Right)

//       val adjacentRegions = List(upperRegion, leftRegion, rightRegion, lowerRegion).flatten

//       // tracer.jsonLog(ShowReadingOrder) {
//       //   zipFlashThroughRegions( ...  )

//       tracer.jsonLog {
//         appendLog("ShowReadingOrder", {
//           zipFlashThroughRegions(
//             s"WS.Col + Adjacent Regions ($level)",
//             wsCol.bounds() :: adjacentRegions
//           )

//         })
//       }


//       val rs = Seq(leftRegion, rightRegion, lowerRegion)
//         .flatten.flatMap{ r =>
//           findReadingOrder(r)(level+1)
//         }

//       upperRegion.map( _ +: rs ).getOrElse(rs)
//     } getOrElse { Seq(initRegion) }
//   }

//   private def splitLinesWithOverlaps(): Unit =  {
//     for {
//       cc <- pageIndex.components.getComponentsWithLabel(LB.LineByHash)
//     } {
//       // Split up lines into strictly non-overlapping regions
//       val intersects = pageIndex.components.searchIntersecting(cc.bounds, LB.LineByHash)


//       if (intersects.length > 1) {
//         val totalBounds = intersects.map(_.bounds).reduce(_ union _)
//         val charsInRegion = pageIndex.components.searchIntersecting(totalBounds, LB.PageAtom)
//         // Remove the LineByHash regions
//         // iterate over chars left-to-right and group them into non-overlaps and overlaps
//         val allChars = charsInRegion.sortBy(_.bounds.left)

//         allChars.groupByPairs { (c1, c2) =>
//           c1.bounds.bottom == c2.bounds.bottom
//         }.foreach{ ccs =>
//           mpageIndex.labelRegion(ccs, LB.LineByHash)
//         }

//         intersects.foreach { cc =>
//           pageIndex.components.removeComponent(cc)
//         }
//       }
//     }
//   }

//   private def splitLinesOnWhitespaceColumns(): Unit = {

//     for {
//       colRegion <- pageIndex.components.getComponentsWithLabel(LB.WhitespaceCol)
//       intersectedLine <- pageIndex.components.searchIntersecting(colRegion.bounds, LB.LineByHash)
//     } {

//       val charsInRegion = pageIndex.components.searchIntersecting(intersectedLine.bounds, LB.PageAtom)
//       val allChars = charsInRegion.sortBy(_.bounds.left)

//       val (leftSplit, rightSplit) = allChars.span(_.bounds.left <= colRegion.bounds.left)

//       mpageIndex.labelRegion(leftSplit, LB.LineByHash)
//       mpageIndex.labelRegion(rightSplit, LB.LineByHash)

//       pageIndex.components.removeComponent(intersectedLine)
//     }

//   }



//   private def pairwiseItemDistances(sortedLineCCs: Seq[PageItem]): Seq[FloatExact] = {
//     val cpairs = sortedLineCCs.sliding(2).toList

//     val dists = cpairs.map({
//       case Seq(c1, c2)  => (c2.bbox.left - c1.bbox.right)
//       case _  => 0d.toFloatExact()
//     })

//     dists :+ 0d.toFloatExact()
//   }

//   private def guessWordbreakWhitespaceThreshold(sortedLineCCs: Seq[PageItem]): FloatExact =  {

//     val charDists = pairwiseItemDistances(sortedLineCCs)
//       .toSet.toSeq

//     val charWidths = sortedLineCCs.map(_.bbox.width)

//     val widestChar = charWidths.max.asDouble()
//     val narrowestChar = charWidths.min.asDouble()
//     val avgCharWidth = (widestChar + narrowestChar) / 2

//     // Don't  accept a space wider than (some magic number)*the widest char?
//     val saneCharDists = charDists
//       .filter(_ < widestChar*2 )
//       .filterNot(_.unwrap == 0)
//       .map(_.asDouble())


//     val noSplitThreshold = widestChar
//     val threshold = if (saneCharDists.length <= 1 || sortedLineCCs.length <= 1) {
//       // If there is only 1 distance between chars, the line is only 1 word (no word breaks)
//       noSplitThreshold
//     } else {
//       val averageDist = saneCharDists.sum / saneCharDists.length

//       val charDistSpread = saneCharDists.max - saneCharDists.min
//       if (charDistSpread < avgCharWidth / 4) {
//         noSplitThreshold
//       } else {

//         // val (littleDists, bigDists) = saneCharDists.sorted.span(_ < averageDist)
//         averageDist
//       }
//     }

//     tracer.printLog {
//       println(
//         s"""|guessWordbreakWhitespaceThreshold
//             | Char Dists      = ${charDists.map(_.pp).mkString(", ")}
//             | Sane Dists      = ${saneCharDists.map(_.pp).mkString(", ")}
//             | Wide/Nar/Avg Ch = ${widestChar.pp}/${narrowestChar.pp}/${avgCharWidth.pp}
//             | Split threshold = ${threshold.pp}
//             |""".stripMargin.mbox
//       )
//     }


//     threshold.toFloatExact
//   }


//   private def insertSpacesInRow(textRow: TextGrid.Row): TextGrid.Row =  {
//     val lineCCs = textRow.cells.collect{
//       case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
//         headItem
//     }

//     val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

//     var spacingDbgGrid = Grid.widthAligned(
//       (1, AlignLeft),  // join indicator
//       (2, AlignLeft),  // char(s)
//       (6, AlignRight), // char.left
//       (1, AlignLeft),  // space
//       (6, AlignRight), // char.right
//       (1, AlignLeft),  // space
//       (6, AlignRight), // c1 - c2 dist
//       (1, AlignLeft),  // space
//       (5, AlignRight)  // char.width
//     )
//     tracer.printLog {
//       spacingDbgGrid = spacingDbgGrid.addRow(
//         "J",
//         "",
//         "LEFT||",
//         "",
//         "RIGHT|",
//         "",
//         "DIST||",
//         "",
//         "WIDTH"
//       )
//       spacingDbgGrid = spacingDbgGrid.addRow(" ", "  ", "      ", " ", "      ", " ", "      ", " ", "     ")
//     }

//     val res = textRow.toCursor().map{ cursor =>
//       val finalRow = cursor.unfoldCursorToRow { nextCursor =>

//         val wordWin = nextCursor.toWindow.slurpRight{ case (win, cell) =>

//           val pairwiseDist = cell.pageRegion.bbox.left - win.last.pageRegion.bbox.right
//           val willGroup = pairwiseDist < splitValue

//            tracer.printLog {
//              val c1 = win.last
//              spacingDbgGrid = spacingDbgGrid.addRow(
//                if(willGroup) "_" else "$",
//                c1.char.toString,
//                c1.pageRegion.bbox.left.pp,
//                "~",
//                c1.pageRegion.bbox.right.pp,
//                "~",
//                pairwiseDist.pp,
//                "~",
//                c1.pageRegion.bbox.width.pp
//              )
//            }


//           willGroup
//         }

//         if (!wordWin.atEnd) {
//           tracer.printLog {
//             spacingDbgGrid = spacingDbgGrid.addRow(
//               " ",
//               " ",
//               " ",
//               " ",
//               " ",
//               " ",
//               " ",
//               " ",
//               " "
//             )
//           }

//           wordWin.extendRight(' ').toLastCursor.some
//         } else None

//       }

//       // tracer.printLog {
//       //   println(spacingDbgGrid.toBox().transpose())
//       // }

//       finalRow

//     } getOrElse { textRow }

//     res
//   }

// }
