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

import scala.collection.mutable
import utils.SlicingAndDicing._
import edu.umass.cs.iesl.watr.tracing.VisualTracer




class LineFinder(
  mpageIndex: MultiPageIndex,
  pageId: Int@@PageID,
  pageNum: Int@@PageNum,
  tracer: VisualTracer
) extends PageSegmenter(pageId, pageNum, mpageIndex, tracer) {


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

    val readingBlocks = orderedRegions.map { labelRegion(_, LB.ReadingBlock) }

    pageIndex.setOrdering(LB.ReadingBlocks, readingBlocks)
    val tmpReadingBlocks = pageIndex.getOrdering(LB.ReadingBlocks)

    // println(" get/set ordering  = ")
    // println("  "+tmpReadingBlocks.map(_.id).toList.sorted)
    // println("  "+readingBlocks.map(_.id).toList.sorted)
    assert(tmpReadingBlocks.map(_.id).toSet == readingBlocks.map(_.id).toSet)

    // pageIndex.addCluster(LB.ReadingBlocks, readingBlocks)

    // vis.writeRTreeImage("06-ReadingOrder", LB.LineByHash, stdLabels():_*)

    // TODO extract path objects in groups, rather than singly, to avoid trying to rewrite large drawn shapes
    rewritePathObjects(orderedRegions)

    findVisualLines(orderedRegions)

    vis.writeRTreeImage("06-VisualLines", LB.VisualLine, stdLabels(LB.PageAtom):_*)

    // reorder visual lines within and across reading blocks

    // val alreadySeen: mutable.Set[Int@@ComponentID] = mutable.Set.empty

    val allPageLines: mutable.ArrayBuffer[Component] = mutable.ArrayBuffer.empty

    pageIndex.getComponentsWithLabel(LB.VisualLine)
      .foreach{ a =>
        // println(s" has VisualLine label ${a}")
        if (a.hasLabel(LB.Canonical)) {
          // println("adding tmp label")
          pageIndex.addLabel(a, LB.Tmp)
        }
      }

    for {
      readingBlock <-  pageIndex.getOrdering(LB.ReadingBlocks)
      vlineRoots   <- Seq(pageIndex.rtreeSearchHasAllLabels(readingBlock.bounds(), LB.VisualLine, LB.Canonical, LB.Tmp)) // TODO this search picks up some lines multiple times
      if vlineRoots.nonEmpty

      // _             = allPageLines.clear()

      sortedLines   = vlineRoots.sortBy(_.bounds.bottom)
      vlineCluster   = pageIndex.addCluster(LB.ReadingBlockLines, sortedLines)

      // Tie together the readingBlock and the canonicalReadingBlockLine
      // _ = pageIndex.addCluster(LB.HasVisualLines, Seq(readingBlock, canonicalReadingBlockLine))
      _                = pageIndex.addRelation(readingBlock, LB.HasVisualLines, vlineCluster)
      vlineClusterTmp  = pageIndex.getRelations(readingBlock, LB.HasVisualLines)
      // _ = println(s"  add/get relation: ")
      // _ = println(s"    ${vlineCluster}")
      // _ = println(s"    ${vlineClusterTmp}")

      line         <- sortedLines
    }  {
      pageIndex.removeLabel(line, LB.Tmp)
      // if (!alreadySeen.contains(line.id)) {
      //   alreadySeen.add(line.id)
      //   lines.append(line)
      // }
      allPageLines.append(line)
    }

    if (allPageLines.nonEmpty) {
      // println(s"clustering...")
      val _ = pageIndex.addCluster(LB.PageLines, allPageLines)
    }
  }


  def findVisualLines(orderedTextBlocks: Seq[LTBounds]): Unit = {
    tracer.enter()
    // val gifBuilder = vis.gifBuilder("findVisualLines", 500.millis)

    pageIndex.getComponentsWithLabel(LB.PageAtom)
      .foreach{ a => pageIndex.addLabel(a, LB.PageAtomTmp) }

    // gifBuilder.addFrame("Starting", LB.PageAtomTmp)

    for { textBlock <- orderedTextBlocks } {
      // gifBuilder.indicate("Examining Block", textBlock, LB.PageAtomTmp)
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
            // gifBuilder.indicate(s"Processing Hash-Line ${index} of ${sortedHashedLinesWithChars.length}", hashLineCC.bounds(), LB.PageAtomTmp)
            val extendedLineRegion = hashLineCC.bounds.withinRegion(textBlock)
              .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
              .getOrElse { hashLineCC.bounds }
            // Progressively scan from center out to find super/subs
            val height = extendedLineRegion.height
            val top = extendedLineRegion.top

            val centerLine = extendedLineRegion
              .copy(height = height/2, top=top+height/4)


            // Now find all chars in queryRegion and string them together into a single visual line
            val visualLineAtoms = pageIndex.rtreeSearchHasLabel(centerLine, LB.PageAtomTmp)

            if (visualLineAtoms.nonEmpty) {
              val xSortedAtoms = visualLineAtoms.sortBy(_.bounds.left).toSeq

              val visualLineBBox = xSortedAtoms.map(_.bounds).reduce { (c1, c2) => c1 union c2 }

              // gifBuilder.indicate("Found VLine Atoms", visualLineBBox, LB.PageAtomTmp)

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

    // gifBuilder.finish()

    tracer.exit()
  }

  def findModalBoundingRect(visualLineCC: Component, visualLineAtoms: Seq[Component]): LTBounds = {
    val visualLineBounds = visualLineCC.bounds()

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

    // gifBuilder.indicate("Modal-base/top VisualLine", visualLineModalBounds)

    visualLineModalBounds
  }



  def textRowFromComponents(visualLineCC: Component, visualLineAtoms: Seq[Component])
    (implicit gifBuilder: GifBuilder): TextGrid.Row = {


    val visualLineModalCC = pageIndex.getRelations(visualLineCC, LB.VisualLineModal).head.head
    val visualLineModalBounds = visualLineModalCC.bounds()

    val (topIntersects, bottomIntersects) = findLineAtomScriptPositions(visualLineCC, visualLineAtoms)


    new TextGrid.MutableRow { self =>
      val init = visualLineAtoms.map{
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


            tracer {

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

        case c@ RegionComponent(id, roleLabel, pageRegion, maybeText) =>
          // TODO this is skipping over text represented as paths (but I have to figure out sup/sub script handling to make it work)
          Seq()

      }

      cells.appendAll(init.flatten)
    }
  }


  def labelSuperAndSubscripts(): Unit = {

  }

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

  // Group line atoms into center/sub/superscript bins
  def findLineAtomScriptPositions(visualLineCC: Component, visualLineAtoms: Seq[Component])
    (implicit gifBuilder: GifBuilder): (Seq[Int@@ComponentID], Seq[Int@@ComponentID]) = {

    val visualLineBounds = visualLineCC.bounds()

    // top 1/3  & bottom 1/3 ==> centered

    val slices = visualLineBounds.sliceHorizontal(3)
    val Seq(topSlice, _, bottomSlice) = slices

    val topIntersections = pageIndex.rtreeSearchHasLabel(topSlice, LB.PageAtomGrp)
    val bottomIntersections = pageIndex.rtreeSearchHasLabel(bottomSlice, LB.PageAtomGrp)
    // val middleIntersections = pageIndex.rtreeSearchHasLabel(middleSlice, LB.PageAtomGrp)

    val topIntersects = topIntersections.map(_.id)
    val bottomIntersects = bottomIntersections.map(_.id)

    // gifBuilder.indicate(s"Top intersection Line", topLine.bounds(), LB.PageAtomGrp)
    // gifBuilder.indicate(s"Bottom intersection Line", bottomLine.bounds(), LB.PageAtomGrp)
    gifBuilder.indicate(s"Top Atoms", topIntersections.map(_.bounds()), LB.PageAtomGrp)
    gifBuilder.indicate(s"Bottom Atoms", bottomIntersections.map(_.bounds()), LB.PageAtomGrp)



    tracer {

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

    }

    (topIntersects, bottomIntersects)
  }


  def createTextRowFromVisualLine(visualLineCC: Component, visualLineAtoms: Seq[Component]): Unit = {
    tracer.enter()

    val visualLineBounds = visualLineCC.bounds()

    implicit val gifBuilder = vis.gifBuilder(
      s"createTextRowsFromVisualLines-${visualLineBounds.left}-${visualLineBounds.bottom}",
      1.seconds
    )


    gifBuilder.indicate(
      s"Text From VisualLine ${visualLineBounds.left}-${visualLineBounds.bottom}",
      visualLineBounds, LB.PageAtomGrp, LB.PageAtomTmp
    )

    if (visualLineAtoms.nonEmpty) {
      // Associate visualLine bounds (modal, normal) w/visual line cluster
      val visualLineModalBounds = findModalBoundingRect(visualLineCC, visualLineAtoms)
      val visualLineModalCC = labelRegion(visualLineModalBounds, LB.VisualLineModal)
      val visualLineClusterCC = pageIndex.addCluster(LB.VisualLine, visualLineAtoms)

      pageIndex.addRelation(visualLineCC, LB.VisualLineModal, visualLineModalCC)

      gifBuilder.indicate(s"VisualLine Bounds", visualLineBounds)
      gifBuilder.indicate(s"ModalVisualLine Bounds", visualLineModalBounds)

      val textRow = textRowFromComponents(visualLineCC, visualLineAtoms)

      tracer {
        println(dbgGrid.toBox().transpose())
      }

      // Convert consecutive sup/sub to single label
      def relabel(c: TextGrid.Cursor, l: Label): Unit = {
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
          cursor.foreachC { c =>
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




      pageIndex.setComponentText(visualLineClusterCC, LB.VisualLine, supSubLabeledRow)

      val textReflow = convertTextRowToTextReflow(supSubLabeledRow)

      docStore.labelRegions(LB.VisualLine, Seq(visualLineCC.pageRegion)).foreach { zoneId =>
        docStore.setTextReflowForZone(zoneId, textReflow)
      }
    }

    gifBuilder.finish()

    tracer.exit()
  }


  def convertTextRowToTextReflow(textRow: TextGrid.Row): TextReflow = {

    val textReflowAtoms: Seq[TextReflow] = textRow.cells.map{ _ match {
      case  TextGrid.LeftExpansionCell(char, root)  => Some(insert(char.toString()))
      case  TextGrid.RightExpansionCell(char, root) => Some(insert(char.toString()))
      case  TextGrid.LeftInsertCell(char, _)      => Some(insert(char.toString()))
      case  TextGrid.RightInsertCell(char, loc@_)     => Some(insert(char.toString()))

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
    tracer.enter()

    charBoxes
      .groupBy{ _.bounds.bottom.unwrap }
      .toSeq
      .map { case (bottomY, charBoxes) =>
        charBoxes.sortBy(_.bounds.left)
      }.foreach{ lineBin =>
        mpageIndex.labelRegion(lineBin, LB.LineByHash)
      }

    tracer.checkpoint("visualize hash-line bins", pageIndex)
    // tracer.checkpoint("visualize hash-line bins")

    tracer.exit()
  }

  def findReadingOrder(initRegion: LTBounds): Seq[LTBounds] = {

    val maybeCol = pageIndex.rtreeSearchOverlapping(initRegion, LB.WhitespaceCol)
      .sortBy(cc => (cc.bounds.top, cc.bounds.left))
      .headOption

    maybeCol.map{ wsCol =>
      val colBounds = wsCol.bounds.withinRegion(initRegion)
      val upperRegion = colBounds.adjacentRegions(Dir.TopLeft, Dir.Top, Dir.TopRight)
      val lowerRegion = colBounds.adjacentRegions(Dir.BottomLeft, Dir.Bottom, Dir.BottomRight)

      val leftRegion = colBounds.adjacentRegion(Dir.Left)
      val rightRegion = colBounds.adjacentRegion(Dir.Right)

      val rs = Seq(leftRegion, rightRegion, lowerRegion)
        .flatten.flatMap{ r =>
          findReadingOrder(r)
        }

      upperRegion.map( _ +: rs ).getOrElse(rs)
    } getOrElse { Seq(initRegion) }
  }

  def splitLinesWithOverlaps(): Unit = {
    for {
      cc <- pageIndex.getComponentsWithLabel(LB.LineByHash)
    } {
      // Split up lines into strictly non-overlapping regions
      val intersects = pageIndex.rtreeSearch(cc.bounds, LB.LineByHash)


      if (intersects.length > 1) {
        val totalBounds = intersects.map(_.bounds).reduce(_ union _)
        val charsInRegion = pageIndex.rtreeSearch(totalBounds, LB.PageAtom)
        // Remove the LineByHash regions
        // iterate over chars left-to-right and group them into non-overlaps and overlaps
        val allChars = charsInRegion.sortBy(_.bounds.left)

        allChars.groupByPairs { (c1, c2) =>
          c1.bounds.bottom == c2.bounds.bottom
        }.foreach{ ccs =>
          mpageIndex.labelRegion(ccs, LB.LineByHash)
        }

        intersects.foreach { cc =>
          pageIndex.removeComponent(cc)
        }
      }
    }
  }

  def splitLinesOnWhitespaceColumns(): Unit = {

    for {
      colRegion <- pageIndex.getComponentsWithLabel(LB.WhitespaceCol)
      intersectedLine <- pageIndex.rtreeSearch(colRegion.bounds, LB.LineByHash)
    } {

      val charsInRegion = pageIndex.rtreeSearch(intersectedLine.bounds, LB.PageAtom)
      val allChars = charsInRegion.sortBy(_.bounds.left)

      val (leftSplit, rightSplit) = allChars.span(_.bounds.left < colRegion.bounds.left)

      mpageIndex.labelRegion(leftSplit, LB.LineByHash)
      mpageIndex.labelRegion(rightSplit, LB.LineByHash)

      pageIndex.removeComponent(intersectedLine)
    }

  }

}
