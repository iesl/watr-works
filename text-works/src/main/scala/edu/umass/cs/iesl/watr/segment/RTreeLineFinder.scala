package edu.umass.cs.iesl.watr
package segment

import spindex._

import watrmarks.{StandardLabels => LB, _}

import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
import utils.EnrichNumerics._

import textreflow.data._
// import scala.concurrent.duration._
import textgrid._
import textboxing.{TextBoxing => TB}, TB._

import scala.collection.mutable
import utils.SlicingAndDicing._
// import edu.umass.cs.iesl.watr.tracing.VisualTracer

import org.dianahep.{histogrammar => HST}
import ammonite.{ops => fs}
// import org.dianahep.histogrammar.ascii._
import tracemacros.VisualTraceLevel



class LineFinder(
  override val mpageIndex: MultiPageIndex,
  override val pageId: Int@@PageID,
  override val pageNum: Int@@PageNum
) extends SegmentationCommons with PageLevelFunctions with tracing.VisualTracer {

  val pageIndex = mpageIndex.getPageIndex(pageNum)
  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

  val segvisRootPath = fs.pwd / s"${stableId}-segs.d"

  def visualLogger(name: String) = {
    new VisualLogger(name, pageIndex, segvisRootPath)
  }

  def determineLines(): Unit = {
    val components = mpageIndex.getPageAtoms(pageNum)

    // def stdLabels(lls: Label*) = List(
    //   LB.Image, LB.HLinePath, LB.VLinePath, LB.LinePath, LB.WhitespaceCol, LB.ReadingBlock, LB.VisualLineModal
    // ) ++ lls.toList

    approximateLineBins(components)

    // vis.writeRTreeImage("01-lineHashing", LB.LineByHash, stdLabels():_*)

    splitLinesWithOverlaps()

    // vis.writeRTreeImage("02-splitLineHashing", LB.LineByHash, stdLabels():_*)

    findCandidateWhitespaceCols(components)

    // vis.writeRTreeImage("03-colCandidates", LB.WhitespaceColCandidate, LB.LineByHash)

    // vis.writeRTreeImage("03.1-lineByHash", LB.LineByHash)

    combineCandidateWhitespaceCols()

    // vis.writeRTreeImage("04-colsCombined", LB.WhitespaceCol, LB.LineByHash, LB.WhitespaceColCandidate)

    splitLinesOnWhitespaceColumns()

    // vis.writeRTreeImage("05-LinesSplitByCols", LB.LineByHash, LB.WhitespaceCol)

    // val svgVis = traced[VisualLogger] {  visualLogger("ShowReadingOrder") }

    val svgVis = visualLogger("ShowReadingOrder")
    val orderedRegions = findReadingOrder(pageGeometry)(svgVis)

    tracer.ifTrace(VisualTraceLevel.AccumLogs) {
      svgVis.writeLogs()
    }

    val readingBlocks = orderedRegions.map { labelRegion(_, LB.ReadingBlock) }

    pageIndex.setOrdering(LB.ReadingBlocks, readingBlocks)
    // val tmpReadingBlocks = pageIndex.getOrdering(LB.ReadingBlocks)
    // assert(tmpReadingBlocks.map(_.id).toSet == readingBlocks.map(_.id).toSet)

    // pageIndex.addCluster(LB.ReadingBlocks, readingBlocks)

    // vis.indicateRegions("06-ReadingBlockOrder", readingBlocks.map(_.bounds()))

    // TODO extract path objects in groups, rather than singly, to avoid trying to rewrite large drawn shapes
    rewritePathObjects(orderedRegions)

    // pageIndex.reportClusters()
    // println()

    findVisualLines(orderedRegions)

    // vis.writeRTreeImage("06-VisualLines", LB.VisualLine)

    setVisualLineOrdering()



    // Group visual lines into text blocks, s.t. each each block is semantically meaningful unit, e.g., part of a paragraph, a chart/table/figure+caption, or footnote


    // Group text blocks into paragraphs (within single page)


  }


  def setVisualLineOrdering(): Seq[Component] = {
    tracer.enter()
    // reorder visual lines within and across reading blocks

    val allPageLines: mutable.ArrayBuffer[Component] = mutable.ArrayBuffer.empty

    val vlineClusterRepLabel = LB.VisualLine.qualifiedAs("cluster").qualifiedAs("rep")

    pageIndex.getComponentsWithLabel(vlineClusterRepLabel)
      .foreach{ a =>
        pageIndex.addLabel(a, LB.Tmp)
      }

    for {
      readingBlock <-  pageIndex.getOrdering(LB.ReadingBlocks)
      vlineRoots   <- Seq(pageIndex.rtreeSearchHasAllLabels(readingBlock.bounds(), vlineClusterRepLabel, LB.Tmp)) // TODO this search picks up some lines multiple times
      if vlineRoots.nonEmpty


      sortedLines   = vlineRoots.sortBy(_.bounds.bottom)
      vlineCluster   = pageIndex.addCluster(LB.ReadingBlockLines, sortedLines)

      // Tie together the readingBlock and the canonicalReadingBlockLine
      _                = pageIndex.addRelation(readingBlock, LB.HasVisualLines, vlineCluster)
      vlineClusterTmp  = pageIndex.getRelations(readingBlock, LB.HasVisualLines)

      line         <- sortedLines
    }  {
      pageIndex.removeLabel(line, LB.Tmp)
      allPageLines.append(line)
    }

    if (allPageLines.nonEmpty) {
      pageIndex.setOrdering(LB.PageLines, allPageLines)
    }

    tracer.exit()
    allPageLines
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


  def textRowFromComponents(visualLineClusterCC: Component, visualLineAtoms: Seq[Component]): TextGrid.Row = {


    val visualLineModalCC = pageIndex.getRelations(visualLineClusterCC, LB.VisualLineModal).head.head
    val visualLineCC = pageIndex.getRelations(visualLineClusterCC, LB.VisualLine).head.head

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
              // gifBuilder.indicate(s"SuperScript", cc.bounds(), LB.PageAtomGrp)
              allCells.foreach{ _.addLabel(LB.Sup) }
            } else if (!intersectsTop && intersectsBottom) {
              // gifBuilder.indicate(s"SubScript", cc.bounds(), LB.PageAtomGrp)
              allCells.foreach{ _.addLabel(LB.Sub) }
            } else {
              // gifBuilder.indicate(s"???Script", cc.bounds(), LB.PageAtomGrp)
            }


            tracer {

              val pinChars = cell.pins.toList.map(_.pinChar).sorted.mkString

              dbgGrid = dbgGrid.addRow(
                "-",
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


  var dbgGrid = TB.Grid.widthAligned()

  // Group line atoms into center/sub/superscript bins
  def findLineAtomScriptPositions(visualLineCC: Component, visualLineAtoms: Seq[Component]): (Seq[Int@@ComponentID], Seq[Int@@ComponentID]) = {

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
    // gifBuilder.indicate(s"Top Atoms", topIntersections.map(_.bounds()), LB.PageAtomGrp)
    // gifBuilder.indicate(s"Bottom Atoms", bottomIntersections.map(_.bounds()), LB.PageAtomGrp)



    tracer {
      dbgGrid = TB.Grid.widthAligned(
        (1, AlignLeft),  // join indicator
        (2, AlignLeft),  // line text (char(s))
        (6, AlignRight), // char.top
        (1, AlignLeft),  // space
        (6, AlignRight), // char.bottom
        (1, AlignLeft),  // space
        (6, AlignLeft), // labels
        (1, AlignLeft)  // space
      )

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

    // val visualLineBounds = visualLineCC.bounds()

    // implicit val gifBuilder = vis.gifBuilder(
    //   s"createTextRowsFromVisualLines-${visualLineBounds.left}-${visualLineBounds.bottom}",
    //   1.seconds
    // )
    // gifBuilder.indicate(
    //   s"Text From VisualLine ${visualLineBounds.left}-${visualLineBounds.bottom}",
    //   visualLineBounds, LB.PageAtomGrp, LB.PageAtomTmp
    // )

    if (visualLineAtoms.nonEmpty) {
      // Associate visualLine bounds (modal, normal) w/visual line cluster
      val visualLineModalBounds = findModalBoundingRect(visualLineCC, visualLineAtoms)
      val visualLineModalCC = labelRegion(visualLineModalBounds, LB.VisualLineModal)
      val visualLineClusterCC = pageIndex.addCluster(LB.VisualLine, visualLineAtoms)

      // pageIndex.addRelation(visualLineCC, LB.VisualLineModal, visualLineModalCC)
      pageIndex.addRelation(visualLineClusterCC, LB.VisualLineModal, visualLineModalCC)
      pageIndex.addRelation(visualLineClusterCC, LB.VisualLine, visualLineCC)

      // gifBuilder.indicate(s"VisualLine Bounds", visualLineBounds)
      // gifBuilder.indicate(s"ModalVisualLine Bounds", visualLineModalBounds)

      val textRow = textRowFromComponents(visualLineClusterCC, visualLineAtoms)

      // tracer {
      //   println(dbgGrid.toBox().transpose())
      // }

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




      pageIndex.setComponentText(visualLineClusterCC, LB.VisualLine, supSubLabeledRow)

      val textReflow = convertTextRowToTextReflow(supSubLabeledRow)

      docStore.labelRegions(LB.VisualLine, Seq(visualLineCC.pageRegion)).foreach { zoneId =>
        docStore.setTextReflowForZone(zoneId, textReflow)
      }
    }

    // gifBuilder.finish()

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

    tracer.exit()
  }

  def findReadingOrder(initRegion: LTBounds)(svgVis: VisualLogger, level: Int=0): Seq[LTBounds] = {

    // svgVis.logRegions("All Columns", bboxes: Seq[LTBounds], lineColor: Color, fillColor: Color)(s"(${level}) Init Region ${initRegion}", initRegion, Colors.Blue, Colors.White)

    val maybeCol = pageIndex.rtreeSearchOverlapping(initRegion, LB.WhitespaceCol)
      .sortBy(cc => (cc.bounds.top, cc.bounds.left))
      .headOption

    // Flash all Whitespace Cols found:...

    maybeCol.map{ wsCol =>
      val colBounds = wsCol.bounds.withinRegion(initRegion)
      val upperRegion = colBounds.adjacentRegions(Dir.TopLeft, Dir.Top, Dir.TopRight)
      val lowerRegion = colBounds.adjacentRegions(Dir.BottomLeft, Dir.Bottom, Dir.BottomRight)

      val leftRegion = colBounds.adjacentRegion(Dir.Left)
      val rightRegion = colBounds.adjacentRegion(Dir.Right)

      val adjacentRegions = List(upperRegion, leftRegion, rightRegion, lowerRegion).flatten

      tracer.ifTrace(tracemacros.VisualTraceLevel.AccumLogs) {
        svgVis.logRegions(
          s"WS.Col + Adjacent Regions ($level)",
          wsCol.bounds() :: adjacentRegions
        )

      }

      // svgVis.indicateRegion(s"(${level}) WS.Column ${wsCol}", wsCol.bounds(), Colors.Black, Colors.Black)
      // upperRegion.foreach { r => svgVis.indicateRegion(s"(${level}) Upper Adj Region ${r}", r, Colors.Green, Colors.Yellow) }
      // upperRegion.foreach { r => svgVis.logRegions(s"(${level}) Upper Adj Region ${r}", r, Colors.Green, Colors.Yellow) }
      // leftRegion.foreach  { r => svgVis.indicateRegion(s"(${level}) Left  Adj Region ${r}", r, Colors.Green, Colors.AliceBlue) }
      // rightRegion.foreach { r => svgVis.indicateRegion(s"(${level}) Right Adj Region ${r}", r, Colors.Green, Colors.Brown) }
      // lowerRegion.foreach { r => svgVis.indicateRegion(s"(${level}) Lower Adj Region ${r}", r, Colors.Green, Colors.Cyan) }

      val rs = Seq(leftRegion, rightRegion, lowerRegion)
        .flatten.flatMap{ r =>
          findReadingOrder(r)(svgVis, level+1)
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

      val (leftSplit, rightSplit) = allChars.span(_.bounds.left <= colRegion.bounds.left)

      mpageIndex.labelRegion(leftSplit, LB.LineByHash)
      mpageIndex.labelRegion(rightSplit, LB.LineByHash)

      pageIndex.removeComponent(intersectedLine)
    }

  }



  def pairwiseItemDistances(sortedLineCCs: Seq[PageItem]): Seq[FloatExact] = {
    val cpairs = sortedLineCCs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => (c2.bbox.left - c1.bbox.right)
      case _  => 0d.toFloatExact()
    })

    dists :+ 0d.toFloatExact()
  }

  def guessWordbreakWhitespaceThreshold(sortedLineCCs: Seq[PageItem]): FloatExact = {
    tracer.enter()

    val charDists = pairwiseItemDistances(sortedLineCCs)
      .toSet.toSeq

    val charWidths = sortedLineCCs.map(_.bbox.width)

    val widestChar = charWidths.max.asDouble()
    val narrowestChar = charWidths.min.asDouble()
    val avgCharWidth = (widestChar + narrowestChar) / 2

    // Don't  accept a space wider than (some magic number)*the widest char?
    val saneCharDists = charDists
      .filter(_ < widestChar*2 )
      .filterNot(_.unwrap == 0)
      .map(_.asDouble())


    val noSplitThreshold = widestChar
    val threshold = if (saneCharDists.length <= 1 || sortedLineCCs.length <= 1) {
      // If there is only 1 distance between chars, the line is only 1 word (no word breaks)
      noSplitThreshold
    } else {
      val averageDist = saneCharDists.sum / saneCharDists.length

      val charDistSpread = saneCharDists.max - saneCharDists.min
      if (charDistSpread < avgCharWidth / 4) {
        noSplitThreshold
      } else {

        // val (littleDists, bigDists) = saneCharDists.sorted.span(_ < averageDist)
        averageDist
      }
    }
    tracer {
      println(
        s"""|guessWordbreakWhitespaceThreshold
            | Char Dists      = ${charDists.map(_.pp).mkString(", ")}
            | Sane Dists      = ${saneCharDists.map(_.pp).mkString(", ")}
            | Wide/Nar/Avg Ch = ${widestChar.pp}/${narrowestChar.pp}/${avgCharWidth.pp}
            | Split threshold = ${threshold.pp}
            |""".stripMargin.mbox
      )
    }

    tracer.exit()

    threshold.toFloatExact
  }






  def insertSpacesInRow(textRow: TextGrid.Row): TextGrid.Row = {
    tracer.enter()
    val lineCCs = textRow.cells.collect{
      case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
        headItem
    }

    val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

    var spacingDbgGrid = Grid.widthAligned(
      (1, AlignLeft),  // join indicator
      (2, AlignLeft),  // char(s)
      (6, AlignRight), // char.left
      (1, AlignLeft),  // space
      (6, AlignRight), // char.right
      (1, AlignLeft),  // space
      (6, AlignRight), // c1 - c2 dist
      (1, AlignLeft),  // space
      (5, AlignRight)  // char.width
    )
    tracer {
      spacingDbgGrid = spacingDbgGrid.addRow(
        "J",
        "",
        "LEFT||",
        "",
        "RIGHT|",
        "",
        "DIST||",
        "",
        "WIDTH"
      )
      spacingDbgGrid = spacingDbgGrid.addRow(" ", "  ", "      ", " ", "      ", " ", "      ", " ", "     ")
    }

    val res = textRow.toCursor().map{ cursor =>
      val finalRow = cursor.unfoldCursorToRow { nextCursor =>

        val wordWin = nextCursor.toWindow.slurpRight{ case (win, cell) =>

          val pairwiseDist = cell.pageRegion.bbox.left - win.last.pageRegion.bbox.right
          val willGroup = pairwiseDist < splitValue

           tracer {
             val c1 = win.last
             spacingDbgGrid = spacingDbgGrid.addRow(
               if(willGroup) "_" else "$",
               c1.char.toString,
               c1.pageRegion.bbox.left.pp,
               "~",
               c1.pageRegion.bbox.right.pp,
               "~",
               pairwiseDist.pp,
               "~",
               c1.pageRegion.bbox.width.pp
             )
           }


          willGroup
        }

        if (!wordWin.atEnd) {
          tracer {
            spacingDbgGrid = spacingDbgGrid.addRow(
              " ",
              " ",
              " ",
              " ",
              " ",
              " ",
              " ",
              " ",
              " "
            )
          }

          wordWin.extendRight(' ').toLastCursor.some
        } else None


      }

      // tracer {
      //   println(spacingDbgGrid.toBox().transpose())
      // }

      finalRow

    } getOrElse { textRow }

    tracer.exit()

    res
  }

  def findCandidateWhitespaceCols(components: Seq[AtomicComponent]): Unit = {

    val cols = findLeftAlignedCharCols(components)

    cols.foreach { colRegion =>
      val colBounds = colRegion.bounds
      val startingRegion = LTBounds(
        left   = colBounds.left-0.1d,
        top    = colBounds.top,
        width  = 0.01.toFloatExact(),
        height = colBounds.height
      )

      growToMaxEmptySpace(startingRegion)
        .foreach{ emptyRegion =>
          val colIsWideEnough = emptyRegion.width > 4.0d

          if (colIsWideEnough) {
            labelRegion(emptyRegion, LB.WhitespaceColCandidate)
          }
        }

    }
  }
  def findLeftAlignedCharCols(
    components: Seq[AtomicComponent]
  ): Seq[RegionComponent] = {
    import HST._
    val componentLefts = HST.SparselyBin.ing(1.0, {x: AtomicComponent => x.bounds.left.asDouble()} named "char-lefts")

    components.foreach { componentLefts.fill(_) }

    // Construct a horizontal query, looking to boost scores of "runs" of consecutive left-x-value
    val queryBoxes = componentLefts.bins.toList
      .sortBy { case (bin, counting) => counting.entries }
      .reverse.take(10) //  only consider the 10 tallest cols
      .map{ case (bin, counting) =>
        val bw = componentLefts.binWidth

        LTBounds.Doubles(
          left   = bw * bin,
          top    = 0d,
          width  = bw,
          height = pageGeometry.height.asDouble()
        )
      }

    val res: List[Option[RegionComponent]] =
      queryBoxes.flatMap { query =>
        val intersects = pageIndex.rtreeSearch(query, LB.PageAtom)

        val consecutiveLeftAlignedCharCols =
          intersects.sortBy(_.bounds.bottom)
            .groupByPairs((c1, c2) => c1.bounds.bottom == c2.bounds.bottom)
            .map(_.sortBy(_.bounds.left).head)
            .groupByPairs((c1, c2) => c1.bounds.left == c2.bounds.left)
            .filter{ groups => groups.length > 1 }

        consecutiveLeftAlignedCharCols
          .map{ ccs => mpageIndex.labelRegion(ccs, LB.LeftAlignedCharCol).map(_._1) }


      }

    res.flatten
  }


}




  // def guessWordbreakWhitespaceThresholdOld(sortedLineCCs: Seq[PageItem]): FloatExact = {
  //   tracer.enter()

  //   def determineSpacings(): Seq[FloatExact] = {
  //     tracer.enter()
  //     // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)
  //     def pairwiseSpaceWidths(): Seq[FloatExact] = {
  //       val cpairs = sortedLineCCs.sliding(2).toList

  //       val dists = cpairs.map({
  //         case Seq(c1, c2)  => (c2.bbox.left - c1.bbox.right)
  //         case _  => 0d.toFloatExact()
  //       })

  //       dists :+ 0d.toFloatExact()
  //     }

  //     val dists = pairwiseSpaceWidths()

  //     val mostFrequentDists = dists.groupBy(x => x)
  //       .mapValues { _.length }
  //       .toList
  //       .sortBy(_._2).reverse

  //     tracer.exit()
  //     mostFrequentDists.map(_._1)
  //   }



  //   val charDists = determineSpacings()

  //   val charWidths = sortedLineCCs.map(_.bbox.width)
  //   val widestChar = charWidths.max

  //   // Don't  accept a space wider than (some magic number)*the widest char?
  //   val saneCharDists = charDists
  //     .filter(_ < widestChar*2 )
  //     .filterNot(_.unwrap == 0)

  //   def resolution =  0.3d

  //   // Try to divide the list of char dists into 2 groups, small gap and large gap:
  //   // See if we can divide our histogram values by some value > 2*histResolution
  //   val distGroups = saneCharDists.groupByPairs( { (c1, c2) =>
  //     math.abs((c2 - c1).asDouble()) < resolution
  //   })


  //   val threshold = if (saneCharDists.length == 1) {
  //     // If there is only 1 distance, the line is only 1 word (no word breaks)
  //     1.0d.toFloatExact()
  //   } else if (distGroups.length == 2) {
  //     val d1 = distGroups(0).last
  //     val d2 = distGroups(1).head

  //     (d1+d2) / 2
  //   } else if (saneCharDists.length >= 2) {
  //     // Take most common space to be char space within words
  //     val modalLittleGap = saneCharDists.head
  //     // The next most frequent space (that is larger than the within-word space) is assumed to be the space between words:
  //     val modalBigGap = saneCharDists
  //       .drop(1)
  //       .filter(_ > modalLittleGap)
  //       .headOption.getOrElse(modalLittleGap)

  //     (modalBigGap+modalLittleGap)/2
  //   } else {
  //     // Fallback to using unfiltered char dists
  //     val modalLittleGap = charDists.head
  //     // The next most frequent space (that is larger than the within-word space) is assumed to be the space between words:
  //     val modalBigGap = charDists
  //       .drop(1)
  //       .filter(_ > modalLittleGap)
  //       .headOption.getOrElse(modalLittleGap)

  //     (modalBigGap*2+modalLittleGap)/3
  //   }

  //   tracer {
  //     println(
  //       s"""|guessWordbreakWhitespaceThreshold
  //           | Char Dists      = ${charDists.map(_.pp).mkString(", ")}
  //           | Sane Dists      = ${saneCharDists.map(_.pp).mkString(", ")}
  //           | Widest Char     = ${widestChar.pp}
  //           | Split threshold = ${threshold.pp}
  //           |""".stripMargin.mbox
  //     )
  //   }

  //   tracer.exit()

  //   threshold
  // }
