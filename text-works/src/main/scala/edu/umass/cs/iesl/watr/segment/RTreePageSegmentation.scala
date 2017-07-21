package edu.umass.cs.iesl.watr
package segment

import spindex._
import utils.SlicingAndDicing._

import ammonite.{ops => fs}, fs._
import watrmarks.{StandardLabels => LB, _}

import geometry._
import geometry.syntax._
import org.dianahep.{histogrammar => HST}
// import org.dianahep.histogrammar.ascii._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
import images.{ImageManipulation => IM}

class RTreePageSegmentation(
  val mpageIndex: MultiPageIndex
) {

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

  lazy val pageIdMap: Map[Int@@PageID, Int@@PageNum] =
    docStore.getPages(docId).zipWithIndex.map{
      case (pageId, pageNum) => (pageId, PageNum(pageNum))
    }.toMap

  def runPageSegmentation(): Unit = {
    runLineDetermination()
  }

  def rtreeSearch(
    pageId: Int@@PageID, queryRegion: LTBounds,
    label: Label, labels: Label*
  ): Seq[Component] = {
    val pageNum = pageIdMap(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex
    val lbls = label :: labels.toList

    rTreeIndex.search(queryRegion, {cc =>
      lbls.contains(cc.roleLabel)
    })
  }

  def labelRegion(pageId: Int@@PageID, bbox: LTBounds, label: Label): RegionComponent = {
    val regionId = docStore.addTargetRegion(pageId, bbox)
    val pageRegion = docStore.getTargetRegion(regionId).toPageRegion
    mpageIndex.createRegionComponent(pageRegion, label)
  }

  def runLineDetermination(): Unit = {
    val pageRegions = for {
      (pageId, pagenum) <- docStore.getPages(docId).zipWithIndex
    } yield {

      println(s"Page ${pagenum} id=${pageId}")
      runLineDeterminationOnPage(pageId, PageNum(pagenum))

      val pageGeometry = docStore.getPageGeometry(pageId)
      docStore.getTargetRegion(
        docStore.addTargetRegion(pageId, pageGeometry)
      ).toPageRegion()
    }

    docStore.labelRegions(LB.DocumentPages, pageRegions)
  }


  def writeRTreeImage(
    pageNum: Int@@PageNum,
    name: String,
    l0: Label,
    labels: Label*
  ): Unit = {
    import com.sksamuel.scrimage
    import scrimage._
    import X11Colorlist._
    val labelColors: Map[Label, Color] = {
      Map(
        (LB.VisualLine, X11Colorlist.Plum),
        (LB.PathBounds, X11Colorlist.RoyalBlue),
        (LB.HLinePath, X11Colorlist.Black),
        (LB.VLinePath, X11Colorlist.Black),
        (LB.Image, X11Colorlist.DarkCyan),
        (LBP.LineByHash, Firebrick3),
        (LBP.LeftAlignedCharCol, Blue),
        (LBP.WhitespaceColCandidate, Green),
        (LBP.WhitespaceCol, X11Colorlist.Peru),
        (LBP.Marked, MediumTurquoise)
      )
    }
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val pageBounds = pageIndex.getPageGeometry.bounds
    val pageCanvas = IM.createCanvas(pageBounds)
    val rTreeIndex = pageIndex.componentIndex
    val lbls = l0 :: labels.toList

    val overlays = rTreeIndex.getItems
      .filter { c =>
        lbls.contains(c.roleLabel)
      }
      .map { c => IM.ltBoundsToDrawables(c.bounds, pageIndex.getPageGeometry, pageBounds, labelColors(c.roleLabel) ) }

    val embossedCanvas = pageCanvas.draw(overlays.flatten)

    val bytes = embossedCanvas.image.bytes
    val outRelDir = RelPath(new java.io.File(s"${stableId}-segs.d"))
    val outDir = fs.pwd / outRelDir
    if (!fs.exists(outDir)) {
      fs.mkdir(outDir)
    }

    val outPath = fs.pwd / outRelDir / RelPath(new java.io.File(s"${name}.pg${pageNum}.png"))

    if (fs.exists(outPath)) {
      fs.rm(outPath)
    }
    fs.write(outPath, bytes)

  }

  def runLineDeterminationOnPage(pageId: Int@@PageID, pageNum: Int@@PageNum): Unit = {
    val atomicComponents = mpageIndex.getPageAtoms(pageNum)

    mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
      mpageIndex.labelRegion(Seq(imgCC), LB.Image)
    }

    determineLines(pageId, pageNum, atomicComponents)
  }


  def determineLines(
    pageId: Int@@PageID,
    pageNum: Int@@PageNum,
    components: Seq[AtomicComponent]
  ): Unit = {
    def stdLabels(lls: Label*) = List(
      LB.Image, LB.HLinePath, LB.VLinePath, LBP.WhitespaceCol
    ) ++ lls.toList

    // val interestingLabels = List(
    //   LBP.LineByHash, LB.Image, LBP.LeftAlignedCharCol, LBP.WhitespaceColCandidate,
    //   LB.PathBounds
    // )

    approximateLineBins(components)
    writeRTreeImage(pageNum, "01-lineHashing", LBP.LineByHash, stdLabels():_*)

    splitLinesWithOverlaps(pageId)

    writeRTreeImage(pageNum, "02-splitLineHashing", LBP.LineByHash, stdLabels():_*)

    findCandidateWhitespaceCols(pageId, components)

    writeRTreeImage(pageNum, "03-colCandidates", LBP.LineByHash, stdLabels(LBP.LeftAlignedCharCol, LBP.WhitespaceColCandidate):_*)

    combineCandidateWhitespaceCols(pageId)

    writeRTreeImage(pageNum, "04-colsCombined", LBP.LineByHash, stdLabels():_*)

    splitLinesOnWhitespaceColumns(pageNum)

    writeRTreeImage(pageNum, "05-LinesSplitByCols", LBP.LineByHash, stdLabels():_*)

    val textGridRows = findVisualLines(pageId)

    writeRTreeImage(pageNum, "06-VisualLines", LB.VisualLine, stdLabels():_*)

    insertSpaces(pageId, textGridRows)


    val pageGeometry = docStore.getPageGeometry(pageId)
    dividePageIntoBlocks(pageId, pageGeometry)

    // determine reading order for page lines
    // Find page-dividing hlines
    //   find h-lines that span page-width (no h text or col overlaps)
    //   divide page into top/bottom regions for each h-line divider
    // within h-divided sub-regions, split on ws cols



    // find line-joins
    // rewrite line-paths representing symbols into chars (eg, -, =, vinculum)
  }

  def guessWordbreakWhitespaceThreshold(sortedLineCCs: Seq[Component]): FloatExact = {
    // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)
    def pairwiseSpaceWidths(): Seq[FloatExact] = {
      val cpairs = sortedLineCCs.sliding(2).toList

      val dists = cpairs.map({
        case Seq(c1, c2)  => (c2.bounds.left - c1.bounds.right)
        case _  => 0d.toFloatExact()
      })

      dists :+ 0d.toFloatExact()
    }

    def determineSpacings(): Seq[FloatExact] = {
      val dists = pairwiseSpaceWidths()

      val mostFrequentDists = dists.groupBy(x => x)
        .mapValues { _.length }
        .toList
        .sortBy(_._2).reverse

      mostFrequentDists.map(_._1)
    }
    val charDists = determineSpacings()

    val charWidths = sortedLineCCs.map(_.bounds.width)
    val widestChar = charWidths.max

    // Don't  accept a space wider than (some magic number)*the widest char?
    val saneCharDists = charDists
      .filter(_ < widestChar*2 )
      .filter(_ == 0)

    def resolution =  0.3d

    // Try to divide the list of char dists into 2 groups, small gap and large gap:
    // See if we can divide our histogram values by some value > 2*histResolution
    val distGroups = saneCharDists.groupByPairs( { (c1, c2) =>
      math.abs((c2 - c1).asDouble()) < resolution
    })


    val threshold = if (saneCharDists.length == 1) {
      // If there is only 1 distance, the line is only 1 word (no word breaks)
      1.0d.toFloatExact()
    } else if (distGroups.length >= 2) {
      // vtrace.trace(message(""))
      val d1 = distGroups(0).last
      val d2 = distGroups(1).head

      (d1+d2) / 2
    } else if (saneCharDists.length >= 2) {
      // Take most common space to be char space within words
      val modalLittleGap = saneCharDists.head
      // The next most frequent space (that is larger than the within-word space) is assumed to be the space between words:
      val modalBigGap = saneCharDists
        .drop(1)
        .filter(_ > modalLittleGap)
        .headOption.getOrElse(modalLittleGap)

      (modalBigGap+modalLittleGap)/2
    } else {
      // Fallback to using unfiltered char dists
      val modalLittleGap = charDists.head
      // The next most frequent space (that is larger than the within-word space) is assumed to be the space between words:
      val modalBigGap = charDists
        .drop(1)
        .filter(_ > modalLittleGap)
        .headOption.getOrElse(modalLittleGap)

      (modalBigGap*2+modalLittleGap)/3
    }


    threshold
  }

  def insertSpaces(pageId: Int@@PageID, textGridRows: Seq[TextGrid.Row]): Unit = {
    textGridRows.map { textRow =>
      val lineCCs = textRow.cells.collect{
        case TextGrid.ComponentCell(cc) => cc
      }

      val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

      val maybeGroups = textRow.groupBy { (c1, c2) =>
        val pairwiseDist = c2.bounds.left - c1.bounds.right
        val sameGroup = pairwiseDist < splitValue
        sameGroup
      }

      maybeGroups.map { groupCursor =>

        val finalGroups = groupCursor.unfoldBy { group =>
          if (!group.atEnd) {
            val ins = group.focus.last.cloneCell()
            Some(group.insertRight(ins.copy(text=" ")))
          } else {
            Some(group)
          }
        }

        val finalRow = finalGroups.toRow
        val hb = lineCCs.head.bounds
        println(s"${hb}>    ${finalRow.toText}")
      }

    }
  }

  def findVisualLines(pageId: Int@@PageID): Seq[TextGrid.Row] = {

    val pageNum = pageIdMap(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex
    val pageGeometry = docStore.getPageGeometry(pageId)

    val linesWithCharCounts = for {
      hashedLines <- pageIndex.labelToComponents.get(LBP.LineByHash).toList
      hashedLine <- hashedLines
      hashedLineCC <- rTreeIndex.get(hashedLine.unwrap)
    } yield {
      val charsInRegion = rTreeIndex.search(hashedLineCC.bounds, {cc =>
        cc.roleLabel == LB.PageAtom
      })
      (hashedLineCC, charsInRegion.length)
    }

    val allTextRows = linesWithCharCounts
      .sortBy { case (_, count) => count }
      .reverse
      .map { case (hashLineRegion, count) =>
        // if not already examined:
        val alreadyProcessed = rTreeIndex.search(hashLineRegion.bounds, {_.roleLabel == LBP.Marked}).nonEmpty

        if (!alreadyProcessed) {


          // look left and right to find candidate line-parts to join into visual lines, respecting whitespace cols

          val leftAdjacentRegion = hashLineRegion.bounds.withinRegion(pageGeometry).adjacentRegion(Dir.Left)

          val leftEdge = leftAdjacentRegion.map { leftAdjacentBounds =>
            val colsToTheLeft = rtreeSearch(pageId, leftAdjacentBounds, LBP.WhitespaceCol)
              .map(_.bounds.right)

            (pageGeometry.left +: colsToTheLeft).max
          } getOrElse {  pageGeometry.left }

          val rightAdjacentRegion = hashLineRegion.bounds.withinRegion(pageGeometry).adjacentRegion(Dir.Right)

          val rightEdge = rightAdjacentRegion.map { rightAdjacentBounds =>
            val colsToTheRight = rtreeSearch(pageId, rightAdjacentBounds, LBP.WhitespaceCol).map(_.bounds.left)

            (pageGeometry.right +: colsToTheRight).min
          } getOrElse {  pageGeometry.right }

          val extendedLineRegion = hashLineRegion.bounds
            .setLeft(leftEdge)
            .setRight(rightEdge)

          // Now find all chars in queryRegion and string them together into a single visual line
          val visualLineAtoms = rtreeSearch(pageId, extendedLineRegion, LB.PageAtom)

          val topLine = extendedLineRegion.toLine(Dir.Top).translate(y=0.5)
          val bottomLine = extendedLineRegion.toLine(Dir.Bottom).translate(y = -0.5)

          // val centerLine = extendedLineRegion.toPoint(Dir.Left).lineTo(
          //   extendedLineRegion.toPoint(Dir.Right)
          // )
          val topIntersects = rTreeIndex.searchLine(topLine, {_.roleLabel == LB.PageAtom}).map(_.id)
          val bottomIntersects = rTreeIndex.searchLine(bottomLine, {_.roleLabel == LB.PageAtom}).map(_.id)
          // val centerIntersects = rTreeIndex.searchLine(centerLine, {_.roleLabel == LB.PageAtom}).map(_.id)


          val comps = visualLineAtoms.sortBy(_.bounds.left)
          val compBbox =  comps.map(_.bounds).reduce { (c1, c2) => c1 union c2 }

          val textRow = TextGrid.Row.fromComponents(comps)

          textRow.foreach{ _ match {
            case cell@ TextGrid.ComponentCell(cc) =>
              val intersectsTop = topIntersects.contains(cc.id)
              val intersectsBottom = bottomIntersects.contains(cc.id)
              // val intersectsCenter = centerIntersects.contains(cc.id)

              if (intersectsTop && !intersectsBottom) {
                cell.addLabel(LB.Sup)
              } else if (!intersectsTop && intersectsBottom) {
                cell.addLabel(LB.Sub)
              }

            case _ =>

          }}


          // val regionId = docStore.addTargetRegion(pageId, compBbox)
          // val targetRegion = docStore.getTargetRegion(regionId)
          // val pageRegion = PageRegion(targetRegion.page, targetRegion.bbox)
          // mpageIndex.createRegionComponent(pageRegion, LBP.Marked)
          // mpageIndex.createRegionComponent(pageRegion, LB.VisualLine)

          labelRegion(pageId, compBbox, LB.VisualLine)
          labelRegion(pageId, compBbox, LBP.Marked)



          Some(textRow)
        } else None
      }

    val justTextRows = allTextRows.flatten

    // Get rid of all the housekeeping "marked" regions
    pageIndex.labelToComponents.get(LBP.Marked).foreach { markedRegions =>
      markedRegions.foreach { ccId =>
        val cc = mpageIndex.getComponent(ccId, pageNum)
        mpageIndex.removeComponent(cc)
      }
    }
    justTextRows

  }




  // First approximation for text line-groups
  def approximateLineBins(charBoxes: Seq[AtomicComponent]): Unit = {
    charBoxes
      .groupBy{ _.bounds.bottom.unwrap }
      .toSeq
      .map { case (bottomY, charBoxes) =>
        charBoxes.sortBy(_.bounds.left)
      }.foreach{ lineBin =>
        mpageIndex.labelRegion(lineBin, LBP.LineByHash)
      }
  }


  def findLeftAlignedCharCols(
    pageId: Int@@PageID,
    components: Seq[AtomicComponent]
  ): Seq[RegionComponent] = {
    import HST._
    val componentLefts = HST.SparselyBin.ing(1.0, {x: AtomicComponent => x.bounds.left.asDouble()} named "char-lefts")

    components.foreach { componentLefts.fill(_) }

    val pageGeometry = docStore.getPageGeometry(pageId)

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
        val intersects = rtreeSearch(pageId, query, LB.PageAtom)

        val consecutiveLeftAlignedCharCols =
          intersects.sortBy(_.bounds.bottom)
            .groupByPairs((c1, c2) => c1.bounds.bottom == c2.bounds.bottom)
            .map(_.sortBy(_.bounds.left).head)
            .groupByPairs((c1, c2) => c1.bounds.left == c2.bounds.left)
            .filter{ groups => groups.length > 1 }

        consecutiveLeftAlignedCharCols
          .map{ ccs => mpageIndex.labelRegion(ccs, LBP.LeftAlignedCharCol).map(_._1) }


      }

    res.flatten
  }


  def findCandidateWhitespaceCols(
    pageId: Int@@PageID,
    components: Seq[AtomicComponent]
  ): Unit = {

    val cols = findLeftAlignedCharCols(pageId, components)

    cols.foreach { colRegion =>
      val colBounds = colRegion.bounds
      val startingRegion = LTBounds(
        left   = colBounds.left-0.1d,
        top    = colBounds.top,
        width  = 0.01.toFloatExact(),
        height = colBounds.height
      )

      growToMaxEmptySpace(pageId, startingRegion)
        .foreach{ emptyRegion =>
          val colIsWideEnough = emptyRegion.width > 4.0d

          if (colIsWideEnough) {
            labelRegion(pageId, emptyRegion, LBP.WhitespaceColCandidate)
          }
        }

    }
  }

  def combineCandidateWhitespaceCols(
    pageId: Int@@PageID
  ): Unit = {
    val pageNum = pageIdMap(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex
    val pageGeometry = docStore.getPageGeometry(pageId)

    // var foundOverlaps = false

    var candidates = pageIndex.labelToComponents.get(LBP.WhitespaceColCandidate).getOrElse { Seq() }

    while(candidates.nonEmpty) {
      val nextCandidate = candidates.head
      val candidate = rTreeIndex.get(nextCandidate.unwrap).get

      val overlaps = rtreeSearch(pageId, candidate.bounds, LBP.WhitespaceColCandidate)
      val totalOverlapArea = overlaps.map(_.bounds).reduce(_ union _)

      val commonArea = overlaps.map(_.bounds)
        .map{ _.withinRegion(pageGeometry).coveringRegion(Dir.Top, Dir.Bottom) }
        .foldLeft(totalOverlapArea){ case (acc, e) =>
          (acc intersection e).get
        }

      labelRegion(pageId, commonArea, LBP.WhitespaceCol)

      mpageIndex.removeComponent(candidate)

      overlaps.foreach{ c =>
        mpageIndex.removeComponent(c)
      }

      candidates = pageIndex.labelToComponents.get(LBP.WhitespaceColCandidate).getOrElse { Seq() }
    }
  }




  def growToMaxEmptySpace(
    pageId: Int@@PageID,
    startingRegion: LTBounds
  ): Option[LTBounds] = {

    val pageNum = pageIdMap(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val pageBounds = pageIndex.getPageGeometry.bounds
    // val rTreeIndex = pageIndex.componentIndex

    var currWhiteSpace = startingRegion
    val nudgeFactor = 0.05.toFloatExact()

    currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Left)
      .foreach { regionLeftOf =>
        // println(s"querying left of ${currWhiteSpace}: ${regionLeftOf}")
        val atomsLeftOf = rtreeSearch(pageId, regionLeftOf, LB.PageAtom, LB.Image, LB.VLinePath, LB.HLinePath)

        if (atomsLeftOf.nonEmpty) {
          val rightMostCC = atomsLeftOf.maxBy(_.bounds.right)
          regionLeftOf.splitVertical(rightMostCC.bounds.right+nudgeFactor)
            .foreach { case (left, right) =>
              currWhiteSpace = currWhiteSpace union right
            }
        }
      }

    currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Top)
      .foreach { regionAbove =>
        // println(s"querying above ${currWhiteSpace}: ${regionAbove}")
        val atomsAbove = rtreeSearch(pageId, regionAbove, LB.PageAtom, LB.Image, LB.HLinePath, LB.VLinePath)

        if (atomsAbove.nonEmpty) {
          val bottomMostCC = atomsAbove.maxBy(_.bounds.bottom)
          regionAbove.splitHorizontal(bottomMostCC.bounds.bottom+nudgeFactor)
            .foreach { case (top, bottom) =>
              currWhiteSpace = currWhiteSpace union bottom
            }
        } else {
          currWhiteSpace = currWhiteSpace union regionAbove
        }
      }

    currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Bottom)
      .foreach { regionBelow =>
        // println(s"querying below ${currWhiteSpace}: ${regionBelow}")
        val atomsBelow = rtreeSearch(pageId, regionBelow, LB.PageAtom, LB.Image, LB.HLinePath, LB.VLinePath)

        if (atomsBelow.nonEmpty) {
          val topmostCC = atomsBelow.minBy(_.bounds.top)
          regionBelow.splitHorizontal(topmostCC.bounds.top-nudgeFactor)
            .foreach { case (top, bottom) =>
              currWhiteSpace = currWhiteSpace union top
            }
        } else {
          currWhiteSpace = currWhiteSpace union regionBelow
        }
      }

    Some(currWhiteSpace)

  }

  def splitLinesWithOverlaps(pageId: Int@@PageID): Unit = {
    val pageNum = pageIdMap(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex
    for {
      hashedLines <- pageIndex.labelToComponents.get(LBP.LineByHash)
      hashedLine <- hashedLines
      cc <- rTreeIndex.get(hashedLine.unwrap)
    } {
      // Split up lines into strictly non-overlapping regions
      val intersects = rtreeSearch(pageId, cc.bounds, LBP.LineByHash)


      if (intersects.length > 1) {
        val totalBounds = intersects.map(_.bounds).reduce(_ union _)
        val charsInRegion = rtreeSearch(pageId, totalBounds, LB.PageAtom)
        // Remove the LineByHash regions
        // iterate over chars left-to-right and group them into non-overlaps and overlaps
        val allChars = charsInRegion.sortBy(_.bounds.left)

        allChars.groupByPairs { (c1, c2) =>
          c1.bounds.bottom == c2.bounds.bottom
        }.foreach{ ccs =>
          mpageIndex.labelRegion(ccs, LBP.LineByHash)
        }

        intersects.foreach { cc =>
          mpageIndex.removeComponent(cc)
        }
      }
    }
  }

  def splitLinesOnWhitespaceColumns(pageNum: Int@@PageNum): Unit = {
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex
    for {
      colRegions <- pageIndex.labelToComponents.get(LBP.WhitespaceCol)
      colRegionId <- colRegions
      colRegion <- rTreeIndex.get(colRegionId.unwrap)
      intersectedLine <- rTreeIndex.search(colRegion.bounds, {_.roleLabel == LBP.LineByHash})
    } {

      val charsInRegion = rTreeIndex.search(intersectedLine.bounds, {cc =>
        cc.roleLabel == LB.PageAtom
      })
      val allChars = charsInRegion.sortBy(_.bounds.left)

      val (leftSplit, rightSplit) = allChars.span(_.bounds.left < colRegion.bounds.left)

      mpageIndex.labelRegion(leftSplit, LBP.LineByHash)
      mpageIndex.labelRegion(rightSplit, LBP.LineByHash)

      mpageIndex.removeComponent(intersectedLine)
    }

  }

  def dividePageIntoBlocks(
    pageId: Int@@PageID,
    initBlock: LTBounds
  ): Unit = {
    val pageNum = pageIdMap(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex


    rtreeSearch(pageId, initBlock, LB.HLinePath)
      .foreach { hlineCC =>
        val hLine = hlineCC.bounds.toLine(Dir.Top)
        val extendedLine = hlineCC.bounds.withinRegion(initBlock).coveringRegion(Dir.Right, Dir.Left)
        val hLineExt = extendedLine.toLine(Dir.Top)
        val noPageHits = rTreeIndex.searchLine(hLineExt,
          {c => List(LB.PageAtom, LB.Image).contains(c.roleLabel)}
        ).isEmpty

        if (noPageHits) {
          val bottomRegion = extendedLine.withinRegion(initBlock).coveringRegion(Dir.BottomLeft, Dir.Bottom, Dir.BottomRight)
          val topRegion = bottomRegion

        }

      }
      // val charsInRegion = rTreeIndex.search(intersectedLine.bounds, {cc =>
      //   cc.roleLabel == LB.PageAtom
      // })
      // val allChars = charsInRegion.sortBy(_.bounds.left)

      // val (leftSplit, rightSplit) = allChars.span(_.bounds.left < colRegion.bounds.left)

      // mpageIndex.labelRegion(leftSplit, LBP.LineByHash)
      // mpageIndex.labelRegion(rightSplit, LBP.LineByHash)

      // mpageIndex.removeComponent(intersectedLine)


  }
}

object LBP {
  // import watrmarks.Label

  val LineByHash = Label("LineByHash")
  val LeftAlignedCharCol = Label("LeftAlignedCharCol")
  val WhitespaceColCandidate = Label("WhitespaceColCandidate")
  val WhitespaceCol = Label("WhitespaceCol")
  val Marked = Label("Marked")

}
