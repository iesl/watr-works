package edu.umass.cs.iesl.watr
package segment

import geometry._
import geometry.syntax._
import spindex._
import TypeTags._

import utils.ExactFloats._
import segment.{SegmentationLabels => LB}
import utils.SlicingAndDicing._

import org.dianahep.{histogrammar => HST}
import extract.ExtractedItem
import utils.FunctionalHelpers._
import textgrid.TextGrid
import watrmarks._
import utils.{RelativeDirection => Dir}
import utils.QuickNearestNeighbors._


trait CharColumnFinding extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val columnFinder = self

  import LB._

  def runPass1(): Unit = {

    markNatLangText()

  }

  def runPass2(): Unit = {

  }


  /**
    *  Returns final reading-ordered text
    */
  def getTextGrid(): TextGrid = {

    val rows1 = getLabeledLines(LB.ContiguousGlyphBaseline)
      .map { baselineShape =>
        val minId  = getCharsForShape(baselineShape).map(_.id.unwrap).min
        val textRow = createTextRowFromVisualLine(baselineShape)
        (minId, textRow)
      }

    val rows2 = getLabeledLines(LB.SymbolicGlyphLine)
      .map { symbolicGlyphLine =>
        val textRow = createTextRowFromVisualLine(symbolicGlyphLine)
        val minId  = getCharsForShape(symbolicGlyphLine).map(_.id.unwrap).min
        (minId, textRow)
      }

    val rows = (rows1 ++ rows2).sortBy(_._1).map(_._2)


    TextGrid.fromRows(docScope.stableId,  rows)
  }

  def indexPathRegions(): Unit = {
    val pathShapes = pageIndex.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.PathItem] }
      .map { item =>
        indexShape(item.minBBox, LB.PathBounds)
      }

    traceLog.trace { shape(pathShapes:_*) tagged "Path Line Bounds" }
  }

  def indexImageRegionsAndDeleteOverlaps(): Unit = {
    val deletedShapes = pageIndex.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .flatMap { imageItem =>
        indexShape(imageItem.minBBox, LB.Image)
        val baseLines: Seq[LineShape] = searchForLines(imageItem.minBBox, LB.CharRunFontBaseline)
        deleteShapes(baseLines)
        baseLines
      }

    traceLog.trace { shape(deletedShapes:_*) tagged "Deleted Intersect Image Bounds" }
    traceLog.trace { labeledShapes(LB.Image) tagged "Image Regions" }
  }


  protected def findDeltas(ns: Seq[Int@@FloatRep]): Seq[Int@@FloatRep] = {
    ns.zip(ns.tail)
      .map {case (n1, n2) => n2 - n1 }
  }

  protected def findPairwiseVerticalJumps[G <: GeometricFigure](
    shapes: Seq[LabeledShape[G]], getY: (LabeledShape[G]) => Int@@FloatRep
  ): Seq[(Int@@FloatRep, (LabeledShape[G], LabeledShape[G]))] = {

    val sorted = shapes.sortBy { getY(_) }
    val yVals = sorted.map(s => getY(s))
    val deltas = findDeltas(yVals)

    deltas.zip(sorted.zip(sorted.tail))

  }




  /**
    * Foreach horizontal nat-lang text line, find the 2 closest lines below
    * it, and record the distances to each line.
    *
    * Finally, cluster those vertical jumps into centroids.
    *
    *  l0     :    --------------
    *  dists  :           | |
    *  l0+1   :    -------- | ---
    *  l0+2   :    --------------
    */
  protected def recordNatLangVerticalLineSpacingStats(baselineShapes: Seq[LineShape]): Unit = {
    val windowSize = 2

    val contextDeltas = baselineShapes.map { baselineShape: LineShape =>
      val Line(Point.Doubles(x1, y1), Point.Doubles(x2, y2)) = baselineShape.shape

      val y1Exact = baselineShape.shape.p1.y

      pageVerticalSlice(x1, x2-x1).toSeq.flatMap { pageColumn =>
        val (aboveLine, belowLine) = pageColumn.splitHorizontal(y1Exact)


        val below = belowLine.map { bbox =>
          val query = bbox.translate(x=0, y = +1.0)
          searchForLines(query, LB.CharRunFontBaseline)
        } getOrElse { List() }


        val winBelow = below.sortBy(_.shape.p1.y).take(windowSize)

        winBelow.map { ctxLine => ctxLine.shape.p1.y - y1Exact }
      }
    }

    val yJumps = contextDeltas.flatten
    val yJumpClusters = qnn(yJumps, tolerance = 0.5d)

    val nonUniqueJumps = yJumpClusters.filter { bin =>
      bin.size() > 1
    }

    println(s"recordNatLangLineSpacing: Assigned Bins")
    println(nonUniqueJumps.mkString("\n  ", "\n  ", "\n"))
  }


  private def recordCharRunWidths(charRunBaselineShapes: Seq[LineShape]): Unit = {
    val pagewiseLineWidthTable = docScope.getPagewiseLinewidthTable();


    val leftToRightGroups = charRunBaselineShapes.groupByPairs {
      case (item1, item2) =>
        val b1 = item1.shape.bounds()
        val b2 = item2.shape.bounds()
        val leftToRight = b1.isStrictlyLeftOf(b2)
        val colinear = b1.bottom == b2.bottom

        leftToRight && colinear
    }

    leftToRightGroups.foreach { lineGroup =>
      val groupBounds = lineGroup.map(_.shape.bounds()).reduce(_ union _)
      val extractedItems = lineGroup.flatMap { lineShape =>
        getExtractedItemsForShape(lineShape)
      }
      val charItems = extractedItems.collect{ case i: ExtractedItem.CharItem =>  i }

      val (num, mostCommonScaledFontId) = charItems
        .map{ item => item.scaledFontId }
        .groupBy { x => x }
        .toList
        .map{case (k, v) => (v.length, k) }
        .sortBy(l => - l._1)
        .head

      // val bottomLine = groupBounds.toLine(Dir.Bottom)
      pagewiseLineWidthTable.modifyOrSet(pageNum, mostCommonScaledFontId, groupBounds.width :: _, List(groupBounds.width))
    }

    traceLog.trace {
      val bottomLines = leftToRightGroups
        .filter(_.length > 1)
        .map { lineGroup =>
          val groupBounds = lineGroup.map(_.shape.bounds()).reduce(_ union _)
          groupBounds.toLine(Dir.Bottom)
        }

      figure(bottomLines:_*) tagged "Joined Nat Lang CharRun Baselines"
    }

  }

  private def inferNLBaselineContinuity(baselineShapes: Seq[LineShape]): Unit = {
    pageIndex.shapes.ensureCluster(LB.ContiguousGlyphs)

    for {
      baselineShape <- baselineShapes
    } {

      val extractedItems = getExtractedItemsForShape(baselineShape)
      val charItems = extractedItems.collect{ case i: ExtractedItem.CharItem =>  i }
      val line = baselineShape.shape

      val heights = charItems.map{ item =>
        item.minBBox.height
      }
      // TODO get doc-wide avg or max height for these chars
      val maxHeight = heights.max
      val avgWidth = charItems.map{ _.minBBox.width.asDouble() }.sum / charItems.length
      val windowWidth = avgWidth*6
      val windowDelta = avgWidth*4


      for {
        baselineSlice <- pageHorizontalSlice(line.p1.y.asDouble()-maxHeight.asDouble(), maxHeight.asDouble())
      } {

        // traceLog.trace { figure(baselineSlice) tagged "Sliding Window PageSlice" }

        val windows = baselineSlice.slidingHorizontalWindow(windowWidth, windowDelta)
        var firstNLGlyphWin = Int.MaxValue
        windows.zipWithIndex.foreach { case (window, winNum) =>
          val glyphsInWindowNL = searchForRects(window, LB.NatLangGlyph)
          val glyphsInWindowSymbolic = searchForRects(window, LB.SymbolicGlyph)

          val someGlyphIsNLOrRootedToNL = glyphsInWindowNL.nonEmpty || {
            glyphsInWindowSymbolic.exists { glyphShape =>
              pageIndex.shapes.getClusterRoot(LB.ContiguousGlyphs, glyphShape).isDefined
            }
          }

          if (someGlyphIsNLOrRootedToNL) {
            val glyphsInWindow = glyphsInWindowNL ++ glyphsInWindowSymbolic

            val glyphItems = glyphsInWindow.map{ g =>
              getExtractedItemsForShape(g).head
            }

            val ids = glyphItems.filter(_ != null)
              .map{ g => g.id.unwrap }
              .sorted
              .toList

            val idRange = (ids.min to ids.max).toList
            val glyphsAreConsecutive = idRange == ids
            if (glyphsAreConsecutive) {
              firstNLGlyphWin = math.min(firstNLGlyphWin, winNum)
              clusterN(LB.ContiguousGlyphs, glyphsInWindow)
            }

          }
        }

        val revWindows = windows.slice(0, firstNLGlyphWin+3).reverse

        revWindows.zipWithIndex.foreach { case (window, winNum) =>
          val glyphsInWindowNL = searchForRects(window, LB.NatLangGlyph)
          val glyphsInWindowSymbolic = searchForRects(window, LB.SymbolicGlyph)

          val someGlyphIsNLOrRootedToNL = glyphsInWindowNL.nonEmpty || {
            glyphsInWindowSymbolic.exists { glyphShape =>
              pageIndex.shapes.getClusterRoot(LB.ContiguousGlyphs, glyphShape).isDefined
            }
          }

          if (someGlyphIsNLOrRootedToNL) {
            val glyphsInWindow = glyphsInWindowNL ++ glyphsInWindowSymbolic

            val glyphItems = glyphsInWindow.map{ g =>
              getExtractedItemsForShape(g).head
            }


            val ids = glyphItems.filter(_ != null)
              .map{ g => g.id.unwrap }
              .sorted
              .toList

            val idRange = (ids.min to ids.max).toList

            val glyphsAreConsecutive = idRange == ids

            if (glyphsAreConsecutive) {
              clusterN(LB.ContiguousGlyphs, glyphsInWindow)
            }
          }
        }
      }
    }

    val allClusterBounds = getClusteredRects(LB.ContiguousGlyphs).map{
      case (clusterReprId, glyphRects) =>
        val glyphBounds = glyphRects.map(_.shape).reduce(_ union _)
        val glyphItems = getExtractedItemsForShapes(glyphRects).flatten
        val continuousGlyphBaseline = glyphBounds.toLine(Dir.Bottom)
        val baselineShape = indexShape(continuousGlyphBaseline, LB.ContiguousGlyphBaseline)
        setExtractedItemsForShape(baselineShape, glyphItems)
        glyphBounds
    }

    traceLog.trace {
      val bottomLines = allClusterBounds.map(_.toLine(Dir.Bottom))
      figure(bottomLines:_*) tagged "ContiguousGlyphBounds"
    }

  }

  private def clusterColumnPoints(points: Seq[Point], label: Label, leftAlignedPoints: Boolean): Unit = {
    val pointHist = HST.SparselyBin.ing(1.4, {p: Point => p.x.asDouble()})
    val pageRight = pageGeometry.right
    val clusterLabel = label::Cluster
    val evidenceLabel = label::Evidence

    points.foreach{ point =>
      indexShape(point, evidenceLabel)
      pointHist.fill(point)
    }

    pageIndex.shapes.ensureCluster(clusterLabel)

    traceLog.trace { labeledShapes(evidenceLabel) tagged s"Points ${evidenceLabel}" }

    pointHist.bins.toList
      .filter { case (bin, counting) =>
        val binWidth = pointHist.binWidth
        val binRight = (bin+1)*binWidth
        counting.entries > 1 && binRight.toFloatExact() < pageRight
      }.foreach{ case (bin, counting) =>
          val binWidth = pointHist.binWidth
          val binLeft = bin*binWidth
          val pageColumn = pageVerticalSlice(binLeft, binWidth).get
          val hitPoints = searchForPoints(pageColumn, evidenceLabel)

          deleteShapes(hitPoints)

          traceLog.trace { figure(pageColumn) tagged "PageColumn" }

          val uniqYHits = hitPoints.uniqueBy(_.shape.y)


          if (uniqYHits.length > 1) {
            val yvals = uniqYHits.map(_.shape.y)
            val (maxy, miny) = (yvals.max,  yvals.min)
            val height = maxy - miny

            val colActual = pageColumn.getHorizontalSlice(miny, height).get
            traceLog.trace { figure(colActual) tagged s"Column Nonempty ${evidenceLabel}" }

            val intersectingBaselines = searchForLines(colActual, LB.CharRunFontBaseline)
              .sortBy(_.shape.p1.y)

            val hitsAndOverlaps = if (leftAlignedPoints) {
              collectSpanEither(intersectingBaselines, { baselineShape: LineShape =>
                val hitLeftX = baselineShape.shape.p1.x
                colActual.left <= hitLeftX
              })
            } else {
              collectSpanEither(intersectingBaselines, { baselineShape: LineShape =>
                val hitRightX = baselineShape.shape.p2.x
                hitRightX <= colActual.right
              })

            }


            hitsAndOverlaps.foreach{ _ match {
              case Right(baselineShapes) if baselineShapes.length > 1 =>
                clusterN(clusterLabel, baselineShapes)

                val columnPoints = if (leftAlignedPoints) {
                  baselineShapes.map(_.shape.p1)
                } else {
                  baselineShapes.map(_.shape.p2)
                }

                val contiguousYValues = columnPoints.map(_.y)

                pageIndex.addPageVerticalJumps(
                  findDeltas(contiguousYValues.sorted)
                )

                traceLog.trace {
                  val columnMbr = columnPoints.map(minBoundingRect(_))reduce(_ union _)
                  // val evLine = Line(baselineShapes.head.shape.p1, baselineShapes.last.shape.p1)
                  figure(columnMbr) tagged s"${label} Points MinBounds"
                }

              case _ =>
            }}
          }
      }

    deleteLabeledShapes(evidenceLabel)
  }


  def boundedHLine(bbox: LTBounds, atY: Int@@FloatRep): Line = {
    val LTBounds(x, y, w, h) = bbox
    Line(
      Point(x, atY),
      Point(x+w, atY)
    )
  }

  def boundedVerticalLine(bbox: LTBounds, atX: Int@@FloatRep): Line = {
    val LTBounds(x, y, w, h) = bbox
    Line(
      Point(atX, y),
      Point(atX, y+h)
    )
  }


  def createCharRunFontBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted = charRun.sortBy { _.minBBox.left }
    val runBeginPt =  Point(xSorted.head.minBBox.left, xSorted.head.fontBbox.bottom)
    val runEndPt = Point(xSorted.last.minBBox.right, xSorted.last.fontBbox.bottom)
    Line(runBeginPt, runEndPt)
  }



  private def findPageCharRuns(retainNatLang: Boolean): Seq[Seq[ExtractedItem.CharItem]] = {
    val charRuns = pageIndex.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filter(_.fontProps.isNatLangFont() == retainNatLang)
      .groupByPairs {
        case (item1, item2) =>
          lazy val consecutive = item1.id.unwrap+1 == item2.id.unwrap
          lazy val sameLine = item1.fontBbox.bottom == item2.fontBbox.bottom
          lazy val sameFont = item1.fontProps == item2.fontProps

          consecutive && sameLine && sameFont
      }

    charRuns
  }

  private def findSymbolicCharRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    val charRuns = pageIndex.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filterNot(_.fontProps.isNatLangFont())
      .groupByPairs {
        case (item1, item2) =>
          lazy val consecutive = item1.id.unwrap+1 == item2.id.unwrap
          lazy val leftToRight = item1.minBBox.left < item2.minBBox.left
          lazy val colinear = item1.minBBox.isNeitherAboveNorBelow(item2.minBBox)

          consecutive && leftToRight && colinear
      }

    charRuns
  }




  def hasNoNonTextOverlaps(queryRect: LTBounds): Boolean = {
    val noImages = searchForRects(queryRect, LB.Image).isEmpty
    val noPaths = searchForRects(queryRect, LB.PathBounds).isEmpty
    noImages && noPaths
  }

  def hasNoOverlaps(queryRect: LTBounds): Boolean = {
    lazy val noGlyphs = searchForRects(queryRect, LB.Glyph).isEmpty
    hasNoNonTextOverlaps(queryRect) && noGlyphs
  }

  def doLineGrouping(sortedLines: Seq[LineShape]): Seq[Seq[LineShape]] = {
    val lineGroups = sortedLines.groupByWindow { case (prevs, currLine) =>

      val lastLine = prevs.last

      val lastLineItems = getCharsForShape(lastLine)
      val currLineItems = getCharsForShape(currLine)

      lazy val currLineText = currLineItems.map(_.char).mkString

      val item1 = lastLineItems.last
      val item2 = currLineItems.head
      val line1EndId = item1.id.unwrap
      val line2StartId = item2.id.unwrap
      val consecutive = line2StartId == line1EndId + 1

      lazy val topToBottom = item1.minBBox.bottom < item2.minBBox.bottom
      lazy val inOrder = topToBottom

      lazy val prevWindowBounds = prevs.map(_.shape.bounds()).reduce(_ union _)

      lazy val combinedWindowBounds = prevWindowBounds union currLine.shape.bounds()

      traceLog.trace {
        figure(combinedWindowBounds) tagged s"Window Bounds ${currLineText} "
      }

      lazy val expansionBounds = lastLine.shape.bounds() union currLine.shape.bounds()

      lazy val noLeftOverlaps = prevWindowBounds.withinRegion(combinedWindowBounds)
        .adjacentRegion(Dir.Left)
        .map(hasNoOverlaps(_))
        .getOrElse(true)

      lazy val noRightOverlaps = prevWindowBounds.withinRegion(combinedWindowBounds)
        .adjacentRegion(Dir.Right)
        .map(hasNoOverlaps(_))
        .getOrElse(true)

      lazy val noLateralOverlaps = noLeftOverlaps && noRightOverlaps

      lazy val glyphAndLineCountsMatch = expansionBounds.withinRegion(combinedWindowBounds)
        .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
        .map{ expansionRect =>

          val queryRect = expansionRect.shave(Dir.Top, FloatExact.epsilon * 5)


          val foundGlyphs: Seq[RectShape] = searchForRects(queryRect, LB.Glyph)

          traceLog.trace {
            figure(queryRect) tagged s"Expansion Rect ${currLineText}"
          }

          traceLog.trace {
            shape(foundGlyphs:_*) tagged s"Expansion Glyphs ${currLineText}"
          }

          val glyphCountsMatch = currLineItems.length == foundGlyphs.length

          val noNonTextOverlaps = hasNoNonTextOverlaps(queryRect)

          noNonTextOverlaps && (glyphCountsMatch || {

            lazy val foundItemIds = foundGlyphs.flatMap{ g =>
              getExtractedItemsForShape(g).map(_.id.unwrap)
            }

            lazy val lastLineIds = lastLineItems.map(_.id.unwrap)
            lazy val commonIds = foundItemIds.intersect(lastLineIds)
            lazy val adjustedFoundGlyphCount = foundGlyphs.length - commonIds.length

            traceLog.trace {
              val commonItems = lastLineItems.filter(item => commonIds.contains(item.id.unwrap))
              val commonItemBounds = commonItems.map(_.minBBox)
              figure(commonItemBounds:_*) tagged s"Common Glyphs ${currLineText}"
            }

            currLineItems.length == adjustedFoundGlyphCount
          })

        } getOrElse { false }

      consecutive && inOrder && glyphAndLineCountsMatch && noLateralOverlaps
    }

    traceLog.trace {
      // val groupBounds = lineGroups.filter(_.length > 1)
      val groupBounds = lineGroups.map { group => 
        if (group.length==1) {
          group.head.shape
        } else {
          group.map(_.shape.bounds()).reduce(_ union _)
        }
      }

      figure(groupBounds:_*) tagged "Grouped Text Blocks"
    }

    lineGroups

  }


  private def findContiguousBlocks(label: Label): Unit = {

    val fontsByMostOccuring = docScope.getFontsWithOccuranceCounts()
      .sortBy(_._2).reverse.map(_._1)

    val sortedLines = getLabeledLines(label).sortBy { lineShape =>
      getCharsForShape(lineShape).head.id.unwrap
    }

    def groupEverything(
      scaledFontIds: List[String@@ScaledFontID],
      lineShapes: Seq[LineShape],
      depth: Int = 0
    ): Seq[Seq[LineShape]] = scaledFontIds match {
      case headFontId :: tailFontIds =>
        val markedLineSpans = collectSpanEither[LineShape](lineShapes, { lineShape =>
          getFontsForShape(lineShape).contains(headFontId)
        })

        traceLog.traceAll {
          markedLineSpans.map { _ match {
            case Right(lines) =>
              val groupBounds = lines.map(_.shape.bounds()).reduce(_ union _)
              figure(groupBounds) tagged s"Shared Font#${depth}. ${headFontId}"

            case Left(lines)  =>
              val groupBounds = lines.map(_.shape.bounds()).reduce(_ union _)
              figure(groupBounds) tagged s"Excluded From Font#${depth}. ${headFontId}"
          }}
        }

        markedLineSpans.flatMap{ _ match {
          case Right(lines) => doLineGrouping(lines)
          case Left(lines)  => groupEverything(tailFontIds, lines, depth+1)
        }}

      case Nil => lineShapes.map(List(_))
    }

    groupEverything(fontsByMostOccuring.toList, sortedLines)

  }





  private def markNatLangText(): Unit = {
    val natLangCharRuns = findPageCharRuns(retainNatLang=true)


    natLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeAndSetItems(item.minBBox, LB.NatLangGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }
    }


    initNatLangCharSpans(natLangCharRuns)

    indexPathRegions()

    indexImageRegionsAndDeleteOverlaps()

    findContiguousBlocks(LB.CharRunFontBaseline)

    val symbolicLangCharRuns = findSymbolicCharRuns()

    symbolicLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeAndSetItems(item.minBBox, LB.SymbolicGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }
    }

    traceLog.trace { labeledShapes(LB.NatLangGlyph) tagged "All Natural Lang Glyphs" }
    traceLog.trace { labeledShapes(LB.SymbolicGlyph) tagged "All Symbolic Glyphs" }

    initSymbolicCharSpans(symbolicLangCharRuns)

  }

  private def initSymbolicCharSpans(symbolicRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    symbolicRuns.map { charRun =>
      val xSorted = charRun.sortBy { _.minBBox.left }
      val p1 = xSorted.head.minBBox.toPoint(Dir.Center)
      val p2 = xSorted.last.minBBox.toPoint(Dir.Center)

      val baseLine = Line(p1, p2)

      val symbolicGlyphLine = indexShape(baseLine, LB.SymbolicGlyphLine)

      setExtractedItemsForShape(symbolicGlyphLine, charRun)

      symbolicGlyphLine
    }.asLineShapes


    traceLog.trace { labeledShapes(LB.SymbolicGlyphLine) }
  }

  private def initNatLangCharSpans(natLangCharRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    val charRunBaselineShapes = natLangCharRuns.map { charRun =>
      val charItems = charRun.map(_.asInstanceOf[ExtractedItem.CharItem])

      val baseLine = createCharRunFontBaseline(charItems)

      val baselineShape = indexShape(baseLine, LB.CharRunFontBaseline)

      val fontIds = charItems.map{ _.scaledFontId }.toSet

      setFontsForShape(baselineShape, fontIds)

      setExtractedItemsForShape(baselineShape, charRun)

      baselineShape
    }.asLineShapes

    traceLog.trace { labeledShapes(LB.CharRunFontBaseline) tagged "Initial Font Baselines" }

    // recordNatLangVerticalLineSpacingStats(charRunBaselineShapes)
    recordCharRunWidths(charRunBaselineShapes)


    val leftmostPoints = charRunBaselineShapes.map{ _.shape.p1 }
    val rightmostPoints = charRunBaselineShapes.map{ _.shape.p2 }

    clusterColumnPoints(leftmostPoints, LB.LeftAlignedCharCol, leftAlignedPoints=true)
    clusterColumnPoints(rightmostPoints, LB.RightAlignedCharCol, leftAlignedPoints=false)

    inferNLBaselineContinuity(charRunBaselineShapes)

  }


}

// private def findContiguousBlocksOrig(label: Label): Unit = {
//   def hasNoNonTextOverlaps(queryRect: LTBounds): Boolean = {
//     val noImages = searchForRects(queryRect, LB.Image).isEmpty
//     val noPaths = searchForRects(queryRect, LB.PathBounds).isEmpty
//     noImages && noPaths
//   }

//   def hasNoOverlaps(queryRect: LTBounds): Boolean = {
//     lazy val noGlyphs = searchForRects(queryRect, LB.Glyph).isEmpty
//     hasNoNonTextOverlaps(queryRect) && noGlyphs
//   }

//   val fontsByMostOccuring = docScope.getFontsWithOccuranceCounts()
//     .sortBy(_._2)
//     .reverse.map(_._1)

//   val sortedLines = getLabeledLines(label).sortBy { lineShape =>
//     val lineItems = getExtractedItemsForShape(lineShape)
//       .collect{ case i: ExtractedItem.CharItem =>  i }
//     lineItems.head.id.unwrap
//   }


//   val lineGroups = sortedLines.groupByWindow { case (prevs, currLine) =>

//     val lastLine = prevs.last

//     val lastLineItems = getCharsForShape(lastLine)
//     val currLineItems = getCharsForShape(currLine)

//     val allPrevFonts: Set[String@@ScaledFontID] = prevs.map(getFontsForShape(_)).reduce(_ ++ _)
//     val currFonts = getFontsForShape(currLine)
//     val hasSharedFont = allPrevFonts.intersect(currFonts).nonEmpty

//     val item1 = lastLineItems.last
//     val item2 = currLineItems.head
//     val line1EndId = item1.id.unwrap
//     val line2StartId = item2.id.unwrap
//     val consecutive = line2StartId == line1EndId + 1

//     lazy val topToBottom = item1.minBBox.bottom < item2.minBBox.bottom
//     lazy val inOrder = topToBottom

//     lazy val prevWindowBounds = prevs.map(_.shape.bounds()).reduce(_ union _)

//     lazy val combinedWindowBounds = prevWindowBounds union currLine.shape.bounds()

//     lazy val expansionBounds = lastLine.shape.bounds() union currLine.shape.bounds()

//     traceLog.trace {
//       val text = currLineItems.map(_.char).mkString
//       figure(combinedWindowBounds) tagged s"Window Bounds ${text} "
//     }


//     lazy val noLeftOverlaps = prevWindowBounds.withinRegion(combinedWindowBounds)
//       .adjacentRegion(Dir.Left)
//       .map(hasNoOverlaps(_))
//       .getOrElse(true)

//     lazy val noRightOverlaps = prevWindowBounds.withinRegion(combinedWindowBounds)
//       .adjacentRegion(Dir.Right)
//       .map(hasNoOverlaps(_))
//       .getOrElse(true)

//     lazy val noLateralOverlaps = noLeftOverlaps && noRightOverlaps

//     lazy val glyphAndLineCountsMatch = expansionBounds.withinRegion(combinedWindowBounds)
//       .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
//       .map{ expansionRect =>

//         val queryRect = expansionRect.shave(Dir.Top, FloatExact.epsilon * 5)

//         val foundGlyphs: Seq[RectShape] = searchForRects(queryRect, LB.Glyph)
//         val glyphCountsMatch = currLineItems.length == foundGlyphs.length

//         traceLog.trace {
//           val text = currLineItems.map(_.char).mkString
//           figure(queryRect) tagged s"Expansion Rect ${text}"
//         }

//         traceLog.trace {
//           val text = currLineItems.map(_.char).mkString
//           shape(foundGlyphs:_*) tagged s"Expansion Glyphs ${text}"
//         }


//         val noNonTextOverlaps = hasNoNonTextOverlaps(queryRect)

//         noNonTextOverlaps && (glyphCountsMatch || {

//           lazy val foundItemIds = foundGlyphs.flatMap{ g =>
//             getExtractedItemsForShape(g).map(_.id.unwrap)
//           }

//           lazy val lastLineIds = lastLineItems.map(_.id.unwrap)
//           lazy val commonIds = foundItemIds.intersect(lastLineIds)
//           lazy val adjustedFoundGlyphCount = foundGlyphs.length - commonIds.length

//           traceLog.trace {
//             val text = currLineItems.map(_.char).mkString
//             val commonItems = lastLineItems.filter(item => commonIds.contains(item.id.unwrap))
//             val commonItemBounds = commonItems.map(_.minBBox)
//             figure(commonItemBounds:_*) tagged s"Common Glyphs ${text}"
//           }

//           currLineItems.length == adjustedFoundGlyphCount
//         })

//       } getOrElse { false }


//     consecutive && inOrder && glyphAndLineCountsMatch && noLateralOverlaps && hasSharedFont
//   }




//   traceLog.trace {
//     val groupBounds = lineGroups
//       .filter(_.length > 1)
//       .map { lineGroup =>
//         val groupBounds = lineGroup.map(_.shape.bounds()).reduce(_ union _)
//         groupBounds
//       }

//     figure(groupBounds:_*) tagged "Grouped Text Blocks"
  //   }


  // }
