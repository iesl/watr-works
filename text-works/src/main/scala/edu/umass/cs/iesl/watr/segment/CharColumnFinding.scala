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


    excludeImageRegionPoints()

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

    // val reorderedLines = orderedLines.sortBy { baselineShape =>
    //   val items = getExtractedItemsForShape(baselineShape)
    //   items.head.id.unwrap
    // }

    // val rows = reorderedLines.map { visualBaseline =>
    //   createTextRowFromVisualLine(visualBaseline.asInstanceOf[LineShape])
    // }


    TextGrid.fromRows(docScope.stableId,  rows)
  }


  def excludeImageRegionPoints(): Unit = {
    val deletedShapes = pageIndex.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .flatMap { imageItem =>
        indexShape(imageItem.minBBox, LB.Image)
        val baseLines: Seq[LineShape] = searchForLines(imageItem.minBBox, LB.CharRunFontBaseline)
        deleteShapes(baseLines)
        baseLines
      }

    traceLog.trace { shape(deletedShapes:_*) tagged "IntersectImageBounds" }
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

        traceLog.trace { figure(baselineSlice) tagged "Sliding Window PageSlice" }

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


  private def markNatLangText(): Unit = {
    val natLangCharRuns = findPageCharRuns(retainNatLang=true)


    natLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeForItems(item.minBBox, LB.NatLangGlyph, item)
      }
    }


    initNatLangCharSpans(natLangCharRuns)

    val symbolicLangCharRuns = findSymbolicCharRuns()

    symbolicLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeForItems(item.minBBox, LB.SymbolicGlyph, item)
      }
    }

    traceLog.trace { labeledShapes(LB.SymbolicGlyph) }

    initSymbolicCharSpans(symbolicLangCharRuns)

  }

  private def initSymbolicCharSpans(symbolicRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    symbolicRuns.map { charRun =>
      val xSorted = charRun.sortBy { _.minBBox.left }
      val p1 = xSorted.head.minBBox.toPoint(Dir.Center)
      val p2 = xSorted.last.minBBox.toPoint(Dir.Center)

      val baseLine = Line(p1, p2)

      val symbolicGlyphLine = indexShape(baseLine, LB.SymbolicGlyphLine)

      // charRun.foreach { item =>
      //   pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, symbolicGlyphLine)
      // }

      setExtractedItemsForShape(symbolicGlyphLine, charRun)

      symbolicGlyphLine
    }.asLineShapes


    traceLog.trace { labeledShapes(LB.SymbolicGlyphLine) }
  }

  private def findCharRunMetrics(): Unit = {
    // Sort fonts
  }

  private def initNatLangCharSpans(natLangCharRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    val charRunBaselineShapes = natLangCharRuns.map { charRun =>
      val baseLine = createCharRunFontBaseline(charRun.map(_.asInstanceOf[ExtractedItem.CharItem]))

      val baselineShape = indexShape(baseLine, LB.CharRunFontBaseline)

      charRun.foreach { item =>
        pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, baselineShape)
      }

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
