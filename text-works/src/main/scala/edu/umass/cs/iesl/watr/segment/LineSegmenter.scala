package edu.umass.cs.iesl.watr
package segment


import geometry._
import geometry.syntax._
import extract._
import segment.{SegmentationLabels => LB}
import utils.ExactFloats._
import utils.{RelativeDirection => Dir}
import utils.FunctionalHelpers._
import utils.QuickNearestNeighbors._
import utils.SlicingAndDicing._
import watrmarks._
import textboxing.{TextBoxing => TB}, TB._
import utils.SlicingAndDicing._

import TypeTags._

trait LineSegmentation extends PageScopeSegmenter
    with FontAndGlyphMetrics
    with TextBlockGrouping { self =>

  lazy val lineSegmenter = self

  def doLineJoining(focalFont: String@@ScaledFontID, baselineShape: LineShape): Unit = {
    val baselineFonts = getFontsForShape(baselineShape)

    assume(baselineFonts.size==1)
    assume(baselineFonts.contains(focalFont))

    val line = baselineShape.shape
    // val scaledFontMetrics = docScope.fontDefs.getScaledFont(focalFont).orDie(s"font ${focalFont} not found")
    val charItems = getCharsForShape(baselineShape)

    // val metrics = scaledFontMetrics.fontMetrics.orDie(s"font metrics ${focalFont} not found")
    val headItem = charItems.head
    // val trueBaseline = 0
    // headItem.char
    // headItem.fontProps
    headItem.glyphProps.scalingFactor


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

  def joinFontBaselines(label: Label): Unit = {
    val fontsByMostOccuring = docScope.getFontsWithOccuranceCounts()
      .sortBy(_._2).reverse.map(_._1)

    //... Filter to fonts on page

    pageIndex.shapes.ensureCluster(LB.ContiguousGlyphs)

    def joinLinesLoop(
      scaledFontIds: List[String@@ScaledFontID],
      lineShapes: Seq[LineShape],
      depth: Int = 0
    ): Unit = scaledFontIds match {

      case headFontId :: tailFontIds =>
        val markedLineSpans = collectSpanEither[LineShape](lineShapes, { lineShape =>
          getFontsForShape(lineShape).contains(headFontId)
        })


        markedLineSpans.foreach{ _ match {
          case Right(lines) =>
            lines.foreach {lineShape =>
              doLineJoining(headFontId, lineShape)
            }

          case Left(lines)  => joinLinesLoop(tailFontIds, lines, depth+1)
        }}

      case Nil =>
    }

    val startingLines = getLabeledLines(label)

    joinLinesLoop(fontsByMostOccuring.toList, startingLines)
  }

  def generatePageRules(label: Label): Unit = {
    val fontsByMostOccuring = docScope.getFontsWithOccuranceCounts()
      .sortBy(_._2).reverse.map(_._1)

    //... Filter to fonts on page


    def _loop(
      scaledFontIds: List[String@@ScaledFontID],
      lineShapes: Seq[LineShape],
      depth: Int = 0
    ): Unit = scaledFontIds match {

      case headFontId :: tailFontIds =>
        val (linesForFont, others) = lineShapes.partition{ lineShape =>
          getFontsForShape(lineShape).contains(headFontId)
        }

        val allAdjustedOffsets = linesForFont.map{ lineShape =>
          val line = lineShape.shape
          val fontOffsets = docScope.fontDefs.getScaledFontOffsets(headFontId)
          fontOffsets.forBaseline(line.p1.y)
        }
        val grouped = allAdjustedOffsets.groupBy(_.baseline)
        val uniqueAdjusted = grouped.values.map(_.head)
        val sorted = uniqueAdjusted.toList.sortBy(_.baseline)

        sorted.foreach { offsetsAtLine =>
          val capDescentBandHeight = offsetsAtLine.descent - offsetsAtLine.cap

          pageHorizontalSlice(
            offsetsAtLine.cap.asDouble(),
            capDescentBandHeight.asDouble()
          ).map{ slice =>
            val band = indexShape(slice, LB.CapDescenderBand)
            traceLog.trace {
              traceLog.shape(band) tagged s"Caps Descender Page Rules Font#${depth}. ${headFontId}"
            }
          }

          val baselineMidRiseHeight = offsetsAtLine.baseline - offsetsAtLine.midrise

          pageHorizontalSlice(
            offsetsAtLine.midrise.asDouble(),
            baselineMidRiseHeight.asDouble()
          ).map{ slice =>
            val band = indexShape(slice, LB.BaselineMidriseBand)
            traceLog.trace {
              traceLog.shape(band) tagged s"Baseline Midrise Page Rules Font#${depth}. ${headFontId}"
            }
          }
        }

        _loop(tailFontIds, others, depth+1)

      case Nil =>
    }

    val startingLines = getLabeledLines(label)

    _loop(fontsByMostOccuring.toList, startingLines)
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

  def createCharRunFontBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted = charRun.sortBy { _.minBBox.left }
    val runBeginPt =  Point(xSorted.head.minBBox.left, xSorted.head.fontBbox.bottom)
    val runEndPt = Point(xSorted.last.minBBox.right, xSorted.last.fontBbox.bottom)
    Line(runBeginPt, runEndPt)
  }


  protected def recordCharRunWidths(charRunBaselineShapes: Seq[LineShape]): Unit = {
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


  private def findNatLangBaselineRuns(retainNatLang: Boolean): Seq[Seq[ExtractedItem.CharItem]] = {
    pageIndex.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filter(_.fontProps.isNatLangFont() == retainNatLang)
      .groupByPairs {
        case (item1, item2) =>
          lazy val consecutive = item1.id.unwrap+1 == item2.id.unwrap
          lazy val sameLine = item1.fontBbox.bottom == item2.fontBbox.bottom
          lazy val sameFont = item1.fontProps == item2.fontProps

          consecutive && sameLine && sameFont
      }
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

  def markNatLangText(): Unit = {

    recordNatLangCharSpans(
      LB.CharRunFontBaseline,
      findNatLangBaselineRuns(retainNatLang=true)
    )
    // assert(index contains (LB.CharRunFontBaseline))


    // recordNatLangVerticalLineSpacingStats(charRunBaselineShapes)
    // recordCharRunWidths(charRunBaselineShapes)

    // joinFontBaselines(LB.CharRunFontBaseline)

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

  private def recordNatLangCharSpans(spanLabel: Label, natLangCharRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    natLangCharRuns.foreach { charRun =>
      val charItems = charRun.map(_.asInstanceOf[ExtractedItem.CharItem])

      charItems.foreach { item =>
        indexShapeAndSetItems(item.minBBox, LB.NatLangGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }

      val baseLine = createCharRunFontBaseline(charItems)

      val baselineShape = indexShape(baseLine, spanLabel)

      val fontIds = charItems.map{ _.scaledFontId }.toSet

      setFontsForShape(baselineShape, fontIds)

      setExtractedItemsForShape(baselineShape, charRun)

      baselineShape
    }

    traceLog.trace { labeledShapes(spanLabel) tagged "Initial Font Baselines" }
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

}




// protected def inferNLBaselineContinuity(baselineShapes: Seq[LineShape]): Unit = {
//   pageIndex.shapes.ensureCluster(LB.ContiguousGlyphs)
//   for {     baselineShape <- baselineShapes   } {
//     val extractedItems = getExtractedItemsForShape(baselineShape)
//     val charItems = extractedItems.collect{ case i: ExtractedItem.CharItem =>  i }
//     val line = baselineShape.shape
//     val heights = charItems.map{ item =>
//       item.minBBox.height
//     }
//     // TODO get doc-wide avg or max height for these chars
//     val maxHeight = heights.max
//     val avgWidth = charItems.map{ _.minBBox.width.asDouble() }.sum / charItems.length
//     val windowWidth = avgWidth*6
//     val windowDelta = avgWidth*4
//     for {
//       baselineSlice <- pageHorizontalSlice(line.p1.y.asDouble()-maxHeight.asDouble(), maxHeight.asDouble())
//     } {

//       val windows = baselineSlice.slidingHorizontalWindow(windowWidth, windowDelta)
//       var firstNLGlyphWin = Int.MaxValue
//       windows.zipWithIndex.foreach { case (window, winNum) =>
//         val glyphsInWindowNL = searchForRects(window, LB.NatLangGlyph)
//         val glyphsInWindowSymbolic = searchForRects(window, LB.SymbolicGlyph)

//         val someGlyphIsNLOrRootedToNL = glyphsInWindowNL.nonEmpty || {
//           glyphsInWindowSymbolic.exists { glyphShape =>
//             pageIndex.shapes.getClusterRoot(LB.ContiguousGlyphs, glyphShape).isDefined
//           }
//         }

//         if (someGlyphIsNLOrRootedToNL) {
//           val glyphsInWindow = glyphsInWindowNL ++ glyphsInWindowSymbolic

//           val glyphItems = glyphsInWindow.map{ g =>
//             getExtractedItemsForShape(g).head
//           }

//           val ids = glyphItems.filter(_ != null)
//             .map{ g => g.id.unwrap }
//             .sorted
//             .toList

//           val idRange = (ids.min to ids.max).toList
//           val glyphsAreConsecutive = idRange == ids
//           if (glyphsAreConsecutive) {
//             firstNLGlyphWin = math.min(firstNLGlyphWin, winNum)
//             clusterN(LB.ContiguousGlyphs, glyphsInWindow)
//           }

//         }
//       }

//       val revWindows = windows.slice(0, firstNLGlyphWin+3).reverse

//       revWindows.zipWithIndex.foreach { case (window, winNum) =>
//         val glyphsInWindowNL = searchForRects(window, LB.NatLangGlyph)
//         val glyphsInWindowSymbolic = searchForRects(window, LB.SymbolicGlyph)

//         val someGlyphIsNLOrRootedToNL = glyphsInWindowNL.nonEmpty || {
//           glyphsInWindowSymbolic.exists { glyphShape =>
//             pageIndex.shapes.getClusterRoot(LB.ContiguousGlyphs, glyphShape).isDefined
//           }
//         }

//         if (someGlyphIsNLOrRootedToNL) {
//           val glyphsInWindow = glyphsInWindowNL ++ glyphsInWindowSymbolic

//           val glyphItems = glyphsInWindow.map{ g =>
//             getExtractedItemsForShape(g).head
//           }


//           val ids = glyphItems.filter(_ != null)
//             .map{ g => g.id.unwrap }
//             .sorted
//             .toList

//           val idRange = (ids.min to ids.max).toList

//           val glyphsAreConsecutive = idRange == ids

//           if (glyphsAreConsecutive) {
//             clusterN(LB.ContiguousGlyphs, glyphsInWindow)
//           }
//         }
//       }
//     }
//   }

//   val allClusterBounds = getClusteredRects(LB.ContiguousGlyphs).map{
//     case (clusterReprId, glyphRects) =>
//       val glyphBounds = glyphRects.map(_.shape).reduce(_ union _)
//       val glyphItems = getExtractedItemsForShapes(glyphRects).flatten
//       val continuousGlyphBaseline = glyphBounds.toLine(Dir.Bottom)
//       val baselineShape = indexShape(continuousGlyphBaseline, LB.ContiguousGlyphBaseline)
//       setExtractedItemsForShape(baselineShape, glyphItems)
//       glyphBounds
//   }
// }
