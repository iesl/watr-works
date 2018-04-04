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
import extract.LetterFrequencies
import watrmarks._
import utils.{RelativeDirection => Dir}
import utils.QuickNearestNeighbors._



trait CharColumnFinding extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val columnFinder = self

  import LB._

  def runPass1(): Unit = {

    markNatLangText()
    // initGridShapes()

    excludeImageRegionPoints()

    createBaselineClusters()

  }

  def runPass2(): Unit = {

    // createFontBaselineShapes() // => createLineMetrics

    // findEvenlySpacedTextBlocks()


  }

  def getTextGrid(): TextGrid = {
    val orderedLines = pageIndex.shapes.getOrdering(LB.FontBaseline::Ordering)

    // Reorder lines
    val reorderedLines = orderedLines.sortBy { baselineShape =>
      val items = getExtractedItemsForShape(baselineShape)
      items.head.id.unwrap
    }

    val rows = reorderedLines.map { visualBaseline =>
      createTextRowFromVisualLine(visualBaseline.asInstanceOf[LineShape])
    }


    TextGrid.fromRows(docScope.stableId,  rows)
  }


  def excludeImageRegionPoints(): Unit = {
    val deletedShapes = pageIndex.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .flatMap { imageItem =>
        indexShape(imageItem.minBBox, LB.Image)
        val baseLines: Seq[LineShape] = searchForLines(imageItem.minBBox, LB.CharRunBaseline)
        deleteShapes(baseLines)
        baseLines
      }

    traceLog.trace { shape(deletedShapes:_*) tagged "IntersectImageBounds" }
  }



  private def extendLineToCover(bbox: LTBounds, line: Line): Line = {
    line.extendLeftTo(bbox.left)
      .extendRightTo(bbox.right)
  }

  def createHPageRules(): Seq[Line] = {
    val charRunBaselines = getLabeledLines(LB.CharRunBaseline)
      .uniqueBy(_.shape.p1.y)

    val hPageRules = charRunBaselines.map { charRunBaseline =>
      extendLineToCover(pageGeometry, charRunBaseline.shape)
    }

    hPageRules
  }


  // HOTSPOT:toList
  def createBaselineClusters(): Unit = {

    pageIndex.shapes.ensureCluster(LB.CharRunBaseline::Cluster)

    val hPageRules = createHPageRules()

    hPageRules.foreach { hPageRule =>

      val ruleY = hPageRule.p1.y.asDouble()

      val queryRegion = pageHorizontalSlice(ruleY-2.0, 4.0).get

      // Query horizontal slice of char baseline runs that might be part of the same line as this one
      val hPageRuleHits = searchForLines(queryRegion, LB.CharRunBaseline)
        .sortBy(_.shape.p1.x)

      hPageRuleHits.sliding(2).foreach { pairs =>
        pairs match {
          case Seq(runBaseline1, runBaseline2) =>

            cluster1(LB.CharRunBaseline::Cluster, runBaseline1)
            cluster1(LB.CharRunBaseline::Cluster, runBaseline2)

            val run1Items = getExtractedItemsForShape(runBaseline1)
            val run2Items = getExtractedItemsForShape(runBaseline2)

            val run1LastChar =  run1Items.last
            val run2FirstChar = run2Items.head
            val intermediateCharsIds = ((run1LastChar.id.unwrap+1) to run2FirstChar.id.unwrap).toList

            if (intermediateCharsIds.nonEmpty) {

              val hasFewIntermediates = intermediateCharsIds.length < 10

              lazy val intermediateCharsStrictlyLeftToRight =
                intermediateCharsIds.zip(intermediateCharsIds.tail :+ run2FirstChar.id.unwrap)
                  .forall { case (id1, id2) =>
                    val item1 = pageIndex.extractedItems(id1)
                    val item2 = pageIndex.extractedItems(id2)
                    val nonNullIntermediates = item1  != null && item2 != null
                    val leftToRight = item1.location.x <= item2.location.x

                    nonNullIntermediates && leftToRight
                  }
              if (hasFewIntermediates && intermediateCharsStrictlyLeftToRight) {
                intermediateCharsIds.foreach { id =>
                    val intBaseline = pageIndex.shapes.extractedItemShapes.get(id, LB.CharRun)
                    if (intBaseline != null) {
                      cluster2(LB.CharRunBaseline::Cluster, runBaseline1, intBaseline)
                    }
                }

              }
            }


          case Seq(run) =>
            cluster1(LB.CharRunBaseline::Cluster, run)

          case _ =>
        }
      }
    }

    // traceLog.drawPageShapes()
  }

  def createFontBaselineShapes(): Unit = {
    getClusteredLines(LB.CharRunBaseline::Cluster)
      .foreach { case (baselineClusterId,  baseLineMembers) =>

        unindexShapes(baseLineMembers)

        val extractedItems = getCharRunBaselineItems(baseLineMembers)

        val sorted = baseLineMembers.sortBy(_.shape.p1.x)
        val totalBounds = sorted.head.shape.bounds.union(
          sorted.last.shape.bounds
        )

        val LTBounds(l, t, w, h) = totalBounds

        val (weight, runLines) = baseLineMembers
          .map { baseLineShape => (baseLineShape.shape.p1.y, baseLineShape.shape.length()) }
          .sortBy { _._1 }
          .groupByPairs { case (l1, l2) => l1._1 == l2._1}
          .map{ group => (group.map(_._2).sum, group) }
          .sortBy(_._1)
          .last

        runLines.headOption.map { case (yval, len) =>
          val likelyBaseline = Line(Point(l, yval), Point(l+w, yval))
          val shape = indexShape(likelyBaseline, LB.FontBaseline)
          setExtractedItemsForShape(shape, extractedItems.flatten)
          pageIndex.shapes.appendToOrdering(LB.FontBaseline::Ordering, shape)
        }
      }


    // traceLog.drawPageShapes()
  }
  /***
   *   Create: Baseline Midline Capline Ascentline Descentline
   *
   *   1. Find most likely font-y-baseline value
   *      - given all extracted items in line, give preference to
   *        most common natLang font per-document, per-page, at given determinant
   *
   *   2. With font-y-baseline:
   *   3.   map getGlyphMetrics over  (y-baseline extractedItems)
   *   4.   Take average (or use first) glyphMetric, translated to y-baseline, as the line's metrics
   *   5.   Index font-y-baseline (currently recorded as FontBaseline)
   *   5.   Record translated metrics as a property on font-y-baseline
   *
   **/
  // def createLineMetricShapes(): Unit = {
  //   getClusteredLines(LB.CharRunBaseline::Cluster)
  //     .foreach { case (baselineClusterId,  baseLineMembers) =>

  //       unindexShapes(baseLineMembers)


  //       val extractedItems = getCharRunBaselineItems(baseLineMembers).flatten

  //       // extractedItems.exists(isMostCommonTextFont (OnPage/InDocument))

  //       extractedItems.collect{ case item: ExtractedItem.CharItem =>
  //         docScope.fontDefs.getFont(item.fontProps.name).foreach { fontProps =>
  //           val itemDetIndex = item.glyphProps.scalingFactor
  //           fontProps.isNatLangFont()
  //         }
  //       }


  //       val sorted = baseLineMembers.sortBy(_.shape.p1.x)
  //       val totalBounds = sorted.head.shape.bounds.union(
  //         sorted.last.shape.bounds
  //       )

  //       val LTBounds(l, t, w, h) = totalBounds

  //       val (weight, runLines) = baseLineMembers
  //         .map { baseLineShape => (baseLineShape.shape.p1.y, baseLineShape.shape.length()) }
  //         .sortBy { _._1 }
  //         .groupByPairs { case (l1, l2) => l1._1 == l2._1}
  //         .map{ group => (group.map(_._2).sum, group) }
  //         .sortBy(_._1)
  //         .last

  //       runLines.headOption.map { case (yval, len) =>
  //         val likelyBaseline = Line(Point(l, yval), Point(l+w, yval))
  //         val shape = indexShape(likelyBaseline, LB.FontBaseline)
  //         setExtractedItemsForShape(shape, extractedItems)
  //         pageIndex.shapes.appendToOrdering(LB.FontBaseline::Ordering, shape)
  //       }
  //     }


  //   // traceLog.drawPageShapes()
  // }


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



  private def subdivideColumnClusters(): Unit = {
    val pageVdists = pageIndex.pageVerticalJumps
    val allBins = qnn(pageVdists, tolerance = 0.5d)

    val bins = allBins.filter(_.size() > 1)

    // println("subdivideColumnEvidence: PageVerticalJumps")
    // println(allBins.mkString("\n  ", "\n  ", "\n"))


    getClusteredLines(LB.LeftAlignedCharCol::Cluster).map{ case (clusterReprId, baselineShapes) =>

      val assignedBins = findPairwiseVerticalJumps[Line](baselineShapes, _.shape.p1.y)
        .map{ case (yJump, (l1, l2)) =>
          // Assign each vertical jump its assinged bin # from above qnn
          val binNum = bins.indexWhere { bin =>
            val (bmin, bmax) = bin.range()
            bmin <= yJump && yJump <= bmax
          }
          (binNum, yJump, (l1, l2))
        }

      // println(s"Assigned Bins")
      // assignedBins.foreach { case (binNum, yJump, (l1, l2)) =>
      //   println(s"    ${binNum}: ${yJump}")
      // }

      val lineGroups = assignedBins.groupByPairs(_._1 == _._1)

      lineGroups.foreach{ group =>
        val binNum = group.head._1
        if (binNum >= 0) {


          val lowerLines = group.map { case (binNum, yJump, (l1, l2)) =>
            l2.shape
          }
          val firstLine = group.head._3._1.shape

          val blockLines = firstLine +: lowerLines
          val rightX = blockLines.map(_.p2.x).max
          val leftX = blockLines.map(_.p1.x).min
          val top = firstLine.p1.y
          val bottom = lowerLines.last.p1.y
          val blockBounds = LTBounds(leftX, top, rightX-leftX, bottom-top)
          indexShape(blockBounds, LB.ReadingBlock)

          // println(s"Block ${blockBounds}")
          val l0 = group.head._3._1
          val ll = group.map { case (binNum, yJump, (l1, l2)) =>
            l2
          }

          // (l0 +: ll).foreach{ lineShape =>

          //   val items = getExtractedItemsForShape(lineShape)
          //   val lineStr = items.map(_.strRepr()).mkString
          //   println(s"   > ${lineStr}")
          // }


          // baselineShapes.foreach { lineShape =>
          //   val items = getExtractedItemsForShape(lineShape)
          //   val lineStr = items.map(_.strRepr()).mkString
          //   println(s"   > ${lineStr}")
          // }

          indexShape(Line(
            Point(leftX, top),
            Point(leftX, bottom)),
            LB.LeftAlignedCharCol
          )

        }
      }

    }

    // Try to combine vertically stacked reading blocks

    // traceLog.drawPageShapes()
  }

  protected def findEvenlySpacedTextBlocks(): Unit = {
    clusterFontBaselinesWithSharedLeftColumn()

    subdivideColumnClusters()
  }

  /**
    *
    */
  private def recordNatLangLineSpacing(baselineShapes: Seq[LineShape]): Unit = {

    val contextDeltas = baselineShapes.map { baselineShape: LineShape =>
      val lineX1 = baselineShape.shape.p1.x
      val lineX2 = baselineShape.shape.p2.x
      val lineY = baselineShape.shape.p1.y

      val maybePageColumn = pageVerticalSlice(lineX1.asDouble(), (lineX2-lineX1).asDouble())
      if (maybePageColumn.isEmpty) {
        println(s"Empty page columns")
        println("shape: " + baselineShape.shape)
        println("lineX1.asDouble(): "  + lineX1.asDouble())
        println("(lineX2-lineX1).asDouble()) " + (lineX2-lineX1).asDouble())

      }


      val pageColumn = pageVerticalSlice(lineX1.asDouble(), (lineX2-lineX1).asDouble()).get
      val (aboveLine, belowLine) = pageColumn.splitHorizontal(lineY)

      // val above = aboveLine.map { bbox =>
      //   val query = bbox.translate(x=0, y = -1.0)
      //   searchForLines(query, LB.CharRunBaseline)
      // } getOrElse { List() }

      val below = belowLine.map { bbox =>
        val query = bbox.translate(x=0, y = +1.0)
        searchForLines(query, LB.CharRunBaseline)
      } getOrElse { List() }

      val windowSize = 2

      // val winAbove = above.sortBy(_.shape.p1.y).reverse.take(windowSize)
      val winBelow = below.sortBy(_.shape.p1.y).take(windowSize)

      // val deltasAbove = winAbove.map { ctxLine => lineY - ctxLine.shape.p1.y }
      val deltasBelow = winBelow.map { ctxLine => ctxLine.shape.p1.y - lineY }
      deltasBelow
    }

    val yJumps = contextDeltas.flatten
    val yJumpClusters = qnn(yJumps, tolerance = 0.5d)

    val nonUniqueJumps = yJumpClusters.filter { bin =>
      bin.size() > 1
    }


    println(s"recordNatLangLineSpacing: Assigned Bins")
    println(nonUniqueJumps.mkString("\n  ", "\n  ", "\n"))

    // val pairwiseYJumps = findPairwiseVerticalJumps[Line](baselineShapes, _.shape.p1.y)
    // val yJumps = pairwiseYJumps.map { case (yJump, (l1, l2)) => yJump }

    // val yJumpClusters = qnn(yJumps, tolerance = 0.5d)

    // pairwiseYJumps.zipWithIndex.foreach { case ((yJump, (l1, l2)), i) =>
    //     println(s"    ${i}: ${yJump.asFloat}")
    //   }


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
      val windowWidth = avgWidth*4
      val windowDelta = avgWidth*2

      // println(s"baselineShape: ${baselineShape.shape}")
      // println(s"   maxHeight: ${maxHeight}")
      // println(s"   avgWidth: ${avgWidth}")

      for {
        baselineSlice <- pageHorizontalSlice(line.p1.y.asDouble()-maxHeight.asDouble(), maxHeight.asDouble())
        // (pre, _) = baselineSlice.splitVertical(line.p1.x)
        // bbox <- pre
      } {
        // println(s"baselineSlice: ${baselineSlice}")

        traceLog.trace {
          figure(baselineSlice) tagged "Sliding Window PageSlice"
        }

        val windows = baselineSlice.slidingHorizontalWindow(windowWidth, windowDelta)
        var firstNLGlyphWin = Int.MaxValue
        windows.zipWithIndex.foreach { case (window, winNum) =>
          val glyphsInWindowNL = searchForRects(window, LB.PageGlyphNL)
          val glyphsInWindowNonNL = searchForRects(window, LB.PageGlyphNonNL)

          val someGlyphIsNLOrRootedToNL = glyphsInWindowNL.nonEmpty || {
            glyphsInWindowNonNL.exists { glyphShape =>
              pageIndex.shapes.getClusterRoot(LB.ContiguousGlyphs, glyphShape).isDefined
            }
          }

          if (someGlyphIsNLOrRootedToNL) {
            val glyphsInWindow = glyphsInWindowNL ++ glyphsInWindowNonNL

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

            // if (pageIndex.pageNum.unwrap == 0) {
            //   traceLog.trace {
            //     val l = window.toLine(Dir.Left)
            //     val b = window.toLine(Dir.Bottom)
            //     figure(l, b) tagged "Sliding Glyph Window Forward Hit"
            //   }
            // }

          }
        }

        val revWindows = windows.slice(0, firstNLGlyphWin+3).reverse

        revWindows.zipWithIndex.foreach { case (window, winNum) =>
          val glyphsInWindowNL = searchForRects(window, LB.PageGlyphNL)
          val glyphsInWindowNonNL = searchForRects(window, LB.PageGlyphNonNL)

          val someGlyphIsNLOrRootedToNL = glyphsInWindowNL.nonEmpty || {
            glyphsInWindowNonNL.exists { glyphShape =>
              pageIndex.shapes.getClusterRoot(LB.ContiguousGlyphs, glyphShape).isDefined
            }
          }

          // if (pageIndex.pageNum.unwrap == 0) {
          //   traceLog.trace {
          //     val l = window.toLine(Dir.Left)
          //     val b = window.toLine(Dir.Bottom)
          //     figure(l, b) tagged "Sliding Glyph Window Reverse "
          //   }
          // }

          if (someGlyphIsNLOrRootedToNL) {
            val glyphsInWindow = glyphsInWindowNL ++ glyphsInWindowNonNL

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

    val contiguousGlyphClusters = pageIndex.shapes.getClusterRoots(LB.ContiguousGlyphs)
    val allClusterBounds = contiguousGlyphClusters.map{ rootShape =>
      val cluster = pageIndex.shapes.getClusterMembers(LB.ContiguousGlyphs, rootShape).get
      val clusterBounds = cluster.map{ shape => shape.asRectShape.shape }.reduce(_ union _)

      clusterBounds
    }

    traceLog.trace {
      val bottomLines = allClusterBounds.map(_.toLine(Dir.Bottom))
      figure(bottomLines:_*) tagged "ContiguousGlyphBounds"
    }
    // pageIndex.shapes.reportClusters()

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

          val intersectingBaselines = searchForLines(colActual, LB.CharRunBaseline)
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
                val evLine = Line(baselineShapes.head.shape.p1, baselineShapes.last.shape.p1)
                figure(columnMbr) tagged s"${label} Points MinBounds"
              }

            case _ =>
          }}
        }
      }

    deleteLabeledShapes(evidenceLabel)
  }

  private def clusterFontBaselinesWithSharedLeftColumn(): Unit = {
    val baseLineLeftPoints = getLabeledLines(LB.FontBaseline).map { charRunLine =>
      charRunLine.shape.p1
    }
    clusterColumnPoints(baseLineLeftPoints, LB.LeftAlignedColEnd, leftAlignedPoints=true)
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


  def createCharRunBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted = charRun.sortBy { _.minBBox.left }
    val runBeginPt =  Point(xSorted.head.minBBox.left, xSorted.head.fontBbox.bottom)
    val runEndPt = Point(xSorted.last.minBBox.right, xSorted.last.fontBbox.bottom)
    Line(runBeginPt, runEndPt)
  }

  private def initNatLangCharSpans(natLangCharRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    val baselineShapes = natLangCharRuns.map { charRun =>
      val baseLine = createCharRunBaseline(charRun.map(_.asInstanceOf[ExtractedItem.CharItem]))

      val baselineShape = indexShape(baseLine, LB.CharRunBaseline)

      charRun.foreach { item =>
        pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, baselineShape)
      }

      setExtractedItemsForShape(baselineShape, charRun)
      // addFontEvidence(charRun)
      baselineShape
    }.asLineShapes

    recordNatLangLineSpacing(baselineShapes)

    val leftmostPoints = baselineShapes.map{ _.shape.p1 }
    val rightmostPoints = baselineShapes.map{ _.shape.p2 }

    clusterColumnPoints(leftmostPoints, LB.LeftAlignedCharCol, leftAlignedPoints=true)
    clusterColumnPoints(rightmostPoints, LB.RightAlignedCharCol, leftAlignedPoints=false)

    println(s"Baseline shape count ${baselineShapes.length}")
    inferNLBaselineContinuity(baselineShapes)


    traceLog.trace { labeledShapes(LB.CharRunBaseline) }
  }

  // private def addFontEvidence(charRun: Seq[ExtractedItem.CharItem]): Unit = {
  //   // Init Font/char stats
  //   charRun.foreach { item =>
  //     item.char.foreach { c =>
  //       docScope.fontDefs.addNGramEvidence(item.fontName, pageNum, c)
  //     }
  //   }

  //   val runChars = charRun.flatMap(_.strRepr())

  //   val fontRuns = charRun.groupByPairs{
  //     case (i1, i2) => i1.fontName == i2.fontName
  //   }

  //   fontRuns.foreach { fontRun =>
  //     val headFont = fontRun.head.fontName

  //     if (charRun.length > 1) {
  //       runChars.sliding(2).foreach { ngram =>
  //         self.docScope.fontDefs.addNGramEvidence(headFont, pageNum, ngram.head, ngram.tail:_*)
  //       }
  //       if (charRun.length > 2) {
  //         runChars.sliding(3).foreach { ngram =>
  //           self.docScope.fontDefs.addNGramEvidence(headFont, pageNum, ngram.head, ngram.tail:_*)
  //         }

  //       }
  //     }
  //   }
  // }

  private def findPageCharRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    val charRuns = pageIndex.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .groupByPairs {
        case (item1, item2) =>
          val consecutive = item1.id.unwrap+1 == item2.id.unwrap
          val sameLine = item1.fontBbox.bottom == item2.fontBbox.bottom
          val sameFont = item1.fontProps == item2.fontProps

          consecutive && sameLine && sameFont
      }

    charRuns
  }

  private def markNatLangText(): Unit = {
    val pageCharRuns = findPageCharRuns()

    // filter char runs to those with natural-language bigrams/trigrams
    val markedNatLangRuns = pageCharRuns.map { run =>
      val str = run.map(_.char).mkString
      val hasBi = LetterFrequencies.hasCommonBigram(str)
      val hasTri = LetterFrequencies.hasCommonTrigram(str)
      val isNatLang = hasBi && hasTri

      if(isNatLang) Right(run) else Left(run)
    }

    val natLangCharRuns = markedNatLangRuns.collect {
      case Right(run) => run
    }

    val allNatLangFontNames = natLangCharRuns.flatMap{ run =>
      run.map{ item =>
        item.fontProps.name
      }
    }.toSet.toList.sorted.mkString("\n  ", "\n  ", "\n")

    // println(s"NatLang Font Names = ${allFontNames} ")


    natLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeForItems(item.minBBox, LB.PageGlyphNL, item)
      }
    }

    val nonNatLangCharRuns = markedNatLangRuns.collect {
      case Left(run) => run
    }

    val nonNatLangFontNames = nonNatLangCharRuns.flatMap{ run =>
      run.map{ item =>
        item.fontProps.name
      }
    }.toSet.toList.sorted.mkString("\n  ", "\n  ", "\n")

    println(s"Non - NL Font Names = ${nonNatLangFontNames} ")

    nonNatLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeForItems(item.minBBox, LB.PageGlyphNonNL, item)
      }
    }


    initNatLangCharSpans(natLangCharRuns)


  }
  private def initGridShapes(): Unit = {
    val pageCharRuns = findPageCharRuns()

    // divide char runs to those with natural-language bigrams
    val markedNatLangRuns = pageCharRuns.map { run =>
      val str = run.map(_.char).mkString
      val hasBi = LetterFrequencies.hasCommonBigram(str)
      val hasTri = LetterFrequencies.hasCommonTrigram(str)
      val isNatLang = hasBi && hasTri

      if(isNatLang) Right(run) else Left(run)
    }

    val natLangCharRuns = markedNatLangRuns.collect {
      case Right(run) => run
    }

    val allFontNames = natLangCharRuns.flatMap{ run =>
      run.map{ item =>
        item.fontProps.name
      }
    }.toSet.toList.sorted.mkString("\n  ", "\n  ", "\n")

    println(s"NatLang Font Names = ${allFontNames} ")


    natLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeForItems(item.minBBox, LB.PageGlyphNL, item)
      }
    }

    val nonNatLangCharRuns = markedNatLangRuns.collect {
      case Left(run) => run
    }

    val nonNatLangFontNames = nonNatLangCharRuns.flatMap{ run =>
      run.map{ item =>
        item.fontProps.name
      }
    }.toSet.toList.sorted.mkString("\n  ", "\n  ", "\n")

    println(s"Non - NL Font Names = ${nonNatLangFontNames} ")

    nonNatLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeForItems(item.minBBox, LB.PageGlyphNonNL, item)
      }
    }


    initNatLangCharSpans(natLangCharRuns)


  }

}
