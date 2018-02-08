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
// import watrmarks._
// import utils.{RelativeDirection => Dir}



trait CharColumnFinding extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val columnFinder = self

  import LB._

  def runPass1(): Unit = {
    initGridShapes()

    excludeImageRegionPoints()

    createBaselineClusters()

  }

  def runPass2(): Unit = {

    createFontBaselineShapes() // => createLineMetrics

    findEvenlySpacedTextBlocks()


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


    pageIndex.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .foreach { imageItem =>
        indexShape(imageItem.minBBox, LB.Image)
        val baseLines = searchForLines(imageItem.minBBox, LB.CharRunBaseline)
        deleteShapes(baseLines)
      }

    traceLog.drawPageShapes()
  }



  def createHPageRules(): Seq[Line] = {
    val pageRight = pageGeometry.right
    val pageLeft = pageGeometry.left

    val charRunBaselines = getLabeledLines(LB.CharRunBaseline)
      .uniqueBy(_.shape.p1.y)

    val hPageRules = charRunBaselines.map { charRunBaseline =>
      charRunBaseline.shape
        .extendLeftTo(pageLeft)
        .extendRightTo(pageRight)
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

    traceLog.drawPageShapes()
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


    traceLog.drawPageShapes()
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
  def createLineMetricShapes(): Unit = {
    getClusteredLines(LB.CharRunBaseline::Cluster)
      .foreach { case (baselineClusterId,  baseLineMembers) =>

        unindexShapes(baseLineMembers)


        val extractedItems = getCharRunBaselineItems(baseLineMembers).flatten

        // extractedItems.exists(isMostCommonTextFont (OnPage/InDocument))

        extractedItems.collect{ case item: ExtractedItem.CharItem =>
          docScope.fontDefs.getFont(item.fontName).foreach { fontProps =>
            val itemDetIndex = item.glyphProps.scalingFactor
            fontProps.isNatLangFont()
          }
        }


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
          setExtractedItemsForShape(shape, extractedItems)
          pageIndex.shapes.appendToOrdering(LB.FontBaseline::Ordering, shape)
        }
      }


    traceLog.drawPageShapes()
  }


  protected def findCommonVerticalJumps(points: Seq[Int@@FloatRep]): Seq[Int@@FloatRep] = {
    val sorted = points.sorted

    sorted.zip(sorted.tail)
      .map {case (upperY, lowerY) =>
        lowerY - upperY
      }
  }

  protected def findPairwiseVerticalJumps[G <: GeometricFigure](
    shapes: Seq[LabeledShape[G]], getY: (LabeledShape[G]) => Int@@FloatRep
  ): Seq[(Int@@FloatRep, (LabeledShape[G], LabeledShape[G]))] = {

    val sorted = shapes.sortBy { getY(_) }

    sorted.zip(sorted.tail)
      .map {case (upper, lower) =>
        val dist = getY(lower) - getY(upper)
        (dist, (upper, lower))
      }
  }

  // Filter out non-text lines


  def subdivideColumnClusters(): Unit = {
    val pageVdists = pageIndex.pageVerticalJumps
    val allBins = QuickNearestNeighbors.qnn(pageVdists, tolerance = 0.5d)

    val bins = allBins.filter(_.size() > 1)

    // println("subdivideColumnEvidence: PageVerticalJumps")
    // println(allBins.mkString("\n  ", "\n  ", "\n"))


    getClusteredLines(LB.LeftAlignedCharCol::Cluster).map{ case (clusterReprId, baselineShapes) =>

      val assignedBins = findPairwiseVerticalJumps[Line](baselineShapes, _.shape.p1.y)
        .map{ case (yJump, (l1, l2)) =>
          // Assign each jump a bin #
          val i = bins.indexWhere { bin =>
            val (bmin, bmax) = bin.range()
            bmin <= yJump && yJump <= bmax
          }
          (i, yJump, (l1, l2))
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
          indexShape(Line(
            Point(leftX, top),
            Point(leftX, bottom)),
            LB.LeftAlignedCharCol
          )

        }
      }

    }

    // Try to combine vertically stacked reading blocks

    traceLog.drawPageShapes()
  }

  def findEvenlySpacedTextBlocks(): Unit = {
    clusterLinesWithSharedLeftColumn()

    subdivideColumnClusters()
  }


  private def clusterLinesWithSharedLeftColumn(): Unit = {
    val colLeftHist = HST.SparselyBin.ing(1.0, {p: Point => p.x.asDouble()})
    // val colRightHist = HST.SparselyBin.ing(2.0, {p: Point => p.x.asDouble()})

    // add column Left/Right evidence points
    getLabeledLines(LB.FontBaseline).foreach { charRunLine =>
      val Line(p1, p2) = charRunLine.shape
      indexShape(p1, LB.ColLeftEvidence)
      // indexShape(p2, LB.ColRightEvidence)
      colLeftHist.fill(p1)
      // colRightHist.fill(p2)
    }

    val pageRight = pageGeometry.right

    pageIndex.shapes.ensureCluster(LB.LeftAlignedCharCol::Cluster)

    colLeftHist.bins.toList
      .filter { case (bin, counting) =>
        val binWidth = colLeftHist.binWidth
        val binRight = (bin+1)*binWidth
        counting.entries > 1 && binRight.toFloatExact() < pageRight
        true
      }
      .foreach{ case (bin, counting) =>
        val binWidth = colLeftHist.binWidth
        val binLeft = bin*binWidth
        val pageColumn = pageVerticalSlice(binLeft, binWidth).get
        val hitLeftColPointsx = searchForPoints(pageColumn, LB.ColLeftEvidence)

        deleteShapes(hitLeftColPointsx)

        val uniqYHits = hitLeftColPointsx.uniqueBy(_.shape.y)


        if (uniqYHits.length > 1) {
          val yvals = uniqYHits.map(_.shape.y)
          val (maxy, miny) = (yvals.max,  yvals.min)
          val height = maxy - miny

          // println(s"getHorizontalSlice: pageColumn: ${pageColumn}")
          // println(s"                  : miny: ${miny.pp()}, height: ${height.pp()}")
          val colActual = pageColumn.getHorizontalSlice(miny, height).get

          val intersectingBaselines = searchForLines(colActual, LB.FontBaseline)
            .sortBy(_.shape.p1.y)

          val hitsAndOverlaps = spanAllEithers(intersectingBaselines, { baselineShape: LineShape =>
            val hitLeftX = baselineShape.shape.p1.x
            colActual.left <= hitLeftX
          })


          hitsAndOverlaps.foreach{ _ match {
            case Right(baselineShapes) if baselineShapes.length > 1 =>
              clusterN(LB.LeftAlignedCharCol::Cluster, baselineShapes)

              val contiguousYValues = baselineShapes.map(_.shape.p1.y)

              pageIndex.addPageVerticalJumps(
                findCommonVerticalJumps(contiguousYValues)
              )

            case _ =>
          }}
        }
      }


    // colRightHist.bins.toList
    //   .filter { case (bin, counting) =>
    //     val binWidth = colLeftHist.binWidth
    //     val binRight = (bin+1)*binWidth
    //     counting.entries > 1 && binRight.toFloatExact() < pageRight
    //   }
    //   .foreach{ case (bin, counting) =>
    //     val binWidth = colRightHist.binWidth
    //     val binLeft = bin*binWidth
    //     val pageColumn = pageVerticalSlice(binLeft, binWidth).get

    //     val hits = searchForPoints(pageColumn, LB.ColRightEvidence)

    //     deleteShapes(hits)

    //     if (hits.length > 1) {
    //       val yvals = hits.map(_.shape.y)
    //       val (maxy, miny) = (yvals.max,  yvals.min)
    //       val height = maxy - miny

    //       val colActual = pageColumn.getHorizontalSlice(miny, height).get

    //       val intersectedRuns = searchForLines(colActual, LB.FontBaseline)
    //         .sortBy(_.shape.p1.y)

    //       val hitsAndOverlaps = spanAllEithers(intersectedRuns, { charRunShape: LineShape =>
    //         val hitRightX = charRunShape.shape.p2.x
    //         colActual.right >= hitRightX
    //       })

    //       hitsAndOverlaps.foreach{ _ match {
    //         case Right(baselineShapes) if baselineShapes.length > 1  =>
    //           clusterN(LB.RightAlignedCharCol::Cluster, baselineShapes)

    //         case _ =>
    //       }}
    //     }
    //   }
    deleteLabeledShapes(LB.ColRightEvidence)
    deleteLabeledShapes(LB.ColLeftEvidence)
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

  protected def findPageCharRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    val charRuns = pageIndex.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .groupByPairsWithIndex {
        case (itm1, itm2, i) =>
          val item1 = itm1.asInstanceOf[ExtractedItem.CharItem]
          val item2 = itm2.asInstanceOf[ExtractedItem.CharItem]
          val consecutive = item1.id.unwrap+1 == item2.id.unwrap
          val sameLine = item1.fontBbox.bottom == item2.fontBbox.bottom
          consecutive && sameLine
      }

    charRuns
  }

  def createCharRunBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    // val hasOutlier = charRun.exists{ charItem =>
    //   !(charItem.minBBox.isContainedBy(pageGeometry)
    //     && charItem.fontBbox.isContainedBy(pageGeometry))
    // }
    // if (hasOutlier) {

    //   val strs = charRun.map{ charItem =>
    //     val mbr = charItem.minBBox
    //     val fbr = charItem.fontBbox
    //     s"${charItem.char}:  mbr: ${mbr}, fbr: ${fbr}"
    //   }
    //   println("createCharRunBaseline")
    //   println(strs.mkString("\n  ", "\n  ", "\n"))

    // }
    val runBeginPt =  Point(charRun.head.fontBbox.left, charRun.head.fontBbox.bottom)
    val runEndPt = Point(charRun.last.fontBbox.right, charRun.last.fontBbox.bottom)
    Line(runBeginPt, runEndPt)
  }

  // def createHorizontalPageRules(charRuns: Seq[Seq[ExtractedItem]]): Seq[Line] = {

  //   val baselines = charRuns.map{ charRun =>
  //     val isChar  = charRun.head.isInstanceOf[ExtractedItem.CharItem]
  //     if (isChar) {
  //       createCharRunBaseline(charRun.map(_.asInstanceOf[ExtractedItem.CharItem])).some
  //     } else None
  //   }.flatten

  //   val hPageRules = baselines
  //     .sortBy(_.p1.y)
  //     .map { _.extendLeftTo(pageGeometry.left).extendRightTo(pageGeometry.right) }
  //     .groupByPairs(_.p1.y == _.p1.y) // uniquify
  //     .map(_.head)                    //  ...

  //   hPageRules
  // }

  // private def doCharMetricComputations(): Unit = {

  //   docScope.fontDefs.fontProperties.foreach{ fontProps =>
  //     println("Font properties")
  //     println(fontProps)
  //     // val bistr = fontProps.bigramEvidence. mkString("{\n  ", "\n  ", "\n}")
  //     // val tristr = fontProps.trigramEvidence. mkString("{\n  ", "\n  ", "\n}")
  //     val bistr = fontProps.bigramEvidence. mkString("{  ", ", ", "  }")
  //     val tristr = fontProps.trigramEvidence. mkString("{  ", ", ", "  }")
  //     println("Bigrams: ")
  //     println(bistr)
  //     println("Trigrams: ")
  //     println(tristr)

  //     val pageEvidence = fontProps.pagewiseEvidence. mkString("{\n  ", "\n  ", "\n}")
  //     println("PageEvidence: ")
  //     println(pageEvidence)


  //     val ds = fontProps.dets.sorted
  //       .toList.groupByPairs { case (a, b) =>
  //         math.abs(a - b) < 0.1
  //       }
  //       .map(_.head)
  //       .mkString(", ")
  //     println(s"Font Trans Dets: $ds")

  //     val _ = fontProps.inferredMetrics()
  //   }


  // }
  private def initGridShapes(): Unit = {
    val pageCharRuns = findPageCharRuns()
    pageCharRuns.foreach { charRun =>

      val baseLine = createCharRunBaseline(charRun.map(_.asInstanceOf[ExtractedItem.CharItem]))

      val baselineShape = indexShape(baseLine, LB.CharRunBaseline)

      charRun.foreach { item =>
        pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, baselineShape)
      }

      setExtractedItemsForShape(baselineShape, charRun)

      // Init Font/char stats
      charRun.foreach { item =>
        item.char.foreach { c =>
          docScope.fontDefs.addNGramEvidence(item.fontName, pageNum, c)
        }
      }

      val runChars = charRun.flatMap(_.strRepr())

      val fontRuns = charRun.groupByPairs{
        case (i1, i2) => i1.fontName == i2.fontName
      }

      fontRuns.foreach { fontRun =>
        val headFont = fontRun.head.fontName

        if (charRun.length > 1) {
          runChars.sliding(2).foreach { ngram =>
            self.docScope.fontDefs.addNGramEvidence(headFont, pageNum, ngram.head, ngram.tail:_*)
          }
          if (charRun.length > 2) {
            runChars.sliding(3).foreach { ngram =>
              self.docScope.fontDefs.addNGramEvidence(headFont, pageNum, ngram.head, ngram.tail:_*)
            }

          }
        }
      }
    }
    traceLog.drawPageShapes()
  }

}
