package edu.umass.cs.iesl.watr
package segment

import geometry._
import geometry.syntax._

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

    createBaselineShapes()

    collectColumnEvidence()
  }

  def runPass2(): Unit = {

    subdivideColumnEvidence()

  }

  def getTextGrid(): TextGrid = {
    val orderedLines = pageIndex.shapes.getOrdering(LB.VisualBaseline::Ordering)

    // Reorder lines
    val reorderedLines = orderedLines.sortBy { baselineShape =>
      val items = getExtractedItemsForShape(baselineShape)
      items.head.id.unwrap
    }

    val rows = reorderedLines.map { visualBaseline =>
      createTextRowFromVisualLine(visualBaseline.asInstanceOf[LineShape])
    }

    TextGrid.fromRows(rows)
  }




  def excludeImageRegionPoints(): Unit = {

    pageIndex.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .foreach { imageItem =>
        indexShape(imageItem.bbox, LB.Image)
        val baseLines = searchForLines(imageItem.bbox, LB.CharRunBaseline)
        deleteShapes(baseLines)
      }

  }

  def createHPageRules(): Seq[Line] = {
    val pageRight = pageGeometry.right
    val pageLeft = pageGeometry.left

    val charRunBaselines = getLabeledLines(LB.CharRunBaseline)
      .sortBy(_.shape.p1.y)

    val hPageRules = charRunBaselines.map { charRunBaseline =>
      charRunBaseline.shape
        .extendLeftTo(pageLeft)
        .extendRightTo(pageRight)
    }.groupByPairs(_.p1.y == _.p1.y).map(_.head)


    hPageRules
  }


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

  def createBaselineShapes(): Unit = {
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
          val shape = indexShape(likelyBaseline, LB.VisualBaseline)
          setExtractedItemsForShape(shape, extractedItems.flatten)
          pageIndex.shapes.appendToOrdering(LB.VisualBaseline::Ordering, shape)
        }
      }


    traceLog.drawPageShapes()
  }


  protected def findCommonVerticalJumps(points: Seq[PointShape]): Seq[(Int@@FloatRep, PointShape)] = {
    val sorted = points.sortBy { _.shape.y }

    sorted.zip(sorted.tail)
      .map {case (upper, lower) =>
        val dist = lower.shape.y - upper.shape.y
        (dist, upper)
      }
  }

  // Divide Left-column lines into shorter lines that cover evenly spaced blocks of lines
  def subdivideColumnEvidence(): Unit = {
    val pageVdists = pageIndex.pageVerticalJumps
    val bins = QuickNearestNeighbors.qnn(pageVdists)

    // println(bins.mkString("\n  ", "\n  ", "\n"))

    val baselineShapeClusters = getClusteredLines(LB.LeftAlignedCharCol::Cluster)

    baselineShapeClusters.foreach { case (clusterReprId, baselineShapes) =>
      val leftXVals = baselineShapes.map(_.shape.p1.x)
      val leftX = leftXVals.min
      val yVals = baselineShapes.map(_.shape.p1.y)
      val topY = yVals.min
      val bottomY = yVals.max

      indexShape(Line(
        Point(leftX, topY),
        Point(leftX, bottomY)),
        LB.LeftAlignedCharCol
      )
    }

    traceLog.drawPageShapes()
  }

  def collectColumnEvidence(): Unit = {

    val colLeftHist = HST.SparselyBin.ing(1.0, {p: Point => p.x.asDouble()})
    val colRightHist = HST.SparselyBin.ing(2.0, {p: Point => p.x.asDouble()})

    // add column Left/Right evidence points
    getLabeledLines(LB.VisualBaseline).foreach { charRunLine =>
      val Line(p1, p2) = charRunLine.shape
      indexShape(p1, LB.ColLeftEvidence)
      indexShape(p2, LB.ColRightEvidence)
      colLeftHist.fill(p1)
      colRightHist.fill(p2)
    }


    // var pageVdists: Seq[Int@@FloatRep] = List()
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
        val hits = searchForPoints(pageColumn, LB.ColLeftEvidence)

        deleteShapes(hits)

        if (hits.length > 1) {
          val yvals = hits.map(_.shape.y)
          val (maxy, miny) = (yvals.max,  yvals.min)
          val height = maxy - miny

          val colActual = pageColumn.getHorizontalSlice(miny, height).get

          val intersectingBaselines = searchForLines(colActual, LB.VisualBaseline)
            .sortBy(_.shape.p1.y)

          val hitsAndOverlaps = spanAllEithers(intersectingBaselines, { baselineShape: LineShape =>
            val hitLeftX = baselineShape.shape.p1.x
            colActual.left <= hitLeftX
          })


          hitsAndOverlaps.foreach{ _ match {
            case Right(baselineShapes) if baselineShapes.length > 1 =>
                clusterN(LB.LeftAlignedCharCol::Cluster, baselineShapes)

            case _ =>
          }}


          pageIndex.addPageVerticalJumps(
            findCommonVerticalJumps(hits).map(_._1)
          )

        }
      }


    colRightHist.bins.toList
      .filter { case (bin, counting) =>
        val binWidth = colLeftHist.binWidth
        val binRight = (bin+1)*binWidth
        counting.entries > 1 && binRight.toFloatExact() < pageRight
      }
      .foreach{ case (bin, counting) =>
        val binWidth = colRightHist.binWidth
        val binLeft = bin*binWidth
        val pageColumn = pageVerticalSlice(binLeft, binWidth).get

        val hits = searchForPoints(pageColumn, LB.ColRightEvidence)

        deleteShapes(hits)

        if (hits.length > 1) {
          val yvals = hits.map(_.shape.y)
          val (maxy, miny) = (yvals.max,  yvals.min)
          val height = maxy - miny

          val colActual = pageColumn.getHorizontalSlice(miny, height).get

          val intersectedRuns = searchForLines(colActual, LB.VisualBaseline)
            .sortBy(_.shape.p1.y)

          val hitsAndOverlaps = spanAllEithers(intersectedRuns, { charRunShape: LineShape =>
            val hitRightX = charRunShape.shape.p2.x
            colActual.right >= hitRightX
          })

          hitsAndOverlaps.foreach{ _ match {
            case Right(baselineShapes) if baselineShapes.length > 1  =>
              clusterN(LB.RightAlignedCharCol::Cluster, baselineShapes)

            case _ =>
          }}
        }
      }


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

  protected def findPageCharRuns(): Seq[Seq[ExtractedItem]] = {
    val charRuns = pageIndex.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.CharItem] }
      .groupByPairsWithIndex {
        case (item1, item2, i) =>
          val consecutive = item1.id.unwrap+1 == item2.id.unwrap
          val sameLine = item1.bbox.bottom == item2.bbox.bottom
          consecutive && sameLine
      }

    charRuns
  }

  def createCharRunBaseline(charRun: Seq[ExtractedItem]): Line = {
    val runBeginPt =  Point(charRun.head.bbox.left, charRun.head.bbox.bottom)
    val runEndPt = Point(charRun.last.bbox.right, charRun.last.bbox.bottom)
    Line(runBeginPt, runEndPt)
  }

  def createHorizontalPageRules(charRuns: Seq[Seq[ExtractedItem]]): Seq[Line] = {

    val baselines = charRuns.map{ charRun =>
      val isChar  = charRun.head.isInstanceOf[ExtractedItem.CharItem]
      if (isChar) {
        createCharRunBaseline(charRun).some
      } else None
    }.flatten

    val hPageRules = baselines
      .sortBy(_.p1.y)
      .map { _.extendLeftTo(pageGeometry.left).extendRightTo(pageGeometry.right) }
      .groupByPairs(_.p1.y == _.p1.y) // uniquify
      .map(_.head)                    //  ...

    hPageRules
  }

  private def initGridShapes(): Unit = {
    val pageCharRuns = findPageCharRuns()
    pageCharRuns.foreach { charRun =>
      val baseLine = createCharRunBaseline(charRun)
      val baselineShape = indexShape(baseLine, LB.CharRunBaseline)

      // val loc = charRun.head.location
      // val endLoc = charRun.last.bbox.toPoint(Dir.BottomRight)
      // indexShape(loc, LB.CharRunBegin)
      // indexShape(endLoc, Label("CharRunEnd"))

      charRun.foreach { _ match  {
        case item:ExtractedItem.CharItem =>
          pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, baselineShape)
      }}

      pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](baselineShape.id, LB.ExtractedItems, charRun)
    }
    traceLog.drawPageShapes()
  }


  // private def initPageDividers(): Unit = {
  //   val ptop = pageGeometry.toPoint(Dir.Top)
  //   val pbot = pageGeometry.toPoint(Dir.Bottom)
  //   val vline = Line(ptop, pbot)
  //   indexShape(vline, Label("PageVertical"))
  // }

}
