package edu.umass.cs.iesl.watr
package segment

import geometry._
import geometry.syntax._

import TypeTags._

import utils.ExactFloats._
import segment.{SegmentationLabels => LB}
import utils.SlicingAndDicing._

import org.dianahep.{histogrammar => HST}
import spindex._
import extract.ExtractedItem
import utils.FunctionalHelpers._


trait CharColumnFinding extends PageScopeSegmenter { self =>
  lazy val columnFinder = self

  import LB._

  /**
    *
    * Init: Shapes added before these functions are called
    *   ++ LB.CharRunBaseLine    : H-Line Shape - baseline running from first-last ll-corner in char run
    *   ++ LB.Image              : Rect Shape - image region
    *   ++ LB.CharRunBegin       : Ordering over Components
    *
    *
    * excludeImageRegionPoints() : Remove shapes that occur within image bounds (e.g. graph labels and textual data points)
    *   -- LB.ColLeftEvidence    : Point Shape
    *
    * sweepJoinColinearCharRunShapes()
    *   ++ LB.VisualLineItems:   : Disjoint Cluster - Group CharRunBegins s.t. each set is a visual line
    *   ++ LB.VisualBaseLine:    : H-Line Shape     - Horizontal baselines forming visual lines



    *   ++ LB.CharRunBegin       : Point Shape - Position of first char in a run (same as leftmost point in CharRunBaseLine)
    */


  def runColumnFinder(): Unit = {
    initialState()

    excludeImageRegionPoints()

    createBaselineClusters()

    createBaselineShapes()

    addColumnEvidence()

    // Find Baseline deviation up/down, for each font


    collectLineText()

    // Final Text State

  }

  def collectLineText(): Unit = {
    // Line chars are in:
    // val baselineClusterId =pageIndex.shapes.getRelation(visualBaseline, LB.HasCharRunBaselines)

    // getExtractedItemsForShape(runBaseline1)

  }

  def initialState(): Unit = {
    implicit val log = createFnLog
    traceLog.drawPageShapes()
  }


  protected def getExtractedItemsForShape(shape: LabeledShape[GeometricFigure]): Seq[ExtractedItem] = {
    pageIndex.shapes.getShapeAttribute[Seq[ExtractedItem]](shape.id, LB.ExtractedItems).get
  }


  def excludeImageRegionPoints(): Unit = {
    implicit val log = createFnLog

    getLabeledRects(LB.Image).foreach { img =>
      val baseLines = searchForLines(img.shape, LB.CharRunBaseline)
      deleteShapes(baseLines)
    }

    traceLog.drawPageShapes()
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
    }
    hPageRules
  }


  def createBaselineClusters(): Unit = {

    pageIndex.shapes.ensureCluster(LB.CharRunBaseline::Cluster)

    createHPageRules().foreach { hPageRule =>

      val ruleY = hPageRule.p1.y.asDouble()

      val queryRegion = pageHorizontalSlice(ruleY-1.0, 2.0).get

      // Query horizontal slice of char baseline runs that might be part of the same line as this one
      val hPageRuleHits = searchForLines(queryRegion, LB.CharRunBaseline)
        .sortBy(_.shape.p1.x)

      hPageRuleHits.sliding(2).foreach { pairs =>
        pairs match {
          case Seq(runBaseline1, runBaseline2) =>

            pageIndex.shapes.addCluster(LB.CharRunBaseline::Cluster, Seq(runBaseline1))
            pageIndex.shapes.addCluster(LB.CharRunBaseline::Cluster, Seq(runBaseline2))

            val run1Items = getExtractedItemsForShape(runBaseline1)
            val run2Items = getExtractedItemsForShape(runBaseline2)

            val run1LastChar =  run1Items.last
            val run2FirstChar = run2Items.head
            val intermediateCharsIds = ((run1LastChar.id.unwrap+1) until run2FirstChar.id.unwrap ).toList


            val intermediateChars = intermediateCharsIds.map { i =>
              val item = pageIndex.extractedItems(i)
              (item.location, item)
            }

            val leftBounds = run1LastChar.location
            val rightBounds = run2FirstChar.location

            val (inlineIntermediates, nonInlineIntermediates) = intermediateChars.span { case (p, _) =>
              leftBounds.x <= p.x && p.x < rightBounds.x
            }

            inlineIntermediates.foreach { case (p, extractedItem) =>
              val intBaseline = pageIndex.shapes.extractedItemShapes.get(extractedItem.id, LB.CharRun)
              pageIndex.shapes.union(LB.CharRunBaseline::Cluster, runBaseline1, intBaseline)
            }

            val allIntermediatesAreInlined = intermediateChars.length == inlineIntermediates.length

            if (allIntermediatesAreInlined) {
              pageIndex.shapes.union(LB.CharRunBaseline::Cluster, runBaseline1, runBaseline2)
            } else {
              nonInlineIntermediates.foreach { case(p, item) =>
                val intBaseline = pageIndex.shapes.extractedItemShapes.get(item.id, LB.CharRun)
                pageIndex.shapes.addCluster(LB.CharRunBaseline::Cluster, Seq(intBaseline))
              }

            }


          case Seq(run) =>
            pageIndex.shapes.addCluster(LB.CharRunBaseline::Cluster, Seq(run))

          case _ =>
        }
      }
    }
  }

  def createBaselineShapes(): Unit = {

    getClusteredLines(LB.CharRunBaseline::Cluster).foreach { case (baselineClusterId,  baseLineMembers) =>

      unindexShapes(baseLineMembers)

      val totalBounds = baseLineMembers.tail.foldLeft(
        baseLineMembers.head.shape.bounds()
      ){ case (acc, e) =>
          acc union e.shape.bounds()
      }

      val LTBounds(l, t, w, h) = totalBounds

      val (weight, runLines) = baseLineMembers
        .map { baseLineShape => (baseLineShape.shape.p1.y, baseLineShape.shape.length()) }
        .sortBy { _._1 }
        .groupByPairs { case (l1, l2) => l1._1 == l2._1}
        .map{ group => (group.map(_._2).sum, group) }
        .sortBy(_._1)
        .last

      runLines.headOption.foreach { case (yval, len) =>
        val likelyBaseline = Line(Point(l, yval), Point(l+w, yval))
        val visualBaseline = indexShape(likelyBaseline, LB.VisualBaseline)
        pageIndex.shapes.addRelation(visualBaseline.id, LB.HasCharRunBaselines, baselineClusterId)
      }

    }

    // pageIndex.components.setOrdering(l: Label, cs: Seq[Component])
    // pageIndex.shapes.setOrdering(LB.)
    // pageIndex.shapes.appendToOrdering(LB.VisualLine::Order, )


    implicit val log = createFnLog
    traceLog.drawPageShapes()
  }



  def addColumnEvidence(): Unit = {
    implicit val log = createFnLog

    val colLeftHist = HST.SparselyBin.ing(1.0, {p: Point => p.x.asDouble()})
    val colRightHist = HST.SparselyBin.ing(6.0, {p: Point => p.x.asDouble()})

    getLabeledLines(LB.VisualBaseline).foreach { charRunLine =>
      val Line(p1, p2) = charRunLine.shape
      indexShape(p1, LB.ColLeftEvidence)
      indexShape(p2, LB.ColRightEvidence)
      colLeftHist.fill(p1)
      colRightHist.fill(p2)
    }

    colLeftHist.bins.toList
      .filter { case (_, counting) => counting.entries > 1 }
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

          val intersectedRuns = searchForLines(colActual, LB.VisualBaseline)
            .sortBy(_.shape.p1.y)

          val hitsAndOverlaps = spanAllEithers(intersectedRuns, { charRunShape: LineShape =>
            val hitLeftX = charRunShape.shape.p1.x
            colActual.left <= hitLeftX
          })

          hitsAndOverlaps.foreach{ _ match {
            case Right(validCharRuns) =>

              if (validCharRuns.length > 1) {

                val yVals = validCharRuns.map(_.shape.p1.y)
                val topY = yVals.min
                val bottomY = yVals.max

                indexShape(Line(
                  Point(colActual.left, topY),
                  Point(colActual.left, bottomY)),
                  LB.LeftAlignedCharCol
                )
              }

            case _ =>
          }}
        }
      }


    colRightHist.bins.toList
      .filter { case (_, counting) => counting.entries > 0 }
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
            case Right(validCharRuns) =>

              if (validCharRuns.length > 1) {
                val yVals = validCharRuns.map(_.shape.p2.y)
                val topY = yVals.min
                val bottomY = yVals.max

                indexShape(Line(
                  Point(colActual.right, topY),
                  Point(colActual.right, bottomY)),
                  LB.RightAlignedCharCol
                )
              }

            case _ =>
          }}
        }
      }

    traceLog.drawPageShapes()
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



  // def initCharRunShapes(): Unit = {
  //   implicit val log = createFnLog

  //   for {
  //     runBeginCC <- pageIndex.components.getOrdering(LB.CharRunBegin)
  //   } {
  //     val point = runBeginCC.bounds.toPoint(Dir.BottomLeft)

  //     // pageIndex.shapes.indexShape(point, LB.ColLeftEvidence)

  //     val vCrossLine = point.translate(x=0.toFloatExact, y= 5.toFloatExact)
  //       .lineTo(point.translate(x=0.toFloatExact, y= -5.toFloatExact))

  //     val beginVLineShape = pageIndex.indexShape(vCrossLine, LB.CharRunBeginVLine)
  //     pageIndex.setShapeAttribute(beginVLineShape.id, LB.CharRunBeginVLineToBaseline, runBaseline)

  //     colLeftHist.fill(point)

  //   }

  //   traceLog.drawPageShapes()

  // }



}
