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


trait CharColumnFinding extends PageScopeSegmenter { self =>
  lazy val columnFinder = self

  /**
    *
    * Init: Shapes added before these functions are called
    *   ++ LB.CharRunBaseLine    : H-Line Shape - baseline running from first-last ll-corner in char run
    *   ++ LB.CharRunBeginVLine  : V-Line Shape
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

    createBaselineLattice()

    addColumnEvidence()
  }

  def initialState(): Unit = {
    implicit val log = createFnLog
    traceLog.drawPageShapes()
  }


  def excludeImageRegionPoints(): Unit = {
    implicit val log = createFnLog

    pageIndex.components.getImageAtoms()
      .foreach { img =>
        pageIndex.shapes.searchShapes(img.bounds, LB.CharRunBaseline)
          .foreach { hit => pageIndex.shapes.removeShape(hit) }
      }

    traceLog.drawPageShapes()
  }

  def createHPageRules(): Seq[Line] = {
    val pageRight = pageGeometry.right
    val pageLeft = pageGeometry.left
    val charRunBaselines = pageIndex
      .shapes.getShapesWithLabel(LB.CharRunBaseline)
      .map(_.asInstanceOf[LabeledShape[Line]])
      .sortBy(_.shape.p1.y)

    val hPageRules = charRunBaselines.map { charRunBaseline =>
      charRunBaseline.shape
        .extendRightTo(pageRight)
        .extendLeftTo(pageLeft)
    }
    hPageRules
  }


  def getExtractedItemsForShape(shape: LabeledShape[GeometricFigure]): Seq[ExtractedItem] = {
    pageIndex.shapes.getShapeAttribute[Seq[ExtractedItem]](shape.id, LB.ExtractedItems).get
  }


  def createBaselineLattice(): Unit = {
    createHPageRules().foreach { hPageRule =>

      // Query horizontal 'corridor' of char-runs that might be part of the same line as this one
      val hCorridorHits = pageIndex.shapes.searchShapes(hPageRule, LB.CharRunBaseline)
        .map(_.asInstanceOf[LabeledShape[Line]])
        .sortBy(_.shape.p1.x)

      pageIndex.shapes.ensureCluster(LB.CharRun)

      hCorridorHits.sliding(2).foreach { pairs =>
        pairs match {
          case Seq(run1, run2) =>

            pageIndex.shapes.addCluster(LB.CharRun, Seq(run1))
            pageIndex.shapes.addCluster(LB.CharRun, Seq(run2))

            val run1Items = getExtractedItemsForShape(run1)
            val run2Items = getExtractedItemsForShape(run2)

            val run1LastChar =  run1Items.last
            val run2FirstChar = run2Items.head
            val intermediateCharsIds = ((run1LastChar.id.unwrap+1) until run2FirstChar.id.unwrap ).toList

            val leftBounds = run1LastChar.location
            val rightBounds = run2FirstChar.location

            val intermediateChars = intermediateCharsIds.map { i =>
              val item = pageIndex.extractedItems(i)
              (item.location, item)
            }


            val (inlineIntermediates, nonInlineIntermediates) = intermediateChars.span { case (p, _) =>
              leftBounds.x <= p.x && p.x < rightBounds.x
            }

            inlineIntermediates.foreach { case (p, item) =>
              val intBaseline = pageIndex.shapes.extractedItemShapes.get(item.id, LB.CharRun)
              pageIndex.shapes.union(LB.CharRun, run1, intBaseline)
            }

            val allIntermediatesAreInlined = intermediateChars.length == inlineIntermediates.length

            if (allIntermediatesAreInlined) {
              pageIndex.shapes.union(LB.CharRun, run1, run2)
            } else {
              nonInlineIntermediates.foreach { case(p, item) =>
                val intBaseline = pageIndex.shapes.extractedItemShapes.get(item.id, LB.CharRun)
                pageIndex.shapes.addCluster(LB.CharRun, Seq(intBaseline))
              }

            }


          case Seq(run) =>
            pageIndex.shapes.addCluster(LB.CharRun, Seq(run))

          case _ =>
        }
      }
    }


    val clusteredRuns = pageIndex.shapes.getClusterRoots(LB.CharRun)

    clusteredRuns.foreach { runShape =>
      val members = pageIndex.shapes.getClusterMembers(LB.CharRun, runShape)
      val baseLineMembers = members.get
        .map {_.asInstanceOf[LabeledShape[Line]]}

      // baseLineMembers.foreach { sh =>
      //   pageIndex.shapes.removeShape(sh)
      // }
      // println(s"clusteredRuns: runShape = ${runShape}, mem#=${baseLineMembers.length}")

      val totalBounds = baseLineMembers.tail.foldLeft(
        baseLineMembers.head.shape.bounds()
      ){ case (acc, e) =>
          acc union e.shape.bounds()
      }

      val LTBounds(l, t, w, h) = totalBounds

      val (weight, runLines) = baseLineMembers
        .map { baseLineShape =>
          (baseLineShape.shape.p1.y, baseLineShape.shape.length())
        }
        .sortBy{_._1}
        .groupByPairs { case (l1, l2) => l1._1 == l2._1}
        .map{ group => (group.map(_._2).sum, group) }
        .sortBy(_._1)
        .last

      runLines.headOption.foreach { case (yval, len) =>
        val likelyBaseline = Line(Point(l, yval), Point(l+w, yval))
        pageIndex.shapes.addShape(likelyBaseline, LB.CharRun)
      }


    }

    implicit val log = createFnLog
    traceLog.drawPageShapes()
  }


  def addColumnEvidence(): Unit = {
    implicit val log = createFnLog

    val colLeftHist = HST.SparselyBin.ing(1.0, {p: Point => p.x.asDouble()})
    val colRightHist = HST.SparselyBin.ing(6.0, {p: Point => p.x.asDouble()})

    pageIndex
      .shapes.getShapesWithLabel(LB.CharRun)
      .map(_.asInstanceOf[LabeledShape[Line]])
      .foreach { charRunLine =>
        val Line(p1, p2) = charRunLine.shape
        pageIndex.shapes.addShape(p1, LB.ColLeftEvidence)
        pageIndex.shapes.addShape(p2, LB.ColRightEvidence)
        colLeftHist.fill(p1)
        colRightHist.fill(p2)
      }

    // import org.dianahep.histogrammar.ascii._
    // println(colLeftHist.ascii)

    colLeftHist.bins.toList
      .filter { case (_, counting) => counting.entries > 1 }
      // .sortBy { case (bin, counting) => counting.entries }
      .foreach{ case (bin, counting) =>
        val bw = colLeftHist.binWidth
        val colGuess = LTBounds.Doubles(left=(bin*bw).toDouble, top=0d, width=bw, pageGeometry.height.asDouble())
        val hits = pageIndex.shapes.searchShapes(colGuess, LB.ColLeftEvidence)
          .map {_.asInstanceOf[LabeledShape[Point]]}

        if (hits.length < 2) {
          hits.foreach {  pageIndex.shapes.removeShape(_) }
        } else {
          val yvals = hits.map(_.shape.y)
          val miny = yvals.min
          val maxy = yvals.max
          val height = maxy - miny
          val colActual = LTBounds(left=(bin*bw).toFloatExact(), top=miny, width=bw.toFloatExact(), height)

          val intersectedRuns = pageIndex.shapes.searchShapes(colActual, LB.CharRun)
            .map(_.asInstanceOf[LabeledShape[Line]])
            .sortBy(_.shape.p1.y)

          val hitsAndOverlaps = intersectedRuns
            .map{ charRunShape =>
              val hitLeftX = charRunShape.shape.p1.x
              if (colActual.left <= hitLeftX) Right(charRunShape)
              else Left(charRunShape)
            }

          hitsAndOverlaps.groupByPairs { case (sh1, sh2) =>
            sh1.isLeft && sh2.isLeft || sh1.isRight && sh2.isRight
          }.map{ grouped =>
            if (grouped.head.isLeft) {
              grouped.map(_.left.get).map{ leftShape =>
                pageIndex.shapes.removeShape(leftShape)
              }
            }

            if (grouped.head.isRight) {
              val validCharRuns = grouped.map(_.right.get)

              if (validCharRuns.length < 2) {
                validCharRuns.foreach { sh =>
                  pageIndex.shapes.removeShape(sh)
                }
              } else {

                val yVals = validCharRuns.map(_.shape.p1.y)
                val topY = yVals.min
                val bottomY = yVals.max

                val leftColLine = Line(
                  Point(colActual.left, topY),
                  Point(colActual.left, bottomY)
                )
                pageIndex.shapes.addShape(leftColLine, LB.LeftAlignedCharCol)
              }

            }
          }
        }
      }

    colRightHist.bins.toList
      .filter { case (_, counting) => counting.entries > 1 }
      .foreach{ case (bin, counting) =>
        val bw = colRightHist.binWidth
        val colGuess = LTBounds.Doubles(left=(bin*bw).toDouble, top=0d, width=bw, pageGeometry.height.asDouble())
        val hits = pageIndex.shapes.searchShapes(colGuess, LB.ColRightEvidence)
          .map {_.asInstanceOf[LabeledShape[Point]]}

        if (hits.length < 2) {
          hits.foreach {  pageIndex.shapes.removeShape(_) }
        } else {
          val yvals = hits.map(_.shape.y)
          val miny = yvals.min
          val maxy = yvals.max
          val height = maxy - miny
          val colActual = LTBounds(left=(bin*bw).toFloatExact(), top=miny, width=bw.toFloatExact(), height)

          val intersectedRuns = pageIndex.shapes.searchShapes(colActual, LB.CharRun)
            .map(_.asInstanceOf[LabeledShape[Line]])
            .sortBy(_.shape.p1.y)

          val hitsAndOverlaps = intersectedRuns
            .map{ charRunShape =>
              val hitRightX = charRunShape.shape.p2.x
              if (colActual.right >= hitRightX) Right(charRunShape)
              else Left(charRunShape)
            }

          hitsAndOverlaps.groupByPairs { case (sh1, sh2) =>
            sh1.isLeft && sh2.isLeft || sh1.isRight && sh2.isRight
          }.map{ grouped =>
            if (grouped.head.isLeft) {
              grouped.map(_.left.get).map{ leftShape =>
                pageIndex.shapes.removeShape(leftShape)
              }
            }

            if (grouped.head.isRight) {
              val validCharRuns = grouped.map(_.right.get)

              if (validCharRuns.length < 2) {
                validCharRuns.foreach { sh =>
                  pageIndex.shapes.removeShape(sh)
                }
              } else {

                val yVals = validCharRuns.map(_.shape.p2.y)
                val topY = yVals.min
                val bottomY = yVals.max

                val leftColLine = Line(
                  Point(colActual.right, topY),
                  Point(colActual.right, bottomY)
                )
                pageIndex.shapes.addShape(leftColLine, LB.RightAlignedCharCol)
              }

            }
          }
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

  //     // pageIndex.shapes.addShape(point, LB.ColLeftEvidence)

  //     val vCrossLine = point.translate(x=0.toFloatExact, y= 5.toFloatExact)
  //       .lineTo(point.translate(x=0.toFloatExact, y= -5.toFloatExact))

  //     val beginVLineShape = pageIndex.addShape(vCrossLine, LB.CharRunBeginVLine)
  //     pageIndex.setShapeAttribute(beginVLineShape.id, LB.CharRunBeginVLineToBaseline, runBaseline)

  //     colLeftHist.fill(point)

  //   }

  //   traceLog.drawPageShapes()

  // }



}
