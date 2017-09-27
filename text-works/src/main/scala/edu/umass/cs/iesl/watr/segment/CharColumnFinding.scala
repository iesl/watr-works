package edu.umass.cs.iesl.watr
package segment


import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import TypeTags._

import utils.ExactFloats._
import segment.{SegmentationLabels => LB}
import utils.SlicingAndDicing._

import org.dianahep.{histogrammar => HST}
import spindex._
import extract.ExtractedItem

// import com.google.{common => guava}
// import guava.{collect => gcol}

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
    // initCharRunShapes()

    excludeImageRegionPoints()

    createBaselineLattice()

    // sweepJoinColinearCharRunShapes()
    // addLeftColumnShapes()

    implicit val log = createFnLog
    traceLog.drawPageShapes()
  }

  val colLeftHist = HST.SparselyBin.ing(0.10, {p: Point => p.x.asDouble()})
  val colRights = HST.SparselyBin.ing(1.0, {p: Point => p.x.asDouble()})

  def excludeImageRegionPoints(): Unit = {
    implicit val log = createFnLog
    pageIndex.components.getImageAtoms().foreach { img =>
      pageIndex.shapes.searchShapes(img.bounds, LB.ColLeftEvidence)
        .foreach { hit =>
          pageIndex.shapes.removeShape(hit)
        }
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


  def createBaselineLattice(): Unit = {
    createHPageRules().foreach { hPageRule =>
      // val currY = hPageRule.p1.y
      // Query horizontal 'corridor' of char-runs that might be part of the same line as this one
      val hCorridorHits = pageIndex.shapes.searchShapes(hPageRule, LB.CharRunBaseline)
        .map(_.asInstanceOf[LabeledShape[Line]])
        .sortBy(_.shape.p1.x)

      println(s"createBaselineLattice: hits = ${hCorridorHits.length}")

      pageIndex.shapes.ensureCluster(LB.CharRun)

      hCorridorHits.sliding(2).foreach { pairs =>
        pairs match {
          case Seq(run1, run2) =>
            val run1Items = pageIndex.shapes.getShapeAttribute[Seq[ExtractedItem]](run1.id, LB.ExtractedItems).get
            val run2Items = pageIndex.shapes.getShapeAttribute[Seq[ExtractedItem]](run2.id, LB.ExtractedItems).get

            val run1LastChar =  run1Items.last
            val run2FirstChar = run2Items.head
            val intermediateCharsIds = ((run1LastChar.id.unwrap+1) until run2FirstChar.id.unwrap ).toList

            val leftBounds = run1LastChar.bbox.toPoint(Dir.BottomLeft)
            val rightBounds = run2FirstChar.bbox.toPoint(Dir.BottomLeft)

            val intermediateChars = intermediateCharsIds.map { i =>
              val item = pageIndex.extractedItems(i)
              (item.bbox.toPoint(Dir.BottomLeft), item)
            }

            val inlineIntermediates = intermediateChars.takeWhile { case (p, _) =>
              leftBounds.x <= p.x && p.x < rightBounds.x
            }

            inlineIntermediates.foreach { case (p, item) =>
              val intBaseline = pageIndex.shapes.extractedItemShapes.get(item.id, LB.CharRun)
              pageIndex.shapes.union(LB.CharRun, run1, intBaseline)
            }

            val allIntermediatesAreInlined = intermediateChars.length == inlineIntermediates.length

            if (allIntermediatesAreInlined) {
              pageIndex.shapes.union(LB.CharRun, run1, run2)
            }

            // if (ints.length > 0) {
            //   println(s"(run1 + ints + run2)= ${run1Items.map(_.strRepr()).mkString}   +++   ${ints.mkString}   +++    ${run2Items.map(_.strRepr()).mkString}")
            // } else {
            //   println(s"(run1, run2)= ${run1Items.map(_.strRepr()).mkString}, ${run2Items.map(_.strRepr()).mkString}")
            // }

            // Seq()

          case Seq(run) =>
            val runItems = pageIndex.shapes.getShapeAttribute[Seq[ExtractedItem]](run.id, LB.ExtractedItems).get
            pageIndex.shapes.addCluster(LB.CharRun, Seq(run))
            println(s"(run)= ${runItems.map(_.strRepr()).mkString}")
            // Seq()

          case _ =>
            println(s"(())= ")
            // Seq()
        }
      }
    }


    val clusteredRuns = pageIndex.shapes.getClusterRoots(LB.CharRun)

    clusteredRuns.foreach { runShape =>
      val members = pageIndex.shapes.getClusterMembers(LB.CharRun, runShape)
      val baseLineMembers = members.get
        .map {_.asInstanceOf[LabeledShape[Line]]}

      println(s"clusteredRuns: runShape = ${runShape}, mem#=${baseLineMembers.length}")

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

      baseLineMembers.foreach { sh =>
        pageIndex.shapes.removeShape(sh)
      }

    }

    implicit val log = createFnLog
    traceLog.drawPageShapes()
  }


  def addLeftColumnShapes(): Unit = {
    implicit val log = createFnLog

    traceLog.drawPageShapes()

    colLeftHist.bins.toList
      .foreach{ case (bin, counting) =>
        val bw = colLeftHist.binWidth
        val colGuess = LTBounds.Doubles(left=(bin*bw).toDouble, top=0d, width=bw, pageGeometry.height.asDouble())
        val hits = pageIndex.shapes.searchShapes(colGuess, LB.ColLeftEvidence)

        if (hits.length < 2) {
          hits.foreach {  pageIndex.shapes.removeShape(_) }
        } else {
          val yvals = hits.map(_.shape.mbr.toPoint(Dir.BottomLeft).y)
          val miny = yvals.min
          val maxy = yvals.max
          val height = maxy - miny
          val colActual = LTBounds(left=(bin*bw).toFloatExact(), top=miny, width=bw.toFloatExact(), height)
          val intersectedRuns = pageIndex.shapes.searchShapes(colActual, LB.CharRunBaseline)
          if (intersectedRuns.isEmpty) {
            pageIndex.shapes.addShape(colActual, LB.LeftAlignedCharCol)

            traceLog.jsonAppend {
              showRegions(s"Column guess ", Seq(colActual))
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
