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
import watrmarks._


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


  val L = Label.apply(_, _)

  def runColumnFinder(): Unit = {
    // initPageComponents()
    // initHRuleShapes()
    initGridShapes()

    excludeImageRegionPoints()

    // createBaselineClusters()

    // createBaselineShapes()

    // addColumnEvidence()

    // Find Baseline deviation up/down, for each font

    collectLineText()

    // Final Text State

  }

  def collectLineText(): Unit = {
    val orderedLines = pageIndex.shapes.getOrdering(LB.VisualLine::Ordering)

    orderedLines.foreach { visualBaseline =>
      val baselineCluster = pageIndex.shapes.getRelation(visualBaseline, LB.HasCharRunBaselines).get

      val baselineMembers = pageIndex.shapes.getClusterMembers(LB.CharRunBaseline::Cluster, baselineCluster).get

      val extractedItems = getCharRunBaselineItems(baselineMembers.map(_.asInstanceOf[LineShape]))

      val clusterStr = extractedItems.map { extractedItems =>
        extractedItems.map(_.strRepr()).mkString
      }

      val lineText = clusterStr.mkString

      println(s"line>> ${lineText.mkString}")
    }

  }


  protected def getExtractedItemsForShape(shape: LabeledShape[GeometricFigure]): Seq[ExtractedItem] = {
    pageIndex.shapes.getShapeAttribute[Seq[ExtractedItem]](shape.id, LB.ExtractedItems).get
  }

  protected def getCharRunBaselineItems(baselineMembers: Seq[LineShape]): Seq[Seq[ExtractedItem]] = {
    baselineMembers.map {charRun =>
      println(s"charRun: ${charRun}")
      getExtractedItemsForShape(charRun)
    }
  }


  def excludeImageRegionPoints(): Unit = {
    implicit val log = createFnLog

    pageIndex.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .foreach { imageItem =>
        indexShape(imageItem.bbox, LB.Image)
        val baseLines = searchForLines(imageItem.bbox, LB.CharRunBaseline)
        deleteShapes(baseLines)
      }

    // getLabeledRects(LB.Image).foreach { img =>
    //   val baseLines = searchForLines(img.shape, LB.CharRunBaseline)
    //   deleteShapes(baseLines)
    // }

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
    }.groupByPairs(_.p1.y == _.p1.y).map(_.head)


    hPageRules
  }


  def createBaselineClusters(): Unit = {

    pageIndex.shapes.ensureCluster(LB.CharRunBaseline::Cluster)

    val hPageRules = createHPageRules()

    hPageRules.foreach { hPageRule =>

      val ruleY = hPageRule.p1.y.asDouble()

      val queryRegion = pageHorizontalSlice(ruleY-2.0, 4.0).get

      // indexShape(queryRegion, Label("QueryRegion"))

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
              cluster2(LB.CharRunBaseline::Cluster, runBaseline1, intBaseline)
            }

            val allIntermediatesAreInlined = intermediateChars.length == inlineIntermediates.length

            if (allIntermediatesAreInlined) {
              cluster2(LB.CharRunBaseline::Cluster, runBaseline1, runBaseline2)
            } else {
              nonInlineIntermediates.foreach { case(p, item) =>
                val intBaseline = pageIndex.shapes.extractedItemShapes.get(item.id, LB.CharRun)
                cluster1(LB.CharRunBaseline::Cluster, intBaseline)
              }

            }


          case Seq(run) =>
            cluster1(LB.CharRunBaseline::Cluster, run)

          case _ =>
        }
      }
    }


    hPageRules.sliding(2).foreach { _ match {
      case Seq(hRule1, hRule2) =>
      case Seq(hRule) =>
      case Seq() =>
    }}

  }


  def createBaselineShapes(): Unit = {
    pageIndex.shapes.reportClusters()

    val visualBaselinesWithMinId = getClusteredLines(LB.CharRunBaseline::Cluster)
      .map { case (baselineClusterId,  baseLineMembers) =>

        val extractedItems = getCharRunBaselineItems(baseLineMembers)

        val minItemID = extractedItems.map(_.map(_.id).min).min

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

        runLines.headOption.map { case (yval, len) =>
          val likelyBaseline = Line(Point(l, yval), Point(l+w, yval))
          val visualBaseline = indexShape(likelyBaseline, LB.VisualBaseline)
          pageIndex.shapes.addRelation(visualBaseline.id, LB.HasCharRunBaselines, baselineClusterId)
          (visualBaseline, minItemID)
        }
      }

    val sortedByExtractionOrder = visualBaselinesWithMinId.sortBy { _.map(_._2.unwrap).getOrElse(Int.MaxValue)  }
    sortedByExtractionOrder.foreach { ordered =>
      ordered.foreach{ case (visualBaseline, minId) =>
        pageIndex.shapes.appendToOrdering(LB.VisualLine::Ordering, visualBaseline)
      }
    }

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
    val hPageRules = createHorizontalPageRules(pageCharRuns)
    // hPageRules.foreach { pageRule =>
    //   indexShape(pageRule, LB.HPageDivider)
    // }


    pageCharRuns.foreach { charRun =>
      val loc = charRun.head.location
      val baseLine = createCharRunBaseline(charRun)
      indexShape(baseLine, LB.CharRunBaseline)
      indexShape(loc, LB.CharRunBegin)
    }

    implicit val log = createFnLog
    traceLog.drawPageShapes()
  }

  // private def initHRuleShapes(): Unit = {
  //   val pageCharRuns = findPageCharRuns()
  //   val hPageRules = createHorizontalPageRules(pageCharRuns)


  //   pageCharRuns.foreach { charRun =>


  //     val charRunY = charRun.head.location.y
  //     val matchingHRuler = hPageRules.find(_.p1.y == charRunY).getOrElse {
  //       sys.error("no matching h-page rule")
  //     }

  //     val initIndex = charRun.head.id.unwrap
  //     // Scan and align items left->right
  //     val endOffset = pageIndex.lastItemOffset

  //     var currIndex = initIndex+1
  //     var done = false

  //     while (!done) {
  //       val item = pageIndex.extractedItems(currIndex)
  //       val lastItem  = pageIndex.extractedItems(currIndex-1)
  //       val itemX = item.location.x
  //       val lastItemX = lastItem.location.x
  //       if (itemX >= lastItemX && currIndex < endOffset) {
  //         currIndex += 1
  //       } else {
  //         done = true
  //       }
  //     }
  //     val leftIncludes = pageIndex.extractedItems.slice(initIndex, currIndex)
  //     val leftStr = leftIncludes.map(_.strRepr()).mkString
  //     println(s"lft>${leftStr}")


  //     // val runBaseline = createCharRunBaseline(charRun)
  //     // val baselineShape = indexShape(runBaseline, LB.CharRunBaseline)

  //     // pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](baselineShape.id, LB.ExtractedItems, charRun)

  //     // charRun.foreach { _ match  {

  //     //   case item:ExtractedItem.CharItem =>
  //     //     if (item.charProps.isRunBegin) {
  //     //       // pageIndex.components.appendToOrdering(LB.CharRunBegin, cc)
  //     //     }

  //     //     pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, baselineShape)


  //     //   case item:ExtractedItem.ImgItem =>
  //     //     // pageIndex.shapes.indexShape(item.bbox, LB.Image)
  //     //     // val underline = item.bbox.toLine(Dir.Bottom)
  //     //     // val pathUnderline = indexShape(underline, LB.CharRunBaseline)
  //     //     // pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, pathUnderline)
  //     //     // // pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](pathUnderline.id, LB.ExtractedItems, run)

  //     //   case item:ExtractedItem.PathItem =>
  //     // }}
  //   }

  //   implicit val log = createFnLog
  //   traceLog.drawPageShapes()
  // }

  // private def initPageComponents(): Unit = {
  //   // import utils.{RelativeDirection => Dir}
  //   val pageCharRuns = findPageCharRuns()
  //   pageCharRuns.foreach { charRun =>
  //     val runBaseline = createCharRunBaseline(charRun)
  //     val baselineShape = indexShape(runBaseline, LB.CharRunBaseline)
  //     pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](baselineShape.id, LB.ExtractedItems, charRun)
  //     charRun.foreach { _ match  {
  //       case item:ExtractedItem.CharItem =>
  //         if (item.charProps.isRunBegin) {
  //           // pageIndex.components.appendToOrdering(LB.CharRunBegin, cc)
  //         }
  //         pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, baselineShape)
  //       case item:ExtractedItem.ImgItem =>
  //         // pageIndex.shapes.indexShape(item.bbox, LB.Image)
  //         // val underline = item.bbox.toLine(Dir.Bottom)
  //         // val pathUnderline = indexShape(underline, LB.CharRunBaseline)
  //         // pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, pathUnderline)
  //         // // pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](pathUnderline.id, LB.ExtractedItems, run)
  //       case item:ExtractedItem.PathItem =>
  //     }}
  //   }
  // }
}
