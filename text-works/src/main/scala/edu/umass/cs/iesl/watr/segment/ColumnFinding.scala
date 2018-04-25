package edu.umass.cs.iesl.watr
package segment

import geometry._
import geometry.syntax._
import TypeTags._

import utils.ExactFloats._
import segment.{SegmentationLabels => LB}
import utils.SlicingAndDicing._

import org.dianahep.{histogrammar => HST}
import utils.FunctionalHelpers._
import watrmarks._


trait ColumnFinding extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val columnFinder = self

  import LB._


  protected def createColumnClusters(): Unit = {
    val charRunBaselineShapes = getLabeledLines(LB.CharRunFontBaseline)
    val leftmostPoints = charRunBaselineShapes.map{ _.shape.p1 }
    val rightmostPoints = charRunBaselineShapes.map{ _.shape.p2 }
    clusterColumnPoints(leftmostPoints, LB.LeftAlignedCharCol, leftAlignedPoints=true)
    clusterColumnPoints(rightmostPoints, LB.RightAlignedCharCol, leftAlignedPoints=false)
  }

  protected def clusterColumnPoints(points: Seq[Point], label: Label, leftAlignedPoints: Boolean): Unit = {
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

}
