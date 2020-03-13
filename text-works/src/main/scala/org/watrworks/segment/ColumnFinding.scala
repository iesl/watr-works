package org.watrworks
package segment

import scala.{ collection => sc }
import sc.Seq

import geometry._
import geometry.syntax._
import TypeTags._

import utils.ExactFloats._
import utils.SlicingAndDicing._

import org.dianahep.{histogrammar => HST}
import utils.FunctionalHelpers._
import watrmarks._
import scala.collection.mutable


trait ColumnFinding extends PageScopeSegmenter
    with LineSegmentation { self =>

  lazy val columnFinder = self

  var pageVerticalJumps: mutable.ListBuffer[Int@@FloatRep] = mutable.ListBuffer()

  protected def addPageVerticalJumps(jumps: Seq[Int@@FloatRep]): Unit = {
    pageVerticalJumps ++= jumps
  }

  import LB._

  val clusterableShapeLabel = LB.BaselineMidriseBand

  def createColumnClusters(): Unit = {

    val charRunBaselineShapes = getLabeledRects(clusterableShapeLabel).map(_.shape.toLine(Dir.Bottom))
    val leftmostPoints = charRunBaselineShapes.map{ _.p1 }
    val rightmostPoints = charRunBaselineShapes.map{ _.p2 }
    clusterColumnPoints(leftmostPoints, LB.LeftAlignedCharCol, leftAlignedPoints=true)
    clusterColumnPoints(rightmostPoints, LB.RightAlignedCharCol, leftAlignedPoints=false)
  }

  protected def clusterColumnPoints(points: Seq[Point], asColumnLabel: Label, leftAlignedPoints: Boolean): Unit = {
    val pointHist = HST.SparselyBin.ing(1.4, {p: Point => p.x.asDouble()})
    val pageRight = pageGeometry.right
    val clusterLabel = asColumnLabel qualifiedAs Cluster
    val evidenceLabel = asColumnLabel qualifiedAs Evidence

    points.foreach{ point =>
      indexShape(point, evidenceLabel)
      pointHist.fill(point)
    }

    shapeIndex.ensureCluster(clusterLabel)

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

          traceLog.trace { figure(pageColumn) tagged "Candidate Page Slices" }

          val uniqYHits = hitPoints.uniqueBy(_.shape.y)


          if (uniqYHits.length > 1) {
            val yvals = uniqYHits.map(_.shape.y)
            val (maxy, miny) = (yvals.max,  yvals.min)
            val height = maxy - miny

            val colActual = pageColumn.getHorizontalSlice(miny, height).get
            traceLog.trace { figure(colActual) tagged s"VSlices ClippedTo YPoints min/max ${evidenceLabel}" }

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

                addPageVerticalJumps(
                  findDeltas(contiguousYValues.sorted)
                )

                traceLog.trace {
                  val columnMbr = columnPoints.map(minBoundingRect(_))reduce(_ union _)
                  figure(columnMbr) tagged s"${asColumnLabel} Final Shape"
                }

              case _ =>
            }}
          }
      }

    deleteLabeledShapes(evidenceLabel)
  }

}
