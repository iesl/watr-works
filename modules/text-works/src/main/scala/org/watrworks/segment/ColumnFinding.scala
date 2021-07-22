package org.watrworks
package segment

import geometry._
import geometry.syntax._

import utils.{Direction => Dir}
import utils.{M3x3Position => M3}
import utils.ExactFloats._
import utils.Interval
import Interval._
import cats.implicits._
import utils.SlicingAndDicing._

case class ColumnInst(
  pageBand: Rect,
  pageNum: Int @@ PageNum,
  points: List[Int @@ FloatRep],
  fontIds: List[String @@ ScaledFontID]
)

case class ColumnClass(
  ranges: List[Interval.FloatExacts]
)

case class ColumnProps(
  guttersEmpty: Boolean,
  col1Empty: Boolean,
  col2Empty: Boolean
)

trait ColumnFinding extends NeighborhoodSearch { self =>

  lazy val columnFinder = self

  def findColumnEvidence(): Unit = {
    val initShapes    = getLabeledShapes[Rect](LB.MonoFontTextLattice)
    val sortedShapes  = sortShapesByFontOccurrence(initShapes)
    val queryDistance = self.pageGeometry.width

    sortedShapes.foreach({ case (focalShapes, fontId) =>
      focalShapes.foreach(focalShape =>
        withUnoccludedShapes(
          Dir.Right,
          focalShape,
          LB.MonoFontTextLattice,
          queryDistance,
          hitFilter = (hit) => hasFont(hit, fontId),
          callback = (hits) => {

            val focalRect = focalShape.shape
            for {
              hitShape <- hits
              hitRect = hitShape.shape
              (sepRect, pos @ _) <- focalRect.minSeparatingRect(hitRect)
              pageBand <- sepRect
                            .withinRegion(pageGeometry)
                            .adjacentRegions(M3.Left, M3.Center, M3.Right)
              _ = traceLog.trace { createLabelOn("PageBand", pageBand) }
              leftGutterQueryRect <- focalRect
                                       .withinRegion(pageBand)
                                       .adjacentRegion(M3.Left)
                                       .map(_.shave(FloatExact.epsilon))
              _ = traceLog.trace { createLabelOn("LeftGutterQuery", leftGutterQueryRect) }
              rightGutterQueryRect <- hitRect
                                        .withinRegion(pageBand)
                                        .adjacentRegion(M3.Right)
                                        .map(_.shave(FloatExact.epsilon))
              _ = traceLog.trace { createLabelOn("RightGutterQuery", rightGutterQueryRect) }

            } {
              val leftGutterHits  = searchForShapes[GeometricFigure](leftGutterQueryRect)
              val leftGutterEmpty = leftGutterHits.isEmpty

              val rightGutterHits  = searchForShapes[GeometricFigure](rightGutterQueryRect)
              val rightGutterEmpty = rightGutterHits.isEmpty

              val leftRightGuttersEmpty = leftGutterEmpty && rightGutterEmpty

              val colEvidence = ColumnInst(
                pageBand,
                pageNum,
                points = List(
                  leftGutterQueryRect.right,
                  sepRect.left,
                  sepRect.right,
                  rightGutterQueryRect.left
                ),
                fontIds = List(
                  fontId,
                  fontId
                )
              )

              docScope.docStats.columns.evidence.append(colEvidence)

              traceLog.trace {
                createLabel(s"ColumnEvidence: ${leftRightGuttersEmpty}")
                  .withProp("class", ">lazy")
                  .withChildren(
                    createLabelOn("PageBand", pageBand)
                      .withProp("class", "=eager"),
                    createLabelOn("LeftGutter", leftGutterQueryRect)
                      .withChildren(
                        createLabelsOnShapes("LGHits", leftGutterHits)
                      ),
                    createLabelOn("MiddleGutter", sepRect),
                    createLabelOn("RightGutter", rightGutterQueryRect)
                      .withChildren(
                        createLabelsOnShapes("RGHits", rightGutterHits)
                      )
                  )

              }

            }
          }
        )
      )
    })
  }

  def applyColumnEvidence(): Unit = {
    val columnClusters    = docScope.docStats.columns.clusters
    val pageRight         = pageGeometry.right
    val pageLeftRange     = Interval.FloatExacts(0.toFloatExact(), 0.toFloatExact())
    val pageRightRange    = Interval.FloatExacts(pageRight, pageRight)
    val queryRectHeight   = 10.toFloatExact() // TODO 10 is approximate glyph height
    val queryLRSlopFactor = 1.toFloatExact()

    val queries: Seq[Option[Seq[Rect]]] = for {
      cluster <- columnClusters.toSeq
    } yield {
      val ranges                   = cluster.rep.ranges
      val rangesWithPageBoundaries = pageLeftRange :: (ranges :+ pageRightRange)

      val maybeQueries = rangesWithPageBoundaries
        .sliding(2)
        .toSeq
        .map {
          case Seq(a, b) =>
            for {
              vslice <- pageGeometry.clipLeftRight(a.max + queryLRSlopFactor, b.min - queryLRSlopFactor)
              hslice <- vslice.clipTopHeight(0.toFloatExact(), queryRectHeight)
            } yield hslice
          case _ => None
        }

      val qband: Option[Seq[Rect]] = maybeQueries.sequence
      qband
    }

    for { query <- queries } {
      query match {
        case Some(queryRects) =>
          val queryCols: Seq[Seq[Rect]] = queryRects.map(r => {
            r.withinRegion(pageGeometry).slidingRects(Dir.Down)
          })
          val queryRows = queryCols.transpose
          val rowProps: Seq[ColumnProps] = queryRows.map {
            _ match {
              case List(gutterL, col1, gutterC, col2, gutterR) =>
                val gutterLEmpty  = hasNoOverlaps(gutterL)
                val gutterCEmpty  = hasNoOverlaps(gutterC)
                val gutterREmpty  = hasNoOverlaps(gutterR)
                val col1Empty     = hasNoOverlaps(col1)
                val col2Empty     = hasNoOverlaps(col2)
                val guttersEmpty  = gutterLEmpty && gutterREmpty && gutterCEmpty
                val colsEmpty     = col1Empty && col2Empty
                val blankStrip    = guttersEmpty && colsEmpty
                val leftColStrip  = guttersEmpty && !col1Empty && col2Empty
                val rightColStrip = guttersEmpty && col1Empty && !col2Empty
                val lrColStrip    = guttersEmpty && !col1Empty && !col2Empty

                // traceLog.trace {
                // val hitStr     = if (emptyQuery) "Empty" else "Nonempty"
                // val gutterStr  = if (isGutter) "Gutter" else "Text"
                //   createLabelOn(s"ColSlice${hitStr}${gutterStr}", queryRect)
                //     .withProp("coord", s"[${rowNum}, ${colNum}]")
                // }
                ColumnProps(guttersEmpty, col1Empty, col2Empty)

              case _ => ???
            }
          }
          rowProps.groupByWindow((window, elem) => {

          })


        case None =>
      }
    }

  }

}

trait ColumnFindingDocScope extends BaseDocumentSegmenter { self =>

  def columnClustering(): Unit = {
    val columnEvidence = docScope.docStats.columns.evidence.toSeq
    val cclusters      = docScope.docStats.columns.clusters

    val columnClusters = clusteringUtils.clusterDataX[ColumnInst](
      columnEvidence,
      _.points.map(_.asDouble()).toArray,
      4d
    )
    val columnClustersWithRanges: Seq[Cluster[ColumnInst, ColumnClass]] =
      columnClusters.map { case Cluster(_, instances) =>
        val trans = instances.map(_.vec).transpose
        val rep: List[Interval.FloatExacts] = trans.toSeq.map(ps => {
          val min = ps.min.toFloatExact()
          val max = ps.max.toFloatExact()
          Interval.FloatExacts(min, max - min)
        })
        Cluster(
          ColumnClass(rep),
          instances
        )
      }

    cclusters.appendAll(columnClustersWithRanges)

  }

}
