package org.watrworks
package segment

import geometry._
import geometry.syntax._

import utils.{Direction => Dir}
import utils.{M3x3Position => M3}
import utils.ExactFloats._
import utils.Interval

sealed trait ColumnEvidence

object ColumnEvidence {
  case class TwoColumn(
    pageBand: Rect,
    pageNum: Int @@ PageNum,
    c1FontId: String @@ ScaledFontID,
    c1Left: Int @@ FloatRep,
    c1Right: Int @@ FloatRep,
    c2FontId: String @@ ScaledFontID,
    c2Left: Int @@ FloatRep,
    c2Right: Int @@ FloatRep
  ) extends ColumnEvidence

  case class TwoColumnClass(
    range1: Interval.FloatExacts,
    range2: Interval.FloatExacts,
    range3: Interval.FloatExacts,
    range4: Interval.FloatExacts
  ) extends ColumnEvidence

}

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

              val colEvidence = ColumnEvidence.TwoColumn(
                pageBand,
                pageNum,
                c1FontId = fontId,
                c1Left = leftGutterQueryRect.right,
                c1Right = sepRect.left,
                c2FontId = fontId,
                c2Left = sepRect.right,
                c2Right = rightGutterQueryRect.left
              )

              docScope.docStats.columnEvidence.twoColumnEvidence.append(colEvidence)

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

  }

}

trait ColumnFindingDocScope extends BaseDocumentSegmenter { self =>

  def columnClustering(): Unit = {
    val twoColumnEvidence = docScope.docStats.columnEvidence.twoColumnEvidence.toSeq

    clusterColumn(twoColumnEvidence, _.c1Left)
    clusterColumn(twoColumnEvidence, _.c1Right)
    clusterColumn(twoColumnEvidence, _.c2Left)
    clusterColumn(twoColumnEvidence, _.c2Right)

  }

  private def clusterColumn(
    evidence: Seq[ColumnEvidence.TwoColumn],
    f: (ColumnEvidence.TwoColumn) => Int @@ FloatRep
  ) = {
    val ColumnRangeWidth = 1.toFloatExact() // FloatRep(20)
    val colPoints        = evidence.map(f(_)).toSeq

    val colClusters = clusteringUtils.clusterPoints(colPoints, ColumnRangeWidth)
    val shown = colClusters
      .map({ clusterRange =>
        Interval.FloatExacts.FloatExactsShow.show(clusterRange).toString()
      })
      .mkString("; ")

    println(s"${shown}")

    colClusters
  }

}
