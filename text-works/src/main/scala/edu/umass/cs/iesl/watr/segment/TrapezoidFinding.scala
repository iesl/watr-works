package edu.umass.cs.iesl.watr
package segment


import watrmarks.{StandardLabels => LB}

import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}

trait ShapeFunctions extends TrapezoidFinding with LineShapeClassification { self =>
  lazy val shapeFunctions = self

}

trait TrapezoidFinding extends PageScopeSegmenter { self =>

  def buildInitialLinePairTrapezoids(): Unit = {

    val docStats = docScope.docStats


    for {
      linePair <- PageSegmenter.getVisualLinesInExtractionOrder(pageIndex).sliding(2)
    }  {

      // construct trapezoids: isosceles, right, rectangular
      linePair match {
        case Seq(l1, l2) =>
          val ml1Text = pageIndex.getComponentText(l1, LB.VisualLine)
          val ml2Text = pageIndex.getComponentText(l2, LB.VisualLine)


          (ml1Text, ml2Text) match {
            case (Some(l1Text), Some(l2Text)) =>

              // pageIndex.getRelation(l1, LB.VisualLineModal)
              val l1VisLineModal = pageIndex.getRelation(l1, LB.VisualLineModal).head
              val l2VisLineModal = pageIndex.getRelation(l2, LB.VisualLineModal).head

              val l1Baseline = l1VisLineModal.bounds().toLine(Dir.Bottom)
              val l2Baseline = l2VisLineModal.bounds().toLine(Dir.Bottom)

              val t = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)

              pageStats.trapezoidHeights.fill(t)
              pageStats.leftAcuteBaseAngles.fill(t)
              pageStats.leftObtuseBaseAngles.fill(t)
              docStats.trapezoidHeights.fill(t)
              docStats.leftAcuteBaseAngles.fill(t)
              docStats.leftObtuseBaseAngles.fill(t)

              pageIndex.setAttribute[Trapezoid](l1.id, watrmarks.Label("Trapezoid"), t)

              Option(t)

            case _ => None
          }
        case Seq(l1) => None
        case Seq() => None
      }
    }

  }

  def buildLinePairTrapezoids(): Unit = {

    val docStats = docScope.docStats

    // pageIndex.reportClusters()

    for {
      (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex).toList
      linePair <- lineCCs.sliding(2)
    }  {

      // construct trapezoids: isosceles, right, rectangular
      linePair match {
        case Seq(l1, l2) =>
          val ml1Text = pageIndex.getComponentText(l1, LB.VisualLine)
          val ml2Text = pageIndex.getComponentText(l2, LB.VisualLine)


          (ml1Text, ml2Text) match {
            case (Some(l1Text), Some(l2Text)) =>

              val l1VisLineModal = pageIndex.getRelation(l1, LB.VisualLineModal).head
              val l2VisLineModal = pageIndex.getRelation(l2, LB.VisualLineModal).head

              val l1Baseline = l1VisLineModal.bounds().toLine(Dir.Bottom)
              val l2Baseline = l2VisLineModal.bounds().toLine(Dir.Bottom)

              val t = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)

              pageStats.trapezoidHeights.fill(t)
              pageStats.leftAcuteBaseAngles.fill(t)
              pageStats.leftObtuseBaseAngles.fill(t)
              docStats.trapezoidHeights.fill(t)
              docStats.leftAcuteBaseAngles.fill(t)
              docStats.leftObtuseBaseAngles.fill(t)

              pageIndex.setAttribute[Trapezoid](l1.id, watrmarks.Label("Trapezoid"), t)

              Option(t)

            case _ => None
          }
        case Seq(l1) => None
        case Seq() => None
      }
    }

  }
}
