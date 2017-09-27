package edu.umass.cs.iesl.watr
package segment


import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import segment.{SegmentationLabels => LB}

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
          val ml1Text = pageIndex.components.getComponentText(l1, LB.VisualLine)
          val ml2Text = pageIndex.components.getComponentText(l2, LB.VisualLine)


          (ml1Text, ml2Text) match {
            case (Some(l1Text), Some(l2Text)) =>

              // pageIndex.components.getRelation(l1, LB.VisualLineModal)
              val l1VisLineModal = pageIndex.components.getRelation(l1, LB.VisualLineModal).head
              val l2VisLineModal = pageIndex.components.getRelation(l2, LB.VisualLineModal).head

              val l1Baseline = l1VisLineModal.bounds().toLine(Dir.Bottom)
              val l2Baseline = l2VisLineModal.bounds().toLine(Dir.Bottom)

              val t = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)

              pageStats.trapezoidHeights.fill(t)
              pageStats.leftAcuteBaseAngles.fill(t)
              pageStats.leftObtuseBaseAngles.fill(t)
              docStats.trapezoidHeights.fill(t)
              docStats.leftAcuteBaseAngles.fill(t)
              docStats.leftObtuseBaseAngles.fill(t)

              pageIndex.components.setAttribute[Trapezoid](l1.id, watrmarks.Label("Trapezoid"), t)

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

    // pageIndex.components.reportClusters()

    for {
      (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex).toList
      linePair <- lineCCs.sliding(2)
    }  {

      // construct trapezoids: isosceles, right, rectangular
      linePair match {
        case Seq(l1, l2) =>
          val ml1Text = pageIndex.components.getComponentText(l1, LB.VisualLine)
          val ml2Text = pageIndex.components.getComponentText(l2, LB.VisualLine)


          (ml1Text, ml2Text) match {
            case (Some(l1Text), Some(l2Text)) =>

              val l1VisLineModal = pageIndex.components.getRelation(l1, LB.VisualLineModal).head
              val l2VisLineModal = pageIndex.components.getRelation(l2, LB.VisualLineModal).head

              val l1Baseline = l1VisLineModal.bounds().toLine(Dir.Bottom)
              val l2Baseline = l2VisLineModal.bounds().toLine(Dir.Bottom)

              val t = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)

              pageStats.trapezoidHeights.fill(t)
              pageStats.leftAcuteBaseAngles.fill(t)
              pageStats.leftObtuseBaseAngles.fill(t)
              docStats.trapezoidHeights.fill(t)
              docStats.leftAcuteBaseAngles.fill(t)
              docStats.leftObtuseBaseAngles.fill(t)

              pageIndex.components.setAttribute[Trapezoid](l1.id, watrmarks.Label("Trapezoid"), t)

              Option(t)

            case _ => None
          }
        case Seq(l1) => None
        case Seq() => None
      }
    }

  }
}
