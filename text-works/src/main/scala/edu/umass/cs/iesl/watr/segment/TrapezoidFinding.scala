package edu.umass.cs.iesl.watr
package segment


import geometry._
import geometry.syntax._
import textgrid._
import watrmarks._

trait ShapeFunctions extends TrapezoidFinding with LineShapeClassification { self =>
  lazy val shapeFunctions = self

}

trait TrapezoidFinding extends PageScopeSegmenter { self =>

  private def getComponentText(cc: Int@@ShapeID, l: Label): Option[TextGrid.Row] = {
    // val l1TextRowOpt = shapeIndex.getComponentText(line1CC, LB.VisualLine)
    ???
  }

  // def buildInitialLinePairTrapezoids(): Unit = {

  //   val docStats = docScope.docStats


  //   for {
  //     linePair <- PageSegmenter.getVisualLinesInExtractionOrder(shapeIndex).sliding(2)
  //   }  {

  //     // construct trapezoids: isosceles, right, rectangular
  //     linePair match {
  //       case Seq(l1, l2) =>
  //         val ml1Text = getComponentText(l1.id, LB.VisualLine)
  //         val ml2Text = getComponentText(l2.id, LB.VisualLine)


  //         (ml1Text, ml2Text) match {
  //           case (Some(l1Text), Some(l2Text)) =>

  //             // shapeIndex.getRelation(l1, LB.VisualLineModal)
  //             val l1VisLineModal = shapeIndex.getRelation(l1, LB.VisualLineModal).head
  //             val l2VisLineModal = shapeIndex.getRelation(l2, LB.VisualLineModal).head

  //             val l1Baseline = l1VisLineModal.shape.bounds().toLine(Dir.Bottom)
  //             val l2Baseline = l2VisLineModal.shape.bounds().toLine(Dir.Bottom)

  //             val t = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)

  //             pageStats.trapezoidHeights.fill(t)
  //             pageStats.leftAcuteBaseAngles.fill(t)
  //             pageStats.leftObtuseBaseAngles.fill(t)
  //             docStats.trapezoidHeights.fill(t)
  //             docStats.leftAcuteBaseAngles.fill(t)
  //             docStats.leftObtuseBaseAngles.fill(t)

  //             shapeIndex.setShapeAttribute[Trapezoid](l1.id, watrmarks.Label("Trapezoid"), t)

  //             Option(t)

  //           case _ => None
  //         }
  //       case Seq(l1) => None
  //       case Seq() => None
  //     }
  //   }

  // }

  // def buildLinePairTrapezoids(): Unit = {

  //   val docStats = docScope.docStats

  //   // shapeIndex.reportClusters()

  //   for {
  //     (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(shapeIndex).toList
  //     linePair <- lineCCs.sliding(2)
  //   }  {

  //     // construct trapezoids: isosceles, right, rectangular
  //     linePair match {
  //       case Seq(l1, l2) =>
  //         val ml1Text = getComponentText(l1.id, LB.VisualLine)
  //         val ml2Text = getComponentText(l2.id, LB.VisualLine)


  //         (ml1Text, ml2Text) match {
  //           case (Some(l1Text), Some(l2Text)) =>

  //             val l1VisLineModal = shapeIndex.getRelation(l1, LB.VisualLineModal).head
  //             val l2VisLineModal = shapeIndex.getRelation(l2, LB.VisualLineModal).head

  //             val l1Baseline = l1VisLineModal.bounds().toLine(Dir.Bottom)
  //             val l2Baseline = l2VisLineModal.bounds().toLine(Dir.Bottom)

  //             val t = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)

  //             pageStats.trapezoidHeights.fill(t)
  //             pageStats.leftAcuteBaseAngles.fill(t)
  //             pageStats.leftObtuseBaseAngles.fill(t)
  //             docStats.trapezoidHeights.fill(t)
  //             docStats.leftAcuteBaseAngles.fill(t)
  //             docStats.leftObtuseBaseAngles.fill(t)

  //             shapeIndex.setAttribute[Trapezoid](l1.id, watrmarks.Label("Trapezoid"), t)

  //             Option(t)

  //           case _ => None
  //         }
  //       case Seq(l1) => None
  //       case Seq() => None
  //     }
  //   }

  // }
}
