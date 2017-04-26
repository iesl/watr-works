package edu.umass.cs.iesl.watr
package labeling

import scalaz.std.anyVal._
// import scalaz._, Scalaz._
import matryoshka._
import matryoshka.implicits._
import textboxing.{TextBoxing => TB}
import scalaz.syntax.foldable._

import LabelWidgetF._

trait LabelWidgetBasics {

  import utils.ScalazTreeImplicits._


  def prettyPrintLabelWidget(lwidget: LabelWidget): TB.Box = {
    lwidget.cata(toTree).drawBox
  }

  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._
  import matryoshka.patterns._

  type LWDiff = Fix[Diff[Fix, LabelWidgetF, ?]]

  def labelWidgetDiff(w1: LabelWidget, w2: LabelWidget): LWDiff = {
    val lwDiff: Fix[Diff[Fix, LabelWidgetF, ?]] = w1.paraMerga(w2)(diff)
    lwDiff
  }

  def drawLabelWidgetDiff(lwDiff: LWDiff): String = {
    lwDiff.cata(toTree).drawTree
  }

  def universeLw(labelWidget: LabelWidget): List[LabelWidget] = {
    // labelWidget.elgotPara(universe).toList
    labelWidget.universe.toList
  }

  def universeLwd(lwDiff: LWDiff): List[LWDiff] = {
    // lwDiff.elgotPara(universe).toList
    lwDiff.universe.toList
  }

  def labelWidgetDiffToMods(lwDiff: LWDiff): Seq[WidgetMod] = {

    val allWidgetMod: Seq[WidgetMod] = universeLwd(lwDiff)
      .flatMap { lwd => lwd.project match {
        case Same             (ident)        => Seq()
        case Similar          (ident: LabelWidgetF[Fix[Diff[Fix, LabelWidgetF, ?]]])        => Seq() //  ident
        case Different        (left: LabelWidget, right: LabelWidget)  =>
          val toRm = universeLw(left).map {w => RmLw(w.project.wid)}
          val toAdd = universeLw(right).map {w => AddLw(w.project.wid)}
          toRm ++ toAdd


        case LocallyDifferent (left, right)  =>
          Seq(RmLw(left.wid), AddLw(right.wid))

        case Inserted         (right)        =>
          // Seq(AddLw(right.wid))
          ???

        case Deleted          (left)         =>
          // Seq(RmLw(left.wid))
          ???

        case Added            (right)        =>
          universeLw(right).map {w => AddLw(w.project.wid)}

        case Removed          (left)         =>
          universeLw(left).map {w => RmLw(w.project.wid)}

      }}
    allWidgetMod
  }
}
