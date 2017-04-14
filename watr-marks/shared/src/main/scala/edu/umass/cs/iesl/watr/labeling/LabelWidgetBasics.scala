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

  def labelWidgetDiffToMods(lwDiff: LWDiff): Seq[WidgetMod] = {

    val allWidgetMod: Seq[WidgetMod] = lwDiff.universe.toList
      .flatMap { lwd => lwd.project match {
        case Same             (ident)        => Seq()
        case Similar          (ident: LabelWidgetF[Fix[Diff[Fix, LabelWidgetF, ?]]])        => Seq() //  ident
        case Different        (left: LabelWidget, right: LabelWidget)  =>
          val toRm = left.universe.toList.map {w => AddLw(w.project.wid)}
          val toAdd = right.universe.toList.map {w => RmLw(w.project.wid)}
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
          right.universe.toList.map {w => AddLw(w.project.wid)}

        case Removed          (left)         =>
          left.universe.toList.map {w => RmLw(w.project.wid)}

      }}
    allWidgetMod
  }
}
