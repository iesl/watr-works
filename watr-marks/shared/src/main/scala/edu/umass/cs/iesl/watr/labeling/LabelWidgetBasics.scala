package edu.umass.cs.iesl.watr
package labeling

import scalaz.std.anyVal._
import matryoshka._
import matryoshka.data._
import matryoshka.patterns._
import matryoshka.implicits._
import textboxing.{TextBoxing => TB}
import scalaz.syntax.foldable._
import geometry._
import geometry.syntax._

import LabelWidgetF._

// Position transform:
//  capture the absolute positioning of a widget along with the matrix transform
//    used to get it there. The inverse of the transform matrix is used to transform
//    selection boxes and clicked points back into the correct geometry to determine
//    what characters within a document are selected
case class AbsPosWidget(
  widget: LabelWidgetF[Unit],
  strictBounds: LTBounds,
  bleedBounds: LTBounds,
  translation: PositionVector,
  zOrder: Int, // not used
  scaling: Double = 1.0d
)

case class WidgetLayout(
  positioning: Seq[AbsPosWidget],
  strictBounds: LTBounds,
  bleedBounds: LTBounds,
  labelWidget: LabelWidget
)

// Accumulator for calculating layout positioning transforms
case class PosAttr(
  widget: LabelWidgetF[Unit],
  strictBounds: LTBounds,
  bleedBounds: LTBounds,
  zOrder: Int, // not used
  selfTranslation: PositionVector,
  childTranslations: List[PositionVector] = List(),
  scaling: Double = 1.0d
) {
    def toAbsPosWidget: AbsPosWidget = {
      AbsPosWidget(widget, strictBounds, bleedBounds, selfTranslation, zOrder, scaling)
    }

  def toStackString = {
    val soff = selfTranslation.prettyPrint
    val ch = childTranslations.map(_.prettyPrint).mkString(", ")
    s"curVec:${soff}   st:[ ${ch} ]"
  }

  lazy val id = widget.wid
  override def toString = {
    val wpp = strictBounds.prettyPrint
    val sstr = toStackString
    val cn = widget.getClass().getName.split("\\$").last
    s"${cn}#${id} bb:${wpp} ${sstr} ]"
  }

  def translate(pvec: PositionVector): PosAttr = {
    PosAttr(
      widget,
      strictBounds.translate(pvec),
      bleedBounds.translate(pvec),
      zOrder,
      selfTranslation.translate(pvec),
      childTranslations.map(_.translate(pvec))
    )
  }
}

trait LabelWidgetBasics {

  import utils.ScalazTreeImplicits._

  def prettyPrintLabelWidget(lwidget: LabelWidget): TB.Box = {
    lwidget.cata(toTree).drawBox
  }

  type LWDiff = Fix[Diff[Fix, LabelWidgetF, ?]]

  def labelWidgetDiff(w1: LabelWidget, w2: LabelWidget): LWDiff = {
    val lwDiff: Fix[Diff[Fix, LabelWidgetF, ?]] = w1.paraMerga(w2)(diff)
    lwDiff
  }

  def drawLabelWidgetDiff(lwDiff: LWDiff): String = {
    lwDiff.cata(toTree).drawTree
  }

  def universeLw(labelWidget: LabelWidget): List[LabelWidget] = {
    labelWidget.universe.toList
  }

  def universeLwd(lwDiff: LWDiff): List[LWDiff] = {
    lwDiff.universe.toList
  }

  def labelWidgetDiffToMods(lwDiff: LWDiff): Seq[WidgetMod] = {

    val allWidgetMod: Seq[WidgetMod] = universeLwd(lwDiff)
      .flatMap { lwd => lwd.project match {
        case Same(ident) =>
          universeLw(ident).map {w => WidgetMod.Unmodified(w.project.wid)}

        case Similar(ident: LabelWidgetF[Fix[Diff[Fix, LabelWidgetF, ?]]]) =>
          Seq(WidgetMod.Unmodified(ident.wid))

        case Different(left: LabelWidget, right: LabelWidget)  =>

          val toRm = universeLw(left).map {w => WidgetMod.Removed(w.project.wid)}
          val toAdd = universeLw(right).map {w => WidgetMod.Added(w.project.wid)}
          toRm ++ toAdd

        case LocallyDifferent (left, right)  =>
          Seq(WidgetMod.Removed(left.wid), WidgetMod.Added(right.wid))

        case Inserted (right) =>
          Seq(WidgetMod.Added(right.wid))

        case Deleted (left)         =>
          Seq(WidgetMod.Removed(left.wid))

        case Added(right) =>
          universeLw(right).map {w => WidgetMod.Added(w.project.wid)}

        case Removed(left) =>
          universeLw(left).map {w => WidgetMod.Removed(w.project.wid)}

      }}
    allWidgetMod
  }
}
