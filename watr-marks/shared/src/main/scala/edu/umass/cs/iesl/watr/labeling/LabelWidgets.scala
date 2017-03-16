package edu.umass.cs.iesl.watr
package labeling

import scalaz.{Functor, Traverse, Applicative, Show, Cofree}
import scalaz.std.list._
import scalaz.syntax.traverse._

import matryoshka._
import matryoshka.data._

import geometry._
import textreflow.data._
import watrmarks.Label
import textboxing.{TextBoxing => TB}
import utils.Color

/**
  LabelWidgets provide a way to combine rectangular regions  into
  a single layout.

  */


sealed trait LabelWidgetF[+A]


case class LabelOptions(
  labels: List[Label]
)

case class LabelingPanel(
  content: LabelWidgetF.LabelWidget,
  options: LabelOptions
)

object LabelWidgetF {

  type LabelWidget = Fix[LabelWidgetF]

  // unFixed type for LabelWidget
  type LabelWidgetT = LabelWidgetF[Fix[LabelWidgetF]]

  // LabelWidget w/ Cofree attribute
  type LabelWidgetAttr[A] = Cofree[LabelWidgetF, A]

  // LabelWidget w/position attribute
  type LabelWidgetPosAttr = LabelWidgetF[LabelWidgetAttr[PosAttr]]

  case class RegionOverlay[A](
    under: PageRegion,
    overs: List[A]
  ) extends LabelWidgetF[A]

  case class LabeledTarget(
    target: PageRegion,
    label: Option[Label],
    score: Option[Double]
  ) extends LabelWidgetF[Nothing]

  case class TextBox(
    textBox: TB.Box
  ) extends LabelWidgetF[Nothing]

  case class Reflow(
    textReflow: TextReflow
  ) extends LabelWidgetF[Nothing]

  case class Row[A](as: List[A]) extends LabelWidgetF[A]
  case class Col[A](as: List[A]) extends LabelWidgetF[A]
  case class Pad[A](a: A, pad: Padding, color: Option[Color]) extends LabelWidgetF[A]

  case class Figure[A](
    figure: GeometricGroup
  ) extends LabelWidgetF[A]

  // case class Panel[A](
  //   a: A
  //     // function: Action => (WidgetTransformFunction, UserFunction: () => {})
  // ) extends LabelWidgetF[A]

  type PositionVector = Point

  implicit def LabelWidgetTraverse: Traverse[LabelWidgetF] = new Traverse[LabelWidgetF] {
    def traverseImpl[G[_], A, B](
      fa: LabelWidgetF[A])(
      f: A => G[B])(
      implicit G: Applicative[G]
    ): G[LabelWidgetF[B]] = {
      fa match {
        case l : RegionOverlay[A]          => l.overs.traverse(f).map(ft => l.copy(overs=ft))
        case l : LabeledTarget             => G.point(l.copy())
        case l @ TextBox(tb)               => G.point(l.copy())
        case l @ Reflow(tr)                => G.point(l.copy())
        case l @ Row(as)                   => as.traverse(f).map(Row(_))
        case l @ Col(as)                   => as.traverse(f).map(Col(_))
        case l @ Pad(a, padding, color)    => f(a).map(Pad(_, padding, color))
        case l @ Figure(f)                 => G.point(l.copy())
      }
    }
  }

  implicit def LabelWidgetFunctor: Functor[LabelWidgetF] = LabelWidgetTraverse

  implicit def LabelWidgetShow: Delay[Show, LabelWidgetF] = new Delay[Show, LabelWidgetF] {
    def apply[A](show: Show[A]) = Show.show {
      case l : RegionOverlay[A]       => s"$l"
      case l : LabeledTarget          => s"label-target"
      case l @ Reflow(tr)             => s"reflow()"
      case l @ TextBox(tb)            => s"textbox"
      case l @ Row(as)                => s"$l"
      case l @ Col(as)                => s"$l"
      case l @ Pad(a, padding, color) => s"$l"
      case l @ Figure(f)              => l.toString
    }
  }
}

object LabelWidgets {

  import matryoshka.data._

  import LabelWidgetF._

  def fixlw = Fix[LabelWidgetF](_)

  def targetOverlay(tr: PageRegion, overs: Seq[LabelWidget]) =
    fixlw(RegionOverlay(tr, overs.toList))

  def labeledTarget(target: PageRegion, label: Option[Label]=None, score: Option[Double]=None) =
    fixlw(LabeledTarget(target, label, score))

  def reflow(tr: TextReflow) =
    fixlw(Reflow(tr))

  def textbox(tb: TB.Box) =
    fixlw(TextBox(tb))

  def col(lwidgets: LabelWidget*): LabelWidget =
    fixlw(Col(lwidgets.toList))

  def row(lwidgets: LabelWidget*): LabelWidget =
    fixlw(Row(lwidgets.toList))

  def pad(content: LabelWidget, pad: Padding): LabelWidget =
    fixlw(Pad(content, pad, None))

  def pad(content: LabelWidget, pad: Padding, color: Color): LabelWidget =
    fixlw(Pad(content, pad, Option(color)))

}
