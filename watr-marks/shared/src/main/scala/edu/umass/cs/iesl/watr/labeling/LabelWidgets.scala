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

sealed trait LabelWidgetF[+A]

object LabelWidgetF {

  type LabelWidget = Fix[LabelWidgetF]

  // unFixed type for LabelWidget
  type LabelWidgetT = LabelWidgetF[Fix[LabelWidgetF]]

  // LabelWidget w/ Cofree attribute
  type LabelWidgetAttr[A] = Cofree[LabelWidgetF, A]

  // LabelWidget w/position attribute
  type LabelWidgetPosAttr = LabelWidgetF[LabelWidgetAttr[PosAttr]]


  case class TargetOverlay[A](
    under: TargetRegion,
    overs: List[A]
  ) extends LabelWidgetF[A]

  case class LabeledTarget(
    target: TargetRegion,
    label: Option[Label],
    score: Option[Double]
  ) extends LabelWidgetF[Nothing]


  case class TextBox(
    textBox: TB.Box
  ) extends LabelWidgetF[Nothing]

  case class Reflow(
    textReflow: TextReflow
  ) extends LabelWidgetF[Nothing]

  // case class RangeSelection(
  //   range: (Int, Int)
  // ) extends LabelWidgetF[Nothing]


  // Overlay that accepts mouse gesture input
  case class MouseOverlay[A](
    backplane: A
  ) extends LabelWidgetF[A]

  case class Panel[A](
    content: A
  ) extends LabelWidgetF[A]

  case class Button(
    action: String
  ) extends LabelWidgetF[Nothing]

  case class Row[A](as: List[A]) extends LabelWidgetF[A]
  case class Col[A](as: List[A]) extends LabelWidgetF[A]
  case class Pad[A](a: A, pad: Padding) extends LabelWidgetF[A]

  type PositionVector = Point

  implicit def LabelWidgetTraverse: Traverse[LabelWidgetF] = new Traverse[LabelWidgetF] {
    def traverseImpl[G[_], A, B](
      fa: LabelWidgetF[A])(
      f: A => G[B])(
      implicit G: Applicative[G]
    ): G[LabelWidgetF[B]] = {
      fa match {
        case l @ TargetOverlay(under, overs)           => overs.traverse(f).map(TargetOverlay(under, _))
        case l @ LabeledTarget(target, label, score)   => G.point(l.copy())
        case l @ TextBox(tb)                           => G.point(l.copy())
        case l @ Reflow(tr)                            => G.point(l.copy())
        case l @ Button(action)                        => G.point(l.copy())
        case l @ MouseOverlay(bkplane)                 => f(bkplane).map(a => l.copy(backplane=a))
        case l @ Panel(content)                        => f(content).map(Panel(_))
        case l @ Row(as)                               => as.traverse(f).map(Row(_))
        case l @ Col(as)                               => as.traverse(f).map(Col(_))
        case l @ Pad(a, padding)                       => f(a).map(Pad(_, padding))
      }
    }
  }

  implicit def LabelWidgetFunctor: Functor[LabelWidgetF] = LabelWidgetTraverse

  implicit def LabelWidgetShow: Delay[Show, LabelWidgetF] = new Delay[Show, LabelWidgetF] {
    def apply[A](show: Show[A]) = Show.show {
      case l @ TargetOverlay(under, overs)           => s"$l"
      case l @ LabeledTarget(target, label, score)   => s"label-target"
      case l @ Reflow(tr)                  => s"reflow()"
      case l @ TextBox(tb)                 => s"textbox"
      case l @ Button(action)              => s"$l"
      case l @ MouseOverlay(bkplane)       => s"$l"
      case l @ Panel(content)              => s"$l"
      case l @ Row(as)                     => s"$l"
      case l @ Col(as)                     => s"$l"
      case l @ Pad(a, padding)             => s"$l"
    }
  }
}

object LabelWidgets {

  import matryoshka.data._

  import LabelWidgetF._

  def fixlw = Fix[LabelWidgetF](_)

  def targetOverlay(tr: TargetRegion, overs: Seq[LabelWidget]) =
    fixlw(TargetOverlay(tr, overs.toList))

  def labeledTarget(target: TargetRegion, label: Option[Label]=None, score: Option[Double]=None) =
    fixlw(LabeledTarget(target, label, score))

  def reflow(tr: TextReflow) =
    fixlw(Reflow(tr))

  def button(a: String) =
    fixlw(Button(a))

  def textbox(tb: TB.Box) =
    fixlw(TextBox(tb))

  def mouseOverlay(bkplane: LabelWidget) =
    fixlw(MouseOverlay(bkplane))

  def col(lwidgets: LabelWidget*): LabelWidget =
    fixlw(Col(lwidgets.toList))

  def row(lwidgets: LabelWidget*): LabelWidget =
    fixlw(Row(lwidgets.toList))

  def panel(content: LabelWidget): LabelWidget =
    fixlw(Panel(content))

  def pad(content: LabelWidget, pad: Padding): LabelWidget =
    fixlw(Pad(content, pad))

}
