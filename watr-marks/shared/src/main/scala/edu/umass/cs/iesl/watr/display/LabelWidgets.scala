package edu.umass.cs.iesl.watr
package display

import scalaz.{Traverse, Applicative}
import scalaz.std.list._
import scalaz.syntax.traverse._

import matryoshka._

import geometry._
import textreflow._
import watrmarks._
import utils.EnrichNumerics._

sealed trait LabelWidgetF[+A]

object LabelWidgetF {

  case class Target(
    targetRegion: TargetRegion,
    emboss: List[Label],
    selections: List[LTBounds] = List()
  ) extends LabelWidgetF[Nothing]

  case class Reflow(
    textReflow: TextReflow,
    selections: List[RangeInt] = List()
  ) extends LabelWidgetF[Nothing]

  case class Button() extends LabelWidgetF[Nothing]

  case class MouseOverlay[A](
    backplane: A
  ) extends LabelWidgetF[A]


  case class Panel[A](content: A)     extends LabelWidgetF[A]
  case class Row[A](as: List[A])      extends LabelWidgetF[A]
  case class Col[A](as: List[A])      extends LabelWidgetF[A]


  implicit def LabelWidgetTraverse: Traverse[LabelWidgetF] = new Traverse[LabelWidgetF] {
    def traverseImpl[G[_], A, B](
      fa: LabelWidgetF[A])(
      f: A => G[B])(
      implicit G: Applicative[G]
    ): G[LabelWidgetF[B]] = {
      fa match {
        case l @ Target(tr, emboss, sels) => G.point(l.copy())
        case l @ Reflow(tr, sels)         => G.point(l.copy())
        case l @ Button()                 => G.point(l.copy())
        case l @ MouseOverlay(bkplane)    => f(bkplane).map(a => l.copy(backplane=a))
        case l @ Panel(content)           => f(content).map(a => l.copy(content=a))
        case l @ Row(as)                  => as.traverse(f).map(Row(_))
        case l @ Col(as)                  => as.traverse(f).map(Col(_))
      }
    }
  }

  // implicit def LabelWidgetShow: Delay[Show, LabelWidgetF] = new Delay[Show, LabelWidgetF] {
  //   def apply[A](show: Show[A]) = Show.show {
  //   }
  // }
}

object LabelWidgets {

  import matryoshka.data._

  import LabelWidgetF._

  def fixlw = Fix[LabelWidgetF](_)

  def target(tr: TargetRegion, emboss: List[Label], sels: List[LTBounds]) =
    fixlw(Target(tr, emboss, sels))

  def reflow(tr: TextReflow) =
    fixlw(Reflow(tr))

  def mouseOverlay(bkplane: LabelWidget) =
    fixlw(MouseOverlay(bkplane))

  def col(lwidgets: LabelWidget*): LabelWidget = {
    fixlw(Col(
      lwidgets.toList
    ))
  }

  def panel(content: LabelWidget): LabelWidget = {
    fixlw(Panel(content))
  }
}
