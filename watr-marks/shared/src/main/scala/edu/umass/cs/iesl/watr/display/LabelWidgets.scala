package edu.umass.cs.iesl.watr
package display

import scalaz._
import Scalaz._

import matryoshka._

import geometry._
import textreflow._
import watrmarks._

sealed trait LabelWidgetF[+A]

object LabelWidgetF {

  case class Target(
    targetRegion: TargetRegion,
    emboss: List[Label]
  ) extends LabelWidgetF[Nothing]

  case class Reflow(textReflow: TextReflow)     extends LabelWidgetF[Nothing]
  case class Button()                           extends LabelWidgetF[Nothing]

  case class MouseOverlay[A](
    backplane: A,
    selections: List[TargetRegion]
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
        case l @ Target(tr, emboss)             => G.point(l.copy())
        case l @ Reflow(tr)                     => G.point(l.copy())
        case l @ Button()                       => G.point(l.copy())
        case l @ MouseOverlay(bkplane, sels)    => f(bkplane).map(a => l.copy(backplane=a))
        case l @ Panel(content)                 => f(content).map(a => l.copy(content=a))
        case l @ Row(holes)                     => holes.traverse(f).map(Row(_))
        case l @ Col(holes)                     => holes.traverse(f).map(Col(_))
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


  def target(tr: TargetRegion, emboss: Label*) =
    fixlw(Target(tr, emboss.toList))

  def mouseOverlay(bkplane: LabelWidget, preselects: List[TargetRegion]=List()) =
    fixlw(MouseOverlay(
      bkplane, preselects
    ))

  def reflow(tr: TextReflow) = fixlw(Reflow(
    tr
  ))
}
