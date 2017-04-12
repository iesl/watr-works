package edu.umass.cs.iesl.watr
package labeling

import scalaz.{@@ => _, _}, Scalaz._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns._

import geometry._
import textreflow.data._
import watrmarks.Label
import textboxing.{TextBoxing => TB}
import utils.Color
import scala.reflect._

/**
  LabelWidgets provide a way to combine rectangular regions into a single layout.
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

  // Used for recording positioning offsets during layout
  type PositionVector = Point

  // Display all (or part) of a page, as the background in an overlay
  // case class PageDisplay(
  //   // page: RecordedPageID,
  //   pageId: Int@@PageID,
  //   bbox: LTBounds
  // ) extends LabelWidgetF[Nothing]

  case class RegionOverlay[A](
    pageId: Int@@PageID,
    geometry: LTBounds,
    clipTo: Option[LTBounds],
    overs: List[A]
  ) extends LabelWidgetF[A]

  case class TextBox(
    textBox: TB.Box
  ) extends LabelWidgetF[Nothing]

  case class Reflow(
    textReflow: TextReflow
  ) extends LabelWidgetF[Nothing]

  case class Row[A](as: List[A]) extends LabelWidgetF[A]
  case class Col[A](as: List[A]) extends LabelWidgetF[A]

  case class Pad[A](
    a: A,
    pad: Padding,
    color: Option[Color]
  ) extends LabelWidgetF[A]

  case class Figure(
    figure: GeometricFigure
  ) extends LabelWidgetF[Nothing]

  case class Panel[A](
    a: A,
    interaction: Interaction
  ) extends LabelWidgetF[A]

  case class Identified[A](
    a: A,
    id: Int,
    idclass: String
  ) extends LabelWidgetF[A]


  implicit def LabelWidgetTraverse: Traverse[LabelWidgetF] = new Traverse[LabelWidgetF] {
    def traverseImpl[G[_], A, B](
      fa: LabelWidgetF[A])(
      f: A => G[B])(
      implicit G: Applicative[G]
    ): G[LabelWidgetF[B]] = {
      fa match {
        // case l : PageDisplay             => G.point(l.copy())
        // case l : PageRegionDisplay       => G.point(l.copy())
        // case l : TargetRegionDisplay     => G.point(l.copy())
        case l : RegionOverlay[A]        => l.overs.traverse(f).map(o => l.copy(overs=o))

        case l @ Row(as)                 => as.traverse(f).map(Row(_))
        case l @ Col(as)                 => as.traverse(f).map(Col(_))
        case l @ Pad(a, pd, clr)         => f(a).map(Pad(_, pd, clr))
        case l : TextBox                 => G.point(l.copy())
        case l : Reflow                  => G.point(l.copy())
        case l : Figure                  => G.point(l.copy())
        case l @ Panel(a, i)             => f(a).map(Panel(_, i))
        case l @ Identified(a, id, cls)  => f(a).map(Identified(_, id, cls))
      }
    }
  }

  implicit def LabelWidgetFunctor: Functor[LabelWidgetF] = LabelWidgetTraverse

  implicit def LabelWidgetShow: Delay[Show, LabelWidgetF] = new Delay[Show, LabelWidgetF] {
    def apply[A](show: Show[A]) = Show.show {
      // case l : PageDisplay               => l.toString()
      // case l : PageRegionDisplay         => l.toString()
      // case l : TargetRegionDisplay       => l.toString()
      case l : RegionOverlay[A]          => s"$l"
      // case l : LabeledTarget             => s"label-target"
      case l @ Reflow(tr)                => s"reflow()"
      case l @ TextBox(tb)               => s"textbox"
      case l @ Row(as)                   => s"$l"
      case l @ Col(as)                   => s"$l"
      case l @ Pad(a, padding, color)    => s"$l"
      case l @ Figure(f)                 => l.toString
      case l @ Panel(a, i)               => l.toString
      case l @ Identified(a, id, cls)    => l.toString
    }
  }

  implicit def equal: Delay[Equal, LabelWidgetF] = new Delay[Equal, LabelWidgetF] {
    def apply[A](eq: Equal[A]) = Equal.equal((a, b) => {
      implicit val ieq = eq;

      (a, b) match {
        // case (l : PageDisplay                  , l2 : PageDisplay         ) => false
        // case (l : PageRegionDisplay            , l2 : PageRegionDisplay   ) => false
        // case (l : TargetRegionDisplay          , l2 : TargetRegionDisplay ) => false
        case (l @ RegionOverlay(p1, g1, c1, o1)             , l2 @ RegionOverlay(p2, g2, c2, o2) ) =>
          p1==p2 && g1==g2 && c1==c2 && o1 === o2
        // case (l @ LabeledTarget(tr, lbl, scr)  , l2 : LabeledTarget       ) => tr.id==l2.target.id
        case (l @ Reflow(tr)                   , l2 : Reflow              ) => false
        case (l @ TextBox(tb)                  , l2 : TextBox             ) => false
        case (l @ Row(as)                      , l2 : Row[A]              ) => as === l2.as
        case (l @ Col(as)                      , l2 : Col[A]              ) => as === l2.as
        case (l @ Pad(a, padding, color)       , l2 : Pad[A]              ) => a === l2.a
        case (l @ Figure(f)                    , l2 : Figure              ) => false
        case (l @ Panel(a, i)                  , l2 : Panel[A]            ) => a === l2.a
        case (l @ Identified(a, id, cls)       , l2 : Identified[A]       ) => true

        case (_ , _) => false

      }
    })
  }

  implicit val diffable: Diffable[LabelWidgetF] = new Diffable[LabelWidgetF] {

    def diffImpl[T[_[_]]: BirecursiveT](l: T[LabelWidgetF], r: T[LabelWidgetF]):
        Option[DiffT[T, LabelWidgetF]] = {

      (l.project, r.project) match {
        case (l @ RegionOverlay(p1, g1, c1, o1)  , r @ RegionOverlay(p2, g2, c2, o2) ) => Some(localDiff(l, r))
        case (l @ Col(as)                        , r @ Col(as2))                       => Some(localDiff(l, r))
        case (l @ Row(as)                        , r @ Row(as2))                       => Some(localDiff(l, r))
        case (_                                  , _)                                  => None
      }

    }
  }

}

object LabelWidgets {

  import matryoshka.data._

  import LabelWidgetF._

  def fixlw = Fix[LabelWidgetF](_)

  // def displayTarget(tr: TargetRegion) = fixlw(TargetRegionDisplay(tr))

  // def displayPage(pageId: Int@@PageID, bbox: LTBounds) =
  //   fixlw(PageDisplay(pageId, bbox))

  def targetOverlay(pageId: Int@@PageID, pageGeometry: LTBounds, clipTo: Option[LTBounds], overs: Seq[LabelWidget]) =
    fixlw(RegionOverlay(pageId, pageGeometry, clipTo, overs.toList))

  // def labeledTarget(target: TargetRegion, label: Option[Label]=None, score: Option[Double]=None) =
  //   fixlw(LabeledTarget(target, label, score))

  def reflow(tr: TextReflow) =
    fixlw(Reflow(tr))

  def textbox(tb: TB.Box) =
    fixlw(TextBox(tb))

  def figure(f: GeometricFigure) = fixlw(Figure(f))

  def col(lwidgets: LabelWidget*): LabelWidget =
    fixlw(Col(lwidgets.toList))

  def row(lwidgets: LabelWidget*): LabelWidget =
    fixlw(Row(lwidgets.toList))

  def pad(content: LabelWidget, pad: Padding): LabelWidget =
    fixlw(Pad(content, pad, None))

  def pad(content: LabelWidget, pad: Padding, color: Color): LabelWidget =
    fixlw(Pad(content, pad, Option(color)))

  def panel(content: LabelWidget, interact: Interaction): LabelWidget =
    fixlw(Panel(content, interact))

  def withId[IdTag: ClassTag](id: Int@@IdTag, a: LabelWidget): LabelWidget = {
    val tagClsname = implicitly[ClassTag[IdTag]].runtimeClass.getSimpleName
    fixlw(Identified(a, id.unwrap, tagClsname))
  }

}
