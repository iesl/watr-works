package edu.umass.cs.iesl.watr
package labeling

import scalaz.{@@ => _, _}, Scalaz._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns._

import geometry._
import geometry.syntax._
import textreflow.data._
import watrmarks.Label
import textboxing.{TextBoxing => TB}
import utils.Color
import utils.Colors
import scala.reflect._

/**
  LabelWidgets provide a way to combine rectangular regions into a single layout.
  */


sealed trait LabelWidgetF[+A] {
  def wid: Int@@WidgetID
}

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

  case class RegionOverlay[A](
    wid: Int@@WidgetID,
    // pageId: Int@@PageID,
    // geometry: LTBounds,
    // clipTo: Option[LTBounds],
    under: TargetRegion,
    overlays: List[A]
  ) extends LabelWidgetF[A]

  case class TextBox(
    wid: Int@@WidgetID,
    textBox: TB.Box
  ) extends LabelWidgetF[Nothing]

  case class Reflow(
    wid: Int@@WidgetID,
    textReflow: TextReflow
  ) extends LabelWidgetF[Nothing]

  case class Row[A](
    wid: Int@@WidgetID,
    as: List[A]
  ) extends LabelWidgetF[A]
  case class Col[A](
    wid: Int@@WidgetID,
    as: List[A]
  ) extends LabelWidgetF[A]

  case class Pad[A](
    wid: Int@@WidgetID,
    a: A,
    pad: Padding,
    color: Option[Color]
  ) extends LabelWidgetF[A]

  case class Figure(
    wid: Int@@WidgetID,
    figure: GeometricFigure
  ) extends LabelWidgetF[Nothing]

  case class Panel[A](
    wid: Int@@WidgetID,
    a: A,
    interaction: Interaction
  ) extends LabelWidgetF[A]

  case class Identified[A](
    wid: Int@@WidgetID,
    a: A,
    id: Int,
    idclass: String
  ) extends LabelWidgetF[A]

  // Kludgy leaf-type to allow for easier deletion of nodes
  case object Terminal extends LabelWidgetF[Nothing] {
    val wid: Int@@WidgetID = TypeTags.WidgetID(0)
  }


  implicit def LabelWidgetTraverse: Traverse[LabelWidgetF] = new Traverse[LabelWidgetF] {
    def traverseImpl[G[_], A, B](
      fa: LabelWidgetF[A])(
      f: A => G[B])(
      implicit G: Applicative[G]
    ): G[LabelWidgetF[B]] = {
      fa match {
        case l : RegionOverlay[A]            => l.overlays.traverse(f).map(o => l.copy(overlays=o))
        case l @ Row(wid, as)                => as.traverse(f).map(Row(wid, _))
        case l @ Col(wid,as)                 => as.traverse(f).map(Col(wid, _))
        case l @ Pad(wid, a, pd, clr)        => f(a).map(Pad(wid, _, pd, clr))
        case l : TextBox                     => G.point(l.copy())
        case l : Reflow                      => G.point(l.copy())
        case l : Figure                      => G.point(l.copy())
        case l @ Panel(wid, a, i)            => f(a).map(Panel(wid, _, i))
        case l @ Identified(wid, a, id, cls) => f(a).map(Identified(wid, _, id, cls))
        case l @ Terminal                     => G.point(Terminal)
      }
    }
  }

  implicit def LabelWidgetFunctor: Functor[LabelWidgetF] = LabelWidgetTraverse

  implicit def LabelWidgetShow: Delay[Show, LabelWidgetF] = new Delay[Show, LabelWidgetF] {
    def apply[A](show: Show[A]) = Show.show {
      case l : RegionOverlay[A]               => s"$l"
      case l @ Reflow(wid, tr)                => s"reflow()"
      case l @ TextBox(wid, tb)               => s"textbox"
      case l @ Row(wid, as)                   => s"$l"
      case l @ Col(wid, as)                   => s"$l"
      case l @ Pad(wid, a, padding, color)    => s"$l"
      case l @ Figure(wid,f)                  => l.toString
      case l @ Panel(wid, a, i)               => l.toString
      case l @ Identified(wid, a, id, cls)    => l.toString
      case l @ Terminal                       => l.toString
    }
  }

  implicit def equal: Delay[Equal, LabelWidgetF] = new Delay[Equal, LabelWidgetF] {
    def apply[A](eq: Equal[A]) = Equal.equal((a, b) => {
      implicit val ieq = eq;
      (a.wid == b.wid) && ((a, b) match {

        case (l @ RegionOverlay(wid1, u1, o1), l2 @ RegionOverlay(wid2, u2, o2)) => u1===u2 && o1===o2
        case (l @ Reflow(wid, tr)                   , l2 : Reflow              ) => false
        case (l @ TextBox(wid, tb)                  , l2 : TextBox             ) => tb.toString()==l2.textBox.toString()
        case (l @ Row(wid, as)                      , l2 : Row[A]              ) => as === l2.as
        case (l @ Col(wid, as)                      , l2 : Col[A]              ) => as === l2.as
        case (l @ Pad(wid, a, padding, color)       , l2 : Pad[A]              ) => a === l2.a
        case (l @ Figure(wid, f)                    , l2 : Figure              ) => f === l2.figure
        case (l @ Panel(wid, a, i)                  , l2 : Panel[A]            ) => a === l2.a
        case (l @ Identified(wid, a, id, cls)       , l2 : Identified[A]       ) => a === l2.a && id==l2.id && cls==l2.idclass
        case (l @ Terminal                          , l2 @ Terminal            ) => true

        case (_ , _) => false

      })
    })
  }

  implicit val diffable: Diffable[LabelWidgetF] = new Diffable[LabelWidgetF] {

    def diffImpl[T[_[_]]: BirecursiveT](l: T[LabelWidgetF], r: T[LabelWidgetF]):
        Option[DiffT[T, LabelWidgetF]] = {

      (l.project, r.project) match {
        // case (l @ RegionOverlay(wid1, p1, g1, c1, o1)  , r @ RegionOverlay(wid2, p2, g2, c2, o2))  =>
        case (l @ RegionOverlay(wid1, u1, o1), r @ RegionOverlay(wid2, u2, o2)) =>
          val ldiff = diffTraverse(o1, o2)

          val locallySimilar = u1===u2  // && o1===o2

          val recdiff = if (locallySimilar) {
            Similar[T, LabelWidgetF, T[Diff[T, LabelWidgetF, ?]]](
              RegionOverlay[DiffT[T, LabelWidgetF]](wid1, u1, ldiff)
            ).embed
          } else {
            Different[T, LabelWidgetF, T[Diff[T, LabelWidgetF, ?]]](
              l.embed, r.embed
            ).embed
          }

          Some(recdiff)

        case (l : Figure      , r : Figure) => Some(localDiff(l, r))
        case (l : Col[_]      , r : Col[_]) => Some(localDiff(l, r))
        case (l : Row[_]      , r : Row[_]) => Some(localDiff(l, r))
        case (_               , _)          => None
      }

    }
  }

}

object LabelWidgets {

  val idgen = utils.IdGenerator[WidgetID]()

  import matryoshka.data._

  import LabelWidgetF._

  def fixlw = Fix[LabelWidgetF](_)

  def targetOverlay(targetRegion: TargetRegion, overlays: Seq[LabelWidget]) =
    fixlw(RegionOverlay(idgen.nextId, targetRegion, overlays.toList))

  def reflow(tr: TextReflow) =
    fixlw(Reflow(idgen.nextId, tr))

  def textbox(tb: TB.Box) =
    fixlw(TextBox(idgen.nextId, tb))

  def figure(f: GeometricFigure) = fixlw(Figure(idgen.nextId, f))

  def col(lwidgets: LabelWidget*): LabelWidget =
    fixlw(Col(idgen.nextId, lwidgets.toList))

  def row(lwidgets: LabelWidget*): LabelWidget =
    fixlw(Row(idgen.nextId, lwidgets.toList))

  def pad(content: LabelWidget, pad: Padding): LabelWidget =
    fixlw(Pad(idgen.nextId, content, pad, None))

  def pad(content: LabelWidget, pad: Padding, color: Color): LabelWidget =
    fixlw(Pad(idgen.nextId, content, pad, Option(color)))

  def panel(content: LabelWidget, interact: Interaction): LabelWidget =
    fixlw(Panel(idgen.nextId, content, interact))

  def withId[IdTag: ClassTag](id: Int@@IdTag, a: LabelWidget): LabelWidget = {
    val tagClsname = implicitly[ClassTag[IdTag]].runtimeClass.getSimpleName
    fixlw(Identified(idgen.nextId, a, id.unwrap, tagClsname))
  }

  lazy val terminal = fixlw(Terminal)

}
