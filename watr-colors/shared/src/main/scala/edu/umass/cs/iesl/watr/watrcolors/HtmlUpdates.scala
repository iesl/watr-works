package edu.umass.cs.iesl.watr
package watrcolors

import boopickle.DefaultBasic._


sealed trait HtmlUpdate

final case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
final case class HtmlAppend(css: String, content: String) extends HtmlUpdate
final case class HtmlReplace(css: String, content: String) extends HtmlUpdate
final case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
final case class HtmlRemove(css: String) extends HtmlUpdate

sealed trait Overlay
sealed trait OverlayInfo

case class CharInfo(
  info: String
) extends OverlayInfo

case class LabelInfo(
  label: String
) extends OverlayInfo

case class Point(
  x: Double, y: Double
) extends Overlay {
  override def toString = {
    s"""(${x}, ${y})"""
  }
}

case class Line(
  p1: Point, p2: Point
) extends Overlay

case class BBox(
  x: Double, y: Double, width: Double, height: Double
) extends Overlay {
  override def toString = {
    s"""BBox(x:${x} y:${y} w:${width} h:${height})"""
  }
}

object VisualTrace {
  sealed trait DSL // [A]

  case object Noop extends DSL //  // // [Unit]
  case class SetViewport(b: BBox) extends DSL //  // // [Unit]
  case class GetViewport() extends DSL// [BBox]
  case class Show(s: Overlay) extends DSL// [Unit]
  case class ShowVDiff(d1: Double, d2: Double) extends DSL// [Unit]
  case class FocusOn(s: Overlay) extends DSL// [Unit]
  case class HRuler(s: Double) extends DSL// [Overlay]
  case class Message(s: String) extends DSL// [Unit]



}

object Picklers {



  implicit val pickler0 = compositePickler[HtmlUpdate]

  implicit val p1: Pickler[HtmlAppend] = PicklerGenerator.generatePickler[HtmlAppend]
  implicit val p2: Pickler[HtmlPrepend] = PicklerGenerator.generatePickler[HtmlPrepend]
  implicit val p3: Pickler[HtmlRemove] = PicklerGenerator.generatePickler[HtmlRemove]
  implicit val p4: Pickler[HtmlReplaceInner] = PicklerGenerator.generatePickler[HtmlReplaceInner]
  implicit val p5: Pickler[HtmlReplace] = PicklerGenerator.generatePickler[HtmlReplace]

  pickler0
    .addConcreteType[HtmlPrepend]
    .addConcreteType[HtmlAppend]
    .addConcreteType[HtmlRemove]
    .addConcreteType[HtmlReplaceInner]
    .addConcreteType[HtmlReplace]

  implicit val pickleOverlayInfo = compositePickler[OverlayInfo]

  implicit val q1 = PicklerGenerator.generatePickler[CharInfo]
  implicit val q2 = PicklerGenerator.generatePickler[LabelInfo]

  pickleOverlayInfo
    .addConcreteType[CharInfo]
    .addConcreteType[LabelInfo]




  implicit val pOverlay = compositePickler[Overlay]
  implicit val pBBox = PicklerGenerator.generatePickler[BBox]
  implicit val pPoint= PicklerGenerator.generatePickler[Point]
  implicit val pLine = PicklerGenerator.generatePickler[Line]

  pOverlay
    .addConcreteType[BBox]
    .addConcreteType[Point]
    .addConcreteType[Line]


  import VisualTrace._
  implicit val pDSL = compositePickler[DSL]

  implicit val pNoop        = PicklerGenerator.generatePickler[Noop.type]
  implicit val pSetViewport = PicklerGenerator.generatePickler[SetViewport]
  implicit val pGetViewport = PicklerGenerator.generatePickler[GetViewport]
  implicit val pShow        = PicklerGenerator.generatePickler[Show]
  implicit val pShowVDiff   = PicklerGenerator.generatePickler[ShowVDiff]
  implicit val pFocusOn     = PicklerGenerator.generatePickler[FocusOn]
  implicit val pHRuler      = PicklerGenerator.generatePickler[HRuler]
  implicit val pMessage     = PicklerGenerator.generatePickler[Message]

  pDSL
    .addConcreteType[Noop.type]
    .addConcreteType[SetViewport]
    .addConcreteType[GetViewport]
    .addConcreteType[Show]
    .addConcreteType[ShowVDiff]
    .addConcreteType[FocusOn]
    .addConcreteType[HRuler]
    .addConcreteType[Message]

}
