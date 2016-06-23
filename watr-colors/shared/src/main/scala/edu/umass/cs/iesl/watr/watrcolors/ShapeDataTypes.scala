package edu.umass.cs.iesl. watr
package watrcolors


sealed trait Overlay

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

trait ShapeDataTypePicklers {
  import boopickle.DefaultBasic._

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
