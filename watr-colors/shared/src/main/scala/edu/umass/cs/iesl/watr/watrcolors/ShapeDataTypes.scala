package edu.umass.cs.iesl.watr
package watrcolors

sealed trait GeometricFigure
sealed trait Area
sealed trait SharedJs

object GeometricFigure {

  case class LTBounds(
    left: Double,
    top: Double,
    width: Double,
    height: Double
  ) extends GeometricFigure with Area with SharedJs

  case class LBBounds(
    left: Double,
    bottom: Double,
    width: Double,
    height: Double
  ) extends GeometricFigure with Area with SharedJs


  case class Point(
    x: Double, y: Double
  ) extends GeometricFigure with SharedJs

  case class Line(
    p1: Point, p2: Point
  ) extends GeometricFigure with SharedJs

}

import GeometricFigure._

case class PageGeometry(
  id: Int, //@@PageID,
  bounds: LTBounds
) extends SharedJs

case class TargetRegion(
  id: Int,     // @@RegionID,
  target: Int, // @@PageID,
  bbox: LTBounds
) extends SharedJs

case class TargetFigure(
  id: Int,
  page: Int,
  figure: GeometricFigure
) extends SharedJs

case class Zone(
  id: Int, // @@ZoneID,
  regions: Seq[TargetRegion]
) extends SharedJs

case class Component(
  id: Int, // @@ComponentID,
  targetRegion: TargetRegion
) extends SharedJs

case class Label(
  ns: String,
  key: String,
  value: Option[String]=None
) extends SharedJs

sealed trait TraceLog

object TraceLog {

  case object Noop                                   extends TraceLog
  case class SetPageGeometries(b: Seq[PageGeometry]) extends TraceLog
  case class Show(s: Seq[TargetRegion])              extends TraceLog
  case class ShowZone(s: Zone)                       extends TraceLog
  case class ShowComponent(s: Component)             extends TraceLog
  case class ShowLabel(l:Label)                      extends TraceLog
  case class FocusOn(s: TargetRegion)                extends TraceLog
  case class Indicate(figure: TargetFigure)          extends TraceLog
  case class Message(s: String)                      extends TraceLog
  case class All(ts: Seq[TraceLog])                  extends TraceLog
  case class Link(ts: Seq[TraceLog])                 extends TraceLog
  case class Group(name: String, ts: Seq[TraceLog])  extends TraceLog
  case class GroupEnd(name: String)                  extends TraceLog

}



trait ShapeDataTypePicklers {
  import boopickle.DefaultBasic._

  implicit val pGeometricFigure = compositePickler[GeometricFigure]
  implicit val pLTBounds = PicklerGenerator.generatePickler[LTBounds]
  implicit val pLBBounds = PicklerGenerator.generatePickler[LBBounds]
  implicit val pPoint= PicklerGenerator.generatePickler[Point]
  implicit val pLine = PicklerGenerator.generatePickler[Line]

  pGeometricFigure
    .addConcreteType[LTBounds]
    .addConcreteType[LBBounds]
    .addConcreteType[Point]
    .addConcreteType[Line]

  implicit val pSharedJs = compositePickler[SharedJs]
  implicit val pPageGeometry = PicklerGenerator.generatePickler[PageGeometry]
  implicit val pTargetRegion = PicklerGenerator.generatePickler[TargetRegion]
  implicit val pTargetFigure = PicklerGenerator.generatePickler[TargetFigure]
  implicit val pLabel = PicklerGenerator.generatePickler[Label]
  implicit val pZone = PicklerGenerator.generatePickler[Zone]
  implicit val pComponent = PicklerGenerator.generatePickler[Component]

  pSharedJs
    .addConcreteType[PageGeometry]
    .addConcreteType[TargetRegion]
    .addConcreteType[TargetFigure]
    .addConcreteType[Label]
    .addConcreteType[Zone]
    .addConcreteType[Component]

  import TraceLog._
  implicit val pDSL = compositePickler[TraceLog]

  implicit val pSetPageGeometry = PicklerGenerator.generatePickler[SetPageGeometries]
  implicit val pShow            = PicklerGenerator.generatePickler[Show]
  implicit val pShowZone        = PicklerGenerator.generatePickler[ShowZone]
  implicit val pShowComponent   = PicklerGenerator.generatePickler[ShowComponent]
  implicit val pShowLabel       = PicklerGenerator.generatePickler[ShowLabel]
  implicit val pFocusOn         = PicklerGenerator.generatePickler[FocusOn]
  implicit val pIndicate        = PicklerGenerator.generatePickler[Indicate]
  implicit val pMessage         = PicklerGenerator.generatePickler[Message]
  implicit val pAll             = PicklerGenerator.generatePickler[All]
  implicit val pLink            = PicklerGenerator.generatePickler[Link]
  implicit val pGroup           = PicklerGenerator.generatePickler[Group]
  implicit val pGroupEnd        = PicklerGenerator.generatePickler[GroupEnd]

  pDSL
    .addConcreteType[SetPageGeometries]
    .addConcreteType[Show]
    .addConcreteType[ShowZone]
    .addConcreteType[ShowComponent]
    .addConcreteType[ShowLabel]
    .addConcreteType[FocusOn]
    .addConcreteType[Indicate]
    .addConcreteType[Message]
    .addConcreteType[All]
    .addConcreteType[Link]
    .addConcreteType[Group]
    .addConcreteType[GroupEnd]

}
