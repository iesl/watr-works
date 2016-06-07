package edu.umass.cs.iesl.watr
package spatial

import watrmarks._

case class Point(
  x: Double, y: Double
)

case class Line(
  p1: Point, p2: Point
)

case class TargetedBounds(
  id: Int@@RegionID,
  target: Int@@PageID,
  bbox: LTBounds
)

case class Zone(
  id: Int@@ZoneID,
  bboxes: List[TargetedBounds]
) {
  def withLabel(l: Label) = ZoneAndLabel(
    this.id, l
  )
}


case class PageGeometry(
  id: Int@@PageID,
  bounds: LTBounds,
  borders:Option[Borders]
)

case class PageChars(
  id: Int@@PageID,
  chars: Seq[CharBox]
)

case class CharBox(
  id: Int@@CharID,
  char: String,
  bbox: LTBounds,
  subs: String = "",
  wonkyCharCode: Option[Int] = None
)

case class ZoneAndLabel(zoneId: Int@@ZoneID, label:Label)

case class ZoneRecords(
  id: String,
  target: String,
  pageGeometries: Seq[PageGeometry],
  zones: Seq[Zone],
  labels: Seq[ZoneAndLabel],
  chars: Seq[PageChars]
)

trait ComponentDataTypeFormats extends TypeTagFormats {
  import play.api.libs.json._


  implicit def FormatLBBounds         = Json.format[LBBounds]
  implicit def FormatLTBounds         = Json.format[LTBounds]
  implicit def FormatTargetedBounds   = Json.format[TargetedBounds]
  implicit def FormatBorders          = Json.format[Borders]
  implicit def FormatLabel            = Json.format[Label]
  implicit def FormatPageGeometry     = Json.format[PageGeometry]
  implicit def FormatZone             = Json.format[Zone]
  implicit def FormatZoneAndLabel     = Json.format[ZoneAndLabel]
  implicit def FormatCharBox          = Json.format[CharBox]
  implicit def FormatPageChars        = Json.format[PageChars]
  implicit def FormatZoneRecords      = Json.format[ZoneRecords]


}
