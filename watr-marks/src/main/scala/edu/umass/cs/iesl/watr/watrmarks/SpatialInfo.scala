package edu.umass.cs.iesl.watr
package watrmarks


import net.sf.jsi.Rectangle
import scala.collection.mutable

import net.sf.jsi.SpatialIndex
import net.sf.jsi.rtree.RTree
import net.sf.jsi

import play.api.libs.json
import json._
import scalaz.{Tag, @@}



case class FontInfo(
  // fontName: String,
  fontFamily: String,
  fontSize: String
)

object jsiRectangle {
  def apply(
    x: Double, y: Double, width: Double, height: Double
  ): Rectangle = apply(
    x.toFloat, y.toFloat, width.toFloat, height.toFloat
  )

  def apply(
    x: Float, y: Float, width: Float, height: Float
  ): Rectangle = new Rectangle(
    x, y, x+width, y+height
  )

  def toLTBounds(r: jsi.Rectangle): LTBounds = {
    LTBounds(
      left = r.minX.toDouble,
      top =  r.minY.toDouble,
      width = (r.maxX - r.minX).toDouble,
      height = (r.maxY - r.minY).toDouble
    )
  }
}



object Bounds {

  implicit class RicherLTBounds(val tb: LTBounds) extends AnyVal {

    def toJsiRectangle: jsi.Rectangle = {
       jsiRectangle(tb.left, tb.top, tb.width, tb.height)
    }

    def toLBBounds: LBBounds = {
      LBBounds(
        left = tb.left,
        bottom =  tb.top+tb.height,
        width = tb.width,
        height = tb.height
      )
    }
  }

  implicit class RicherLBBounds(val tb: LBBounds) extends AnyVal {
    def toLTBounds: LTBounds = {
      LTBounds(
        left = tb.left,
        top =  tb.bottom-tb.height,
        width = tb.width,
        height = tb.height
      )

    }
  }
}

sealed trait Bounds

case class LTBounds(
  left: Double,
  top: Double,
  width: Double,
  height: Double
) extends Bounds

case class LBBounds(
  left: Double,
  bottom: Double,
  width: Double,
  height: Double
) extends Bounds

case class Borders(
  bleft: Double,
  btop: Double,
  bright: Double,
  bbottom: Double
)

case class TargetedBounds(
  target: Int@@PageID,
  bbox: LTBounds
)


sealed trait ZoneID
sealed trait LabelID

case class Zone(
  id: Int@@ZoneID,
  bboxes: List[TargetedBounds]
) {
  def withLabel(l: Label) = ZoneAndLabel(
    this.id, l
  )
}

case class Label(
  ns: String,
  key: String,
  value: Option[String]  = None
)


sealed trait PageID

case class PageGeometry(
  id: Int,
  bounds: LBBounds,
  borders:Option[Borders]
)


case class ZoneAndLabel(zoneId: Int@@ZoneID, label:Label)

case class ZoneRecords(
  id: String,
  target: String,
  pageGeometries: List[PageGeometry],
  zones: List[Zone],
  labels: List[ZoneAndLabel]
)



class ZoneIndexer  {


  def zoneIds = IdGenerator[ZoneID]
  def sindex: SpatialIndex = new RTree()

  val zoneMap = mutable.HashMap[Int@@ZoneID, Zone]()
  val pageGeometries = mutable.Map[Int, PageGeometry]()
  // val targetedZones = mutable.HashMap[String, Zone]()
  val zoneLabelMap = mutable.HashMap[Int, mutable.ArrayBuffer[Label]]()

  def addPage(p: PageGeometry): PageGeometry = {
    if(pageGeometries.contains(p.id)) {
      sys.error("adding new page w/existing id")
    }

    pageGeometries.put(p.id, p)
    p
  }

  def addZone(zone: Zone, labels: Label*): Int@@ZoneID = {
    val zoneId = zoneIds.nextId
    zoneMap.put(zoneId, zone)
    zone.bboxes.foreach{ targetedBounds  =>
      sindex.add(
        targetedBounds.bbox.toJsiRectangle,
        ZoneID.unwrap(zoneId)
      )
    }

    val lls = zoneLabelMap.getOrElseUpdate(ZoneID.unwrap(zoneId), mutable.ArrayBuffer[Label]())
    labels.foreach { label =>
      lls += label
    }
    zoneId
  }
}



object ZoneIndexer extends SpatialJsonFormat {
  def minMaxPairToPointWidth(minMax: (Double, Double)): (Double, Double) = {
    (minMax._1, minMax._2-minMax._1)
  }

  def vconcat(psis: ZoneIndexer*): ZoneIndexer = {

    ???
  }


  def loadSpatialIndices(zoneRec: ZoneRecords): ZoneIndexer = {
    println(zoneRec)

    val zindexer = new ZoneIndexer()

    zoneRec.pageGeometries.foreach { p =>
      zindexer.addPage(p)
    }

    zoneRec.zones.foreach { z =>
      zindexer.addZone(z)
    }

    zindexer
  }
}

trait SpatialJsonFormat {

  val ReadPageID: Reads[Int@@PageID]   = __.read[Int].map(i => Tag.of[PageID](i))
  val WritePageID: Writes[Int@@PageID] = Writes[Int@@PageID] { i => JsNumber(PageID.unwrap(i)) }
  implicit def FormatPageID            = Format(ReadPageID, WritePageID)

  val ReadZoneID: Reads[Int@@ZoneID]   = __.read[Int].map(i => Tag.of[ZoneID](i))
  val WriteZoneID: Writes[Int@@ZoneID] = Writes[Int@@ZoneID] { i => JsNumber(ZoneID.unwrap(i)) }
  implicit def FormatZoneID            = Format(ReadZoneID, WriteZoneID)


  implicit def residentFormat = Json.format[LBBounds]
  implicit def LTBoundsFormat = Json.format[LTBounds]
  implicit def FormatTargetedBounds = Json.format[TargetedBounds]
  implicit def FormatBorders = Json.format[Borders]
  implicit def FormatLabel = Json.format[Label]
  implicit def FormatPageGeometry = Json.format[PageGeometry]
  implicit def FormatZone = Json.format[Zone]
  implicit def FormatZoneAndLabel = Json.format[ZoneAndLabel]

  implicit def FormatZoneRecords = Json.format[ZoneRecords]


  // __.read(
  //     (__ \ "id").read[Int@@ZoneID] and
  //     (__ \ "label").read[String] and
  //     (__ \ "bboxes").read[List[TargetedBounds]]
  //     tupled
  //   ).map{ rec => Zone(rec._1, rec._3) }


}
