package edu.umass.cs.iesl.watr
package spatial


import net.sf.jsi.Rectangle
import scala.collection.mutable

import net.sf.jsi.SpatialIndex
import net.sf.jsi.rtree.RTree
import net.sf.jsi

import play.api.libs.json
import json._
import scalaz.{Tag, @@}

import utils.CompassDirection
import watrmarks._
import TypeTags._


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

  def charBoxesBounds(charBoxes: Seq[CharBox]): LTBounds = {
    if (charBoxes.isEmpty) {
      LTBounds(0, 0, 0, 0)
    } else {
      val cbs = charBoxes.sortBy(_.bbox.left)
      val top = cbs.map(_.bbox.top).min
      val bottom = cbs.map(_.bbox.bottom).max
      val l=cbs.head.bbox.left
      val r=cbs.last.bbox.right

      LTBounds(l, top, r-l, bottom-top)
    }
  }


  def fmt = (d: Double) => f"${d}%1.2f"

  implicit class RicherDouble(val d: Double) extends AnyVal {
    def pp:String = fmt(d)

    // def ltFuzzy(tolerance: Double)(d2: Double): Boolean = {
    //   d > d2
    //   d < (d2+tolerance)
    // }

    // def gtFuzzy(tolerance: Double)(d2: Double): Boolean =
    //   compareFuzzy(tolerance)(d2) > 0

    def eqFuzzy(tolerance: Double)(d2: Double): Boolean =
      compareFuzzy(tolerance)(d2) == 0


    def compareFuzzy(tolerance: Double)(d2: Double): Int = {
      if (math.abs(d - d2) < tolerance) 0
      else if (d < d2) -1
      else 1
    }
  }

  implicit class RicherPoint(val p0: Point) extends AnyVal {

    def -(p1:Point): Point = {
      Point(p0.x-p1.x, p0.y-p1.y)
    }


    def hdist(p1: Point): Double = math.abs(p0.x - p1.x)
    def vdist(p1: Point): Double = math.abs(p0.y - p1.y)

    def dist(p1: Point): Double = {
      val x = (p0 hdist p1)
      val y = (p0 vdist p1)
      math.sqrt(x*x + y*y)
    }

    def angleTo(p1: Point): Double = {
      if (p0.x > p1.x) {
        math.atan2(p0.y - p1.y, p0.x - p1.x);
      } else {
        math.atan2(p1.y - p0.y, p1.x - p0.x);
      }
    }
    def prettyPrint: String = {
      s"""[${fmt(p0.x)}, ${fmt(p0.y)}]"""
    }

  }

  implicit val ptOrd = Ordering.by{ p: Point =>
    (p.x, p.y)
  }

  implicit val lineOrd = Ordering.by{ l:Line =>
    (l.p1, l.p2)
  }

  implicit class RicherLine(val line: Line) extends AnyVal {

    def ordered(l2: Line): (Line, Line) = {
      if (lineOrd.compare(line, l2) <= 0) (line, l2)
      else (l2, line)
    }


    // def inside(l2: Line): Boolean = {
    //   val (lmin, lmax) = ordered(l2)

    //   true
    // }

    // def overlaps(l2: Line): Boolean = {

    //   true
    // }
    // def intersects(l2: Line): Boolean = {
    //   true

    // }
  }

  implicit class RicherLTBounds(val tb: LTBounds) extends AnyVal {
    def right = tb.left+tb.width
    def bottom = tb.top+tb.height

    def translate(x: Double, y: Double): LTBounds = {
      tb.copy(
        left=tb.left+x,
        top=tb.top+y
      )
    }

    def union(b: LTBounds): LTBounds = {
      val left   = math.min(tb.left, b.left)
      val top    = math.min(tb.top, b.top)
      val right = math.max(tb.right, b.right)
      val bottom = math.max(tb.bottom, b.bottom)
      LTBounds(
        left, top,
        right-left,
        bottom-top
      )
    }

    // Compass direction point (bbox -> left-center, right-center, top-corner, etc)
    def toWesternPoint: Point = Point((tb.left), (tb.top+tb.height/2))
    def toEasternPoint: Point = Point((tb.left+tb.width), (tb.top+tb.height/2))

    import CompassDirection._

    def toPoint(cd: CompassDirection): Point ={
      def centerX = (tb.left+tb.width/2)
      def centerY = (tb.top+tb.height/2)

      cd match {
        case N  => Point(centerX, tb.top)
        case S  => Point(centerX, tb.bottom)
        case E  => Point(tb.right, centerY)
        case W  => Point(tb.left, centerY)
        case NW => Point(tb.left, tb.top)
        case SW => Point(tb.left, tb.bottom)
        case NE => Point(tb.right, tb.top)
        case SE => Point(tb.right, tb.bottom)
      }
    }

    def toLine(cd: CompassDirection): Line = cd match {
      case N  => Line(tb.toPoint(NW), tb.toPoint(NE))
      case S  => Line(tb.toPoint(SW), tb.toPoint(SE))
      case E  => Line(tb.toPoint(NE), tb.toPoint(SE))
      case W  => Line(tb.toPoint(NW), tb.toPoint(SW))
      case NW => ???
      case SW => ???
      case NE => ???
      case SE => ???
    }

    def xProjection(): Line = Line(
      Point((tb.left), 0),
      Point((tb.right), 0)
    )

    def yProjection(): Line = Line(
      Point(0, (tb.top)),
      Point(0, (tb.bottom))
    )



    def toCenterPoint: Point = Point(
      (tb.left+tb.width/2),
      (tb.top+tb.height/2)
    )

    def centerDistanceTo(other: LTBounds): Double = {
      val cx = (tb.left+tb.width/2).toFloat
      val cy = (tb.top+tb.height/2).toFloat
      val cx2 = (other.left+other.width/2).toFloat
      val cy2 = (other.top+other.height/2).toFloat

      math.sqrt(
        math.pow((cx-cx2).toDouble, 2) + math.pow((cy-cy2).toDouble, 2)
      )
    }

    def toJsiRectangle: jsi.Rectangle = {
       jsiRectangle(tb.left, tb.top, tb.width, tb.height)
    }

    def jsiCenterPoint: jsi.Point = {
      new jsi.Point(
        (tb.left+tb.width/2).toFloat,
        (tb.top+tb.height/2).toFloat
      )
    }

    def toLBBounds: LBBounds = {
      LBBounds(
        left = tb.left,
        bottom =  tb.top+tb.height,
        width = tb.width,
        height = tb.height
      )
    }
    def prettyPrint: String = {
      val left = tb.left
      val top=  tb.top
      val width = tb.width
      val height = tb.height
      s"""(l:${fmt(left)}, t:${fmt(top)}, w:${fmt(width)}, h:${fmt(height)})"""
    }

    def compactPrint: String = {
      val left = tb.left
      val top=  tb.top
      val width = tb.width
      val height = tb.height
      s"""[${left.pp}, ${top.pp}, ${width.pp}, ${height.pp}]"""
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
    def prettyPrint: String = {
      val left = tb.left
      val bottom=  tb.bottom
      val width = tb.width
      val height = tb.height
      s"""(l:${fmt(left)}, b:${fmt(bottom)}, w:${fmt(width)}, h:${fmt(height)})"""
    }
  }


  implicit class RicherCharBox(val charBox: CharBox) extends AnyVal {
    // case class CharBox(
    //   id: Int@@CharID,
    //   char: String,
    //   bbox: LTBounds,
    //   subs: String = "",
    //   wonkyCharCode: Option[Int] = None
    // )


    def prettyPrint: String = {
      charBox.wonkyCharCode
        .map({ code =>
          if (code==32) { "_"  }
          else { s"#${code}?" }
        })
        .getOrElse({
          if (!charBox.subs.isEmpty()) charBox.subs
          else charBox.char
        })
    }

    def bestGuessChar: String = {
      charBox.wonkyCharCode
        .map({ code =>
          if (code==32) { s" "  }
          else { s"#{${code}}" }
        })
        .getOrElse({
          if (!charBox.subs.isEmpty()) charBox.subs
          else charBox.char
        })
    }

    def isWonky: Boolean = charBox.wonkyCharCode.isDefined

    def isSpace: Boolean = charBox.wonkyCharCode.exists(_==32)

  }
}

import Bounds._

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


class ZoneIndexer  {

  val pageRIndexes = mutable.HashMap[Int@@PageID, SpatialIndex]()
  val pageGeometries = mutable.Map[Int@@PageID, PageGeometry]()
  val pageChars = mutable.HashMap[Int@@PageID, mutable.ArrayBuffer[CharBox]]()
  // val charBoxes = mutable.HashMap[Int@@CharID, CharBox]()
  val charBoxes = mutable.LongMap[CharBox]()

  val zoneMap = mutable.HashMap[Int@@ZoneID, Zone]()
  val zoneLabelMap = mutable.HashMap[Int@@ZoneID, mutable.ArrayBuffer[Label]]()

  val regionToZone = mutable.HashMap[Int@@RegionID, Zone]()

  def getPageGeometry(p: Int@@PageID) = pageGeometries(p)

  def getComponent(pageId: Int@@PageID, charId: Int@@CharID): CharBox = {
    charBoxes(charId.unwrap.toLong)
  }

  // See getComponentsUnfiltered() for explanation of filterNot
  def getComponents(pageId: Int@@PageID): Seq[CharBox] = {
    pageChars(pageId).filterNot(_.isSpace)
  }


  // Some chars from a pdf are unuseable, either unprintable or embedded space characters.
  //   This will return all of them
  def getComponentsUnfiltered(pageId: Int@@PageID): Seq[CharBox] = {
    pageChars(pageId)
  }

  def getZoneLabels(id: Int@@ZoneID): Seq[Label] = {
    zoneLabelMap.get(id).getOrElse(Seq())
  }

  def getPages(): List[Int@@PageID] = {
    pageRIndexes.keys.toList.sortBy(PageID.unwrap(_))
  }


  def addPage(p: PageGeometry): Unit = {

    if(pageGeometries.contains(p.id)) {
      sys.error("adding new page w/existing id")
    }

    pageGeometries.put(p.id, p)
    val si: SpatialIndex = new RTree()
    si.init(null)
    pageRIndexes.put(p.id, si)

    pageChars.getOrElseUpdate(p.id, mutable.ArrayBuffer())

  }

  def addCharInfo(pageId: Int@@PageID, cb: CharBox): Unit = {
    val rindex = pageRIndexes(pageId)
    rindex.add(cb.bbox.toJsiRectangle, cb.id.unwrap.toInt)
    pageChars(pageId).append(cb)
    charBoxes.put(cb.id.unwrap, cb)
  }

  def addZone(zone: Zone): Unit = {
    val zoneId = zone.id
    zoneMap.put(zoneId, zone).map(existing => sys.error(s"zone already exists in zoneMap"))
    zone.bboxes.foreach{ targetedBounds  =>
      regionToZone.put(targetedBounds.id, zone)
      val rindex = pageRIndexes(targetedBounds.target)
      rindex.add(
        targetedBounds.bbox.toJsiRectangle,
        RegionID.unwrap(targetedBounds.id)
      )
    }
  }

  def addLabels(zl: ZoneAndLabel): Unit = {
    val lls = zoneLabelMap.getOrElseUpdate(zl.zoneId, mutable.ArrayBuffer[Label]())
    lls.append(zl.label)
  }

  import gnu.trove.procedure.TIntProcedure

  class CollectRegionIds extends TIntProcedure {
    import scala.collection.mutable
    val ids = mutable.ArrayBuffer[Int]()

    override def execute(id: Int): Boolean = {
      ids.append(id)
      true
    }

    def getIDs: Seq[Int] = {
      ids
    }
  }

  def putCharBox(cb: CharBox): Unit = {
    charBoxes.put(cb.id.unwrap, cb)
  }

  def getCharBox(id: Int): CharBox = {
    charBoxes(id.toLong)
  }

  def getCharBox(id: Int@@CharID): CharBox = {
    charBoxes(id.unwrap)
  }

  def getCharBox(id: Long): CharBox = {
    charBoxes(id)
  }

  def queryCharsIntersects(page: Int@@PageID, q: LTBounds): Seq[CharBox] = {
    val rindex = pageRIndexes(page)

    val collectRegions = new CollectRegionIds()
    // rindex.intersects(x$1: Rectangle, x$2: TIntProcedure)
    rindex.intersects(q.toJsiRectangle, collectRegions)
    val neighbors = collectRegions.getIDs.filter{ id =>
      charBoxes.contains(id.toLong)
    }
    neighbors.map(cid => charBoxes(cid.toLong))
  }

  def queryChars(page: Int@@PageID, q: LTBounds): Seq[CharBox] = {
    val rindex = pageRIndexes(page)

    val collectRegions = new CollectRegionIds()
    rindex.contains(q.toJsiRectangle, collectRegions)
    val neighbors = collectRegions.getIDs.filter{ id =>
      charBoxes.contains(id.toLong)
    }

    neighbors map getCharBox
  }


  // def nearestChars(page: Int@@PageID, q: LTBounds, radius: Float): Seq[Int@@CharID] = {
  //   val rindex = pageRIndexes(page)

  //   val collectRegions = new CollectRegionIds()
  //   rindex.nearest(q.jsiCenterPoint, collectRegions, radius)
  //   val neighbors = collectRegions.getIDs.filter{ id =>
  //     charBoxes.contains(CharID(id))
  //   }
  //   neighbors.map(CharID(_))
  // }

  // def nearestNCharIDs(page: Int@@PageID, qbox: LTBounds, n: Int, radius: Float): Seq[Int@@CharID] = {
  //   val rindex = pageRIndexes(page)
  //   val ctr = qbox.toCenterPoint
  //   val searchRect = LTBounds(
  //     left=ctr.x- (radius/2),
  //     top=ctr.y- (radius/2),
  //     width=radius.toDouble,
  //     height=radius.toDouble
  //   )

  //   val collectRegions = new CollectRegionIds()
  //   println(s" searching ${n} nearest from bbox${searchRect.prettyPrint}, ctr=${ctr}, radius: ${radius}")

  //   rindex.intersects(searchRect.toJsiRectangle, collectRegions)
  //   println(s""" found ${collectRegions.getIDs.mkString(",")} """)
  //   collectRegions
  //     .getIDs
  //     .map({ id =>
  //       val cbox = charBoxes(CharID(id))
  //       val dist = cbox.bbox.toCenterPoint.vdist(ctr)
  //       (dist, cbox)
  //     })
  //     .sortBy(_._1)
  //     .take(n)
  //     .map(_._2.id)
  // }


  // def nearestNChars(page: Int@@PageID, qbox: LTBounds, n: Int, radius: Float): Seq[CharBox] = {
  def nearestNChars(page: Int@@PageID, fromChar: CharBox, n: Int, radius: Float): Seq[CharBox] = {
    val rindex = pageRIndexes(page)
    val ctr = fromChar.bbox.toCenterPoint
    val searchRect = LTBounds(
      left   =ctr.x - radius,
      top    =ctr.y - radius,
      width  =(radius*2.0).toDouble,
      height =(radius*2.0).toDouble
    )

    val collectRegions = new CollectRegionIds()
    // println(s" searching ${n} nearest from c=${fromChar.char} bbox ${searchRect.prettyPrint}, ctr=${ctr}, radius: ${radius}")

    rindex.intersects(searchRect.toJsiRectangle, collectRegions)
    // println(s""" found ${collectRegions.getIDs.mkString(",")} """)
    collectRegions.getIDs
      .map({ id =>
        val cbox = getCharBox(id)
        (cbox.bbox.toCenterPoint.dist(ctr), cbox)
      })
      .filterNot(_._2.id == fromChar.id)
      .sortBy(_._1)
      .take(n)
      .map(_._2)
  }

  def query(page: Int@@PageID, q: LTBounds): Seq[Zone] = {
    println(s"ZoneIndex.query(${page}, ${q.prettyPrint})")
    val rindex = pageRIndexes(page)

    val collectRegions = new CollectRegionIds()
    rindex.contains(q.toJsiRectangle, collectRegions)
    // rindex.intersects(q.toJsiRectangle, collectRegions)
    val regions = collectRegions.getIDs
    val zones = collectRegions
      .getIDs.map{ regionId => regionToZone(RegionID(regionId)) }

    zones.sortBy { z => z.bboxes.head.bbox.left }
  }

}



object ZoneIndexer extends SpatialJsonFormat {
  def minMaxPairToPointWidth(minMax: (Double, Double)): (Double, Double) = {
    (minMax._1, minMax._2-minMax._1)
  }

  def loadSpatialIndices(charsAndGeometry: Seq[(PageChars, PageGeometry)]): ZoneIndexer = {

    val zindexer = new ZoneIndexer()
    charsAndGeometry.foreach { case(chars, geom)  =>
      zindexer.addPage(geom)

      chars.chars.foreach { cb =>
        if (!cb.isSpace) {
          zindexer.addCharInfo(geom.id, cb)
        }
      }
    }
    zindexer
  }

  def loadSpatialIndices(zoneRec: ZoneRecords): ZoneIndexer = {

    val zindexer = new ZoneIndexer()

    println("added zone indexer")

    zoneRec.pageGeometries.foreach { p =>
      zindexer.addPage(p)
    }
    println("added zone pages")

    zoneRec.zones.foreach { z =>
      zindexer.addZone(z)
    }
    println("added zones")
    zoneRec.labels.foreach { zl =>
      zindexer.addLabels(zl)
    }
    println("added zone labels")

    zindexer
  }
}

object SpatialEnrichments {

  implicit class RicherZone(val zone: Zone) extends AnyVal {

    def area(): Double = {
      zone.bboxes.foldLeft(0d){ case (acc, a) =>
        a.bbox.area
      }
    }

  }

  implicit class RicherLTBounds(val bb: LTBounds) extends AnyVal {
    def area: Double = bb.width*bb.height
  }
}

trait SpatialJsonFormat {

  val ReadPageID: Reads[Int@@PageID]   = __.read[Int].map(i => Tag.of[PageID](i))
  val WritePageID: Writes[Int@@PageID] = Writes[Int@@PageID] { i => JsNumber(PageID.unwrap(i)) }
  implicit def FormatPageID            = Format(ReadPageID, WritePageID)

  val ReadZoneID: Reads[Int@@ZoneID]   = __.read[Int].map(i => Tag.of[ZoneID](i))
  val WriteZoneID: Writes[Int@@ZoneID] = Writes[Int@@ZoneID] { i => JsNumber(ZoneID.unwrap(i)) }
  implicit def FormatZoneID            = Format(ReadZoneID, WriteZoneID)

  val ReadRegionID: Reads[Int@@RegionID]   = __.read[Int].map(i => Tag.of[RegionID](i))
  val WriteRegionID: Writes[Int@@RegionID] = Writes[Int@@RegionID] { i => JsNumber(RegionID.unwrap(i)) }
  implicit def FormatRegionID            = Format(ReadRegionID, WriteRegionID)

  val ReadTokenID: Reads[Int@@TokenID]   = __.read[Int].map(i => Tag.of[TokenID](i))
  val WriteTokenID: Writes[Int@@TokenID] = Writes[Int@@TokenID] { i => JsNumber(TokenID.unwrap(i)) }
  implicit def FormatTokenID            = Format(ReadTokenID, WriteTokenID)

  val ReadCharID: Reads[Int@@CharID]   = __.read[Int].map(i => Tag.of[CharID](i))
  val WriteCharID: Writes[Int@@CharID] = Writes[Int@@CharID] { i => JsNumber(CharID.unwrap(i)) }
  implicit def FormatCharID            = Format(ReadCharID, WriteCharID)

  val ReadChar: Reads[Char]   = __.read[String].map(c => c(0))
  val WriteChar: Writes[Char] = Writes[Char] { c => JsString(c.toString()) }
  implicit def FormatChar     = Format(ReadChar, WriteChar)

  implicit def residentFormat = Json.format[LBBounds]
  implicit def LTBoundsFormat = Json.format[LTBounds]
  implicit def FormatTargetedBounds = Json.format[TargetedBounds]
  implicit def FormatBorders = Json.format[Borders]
  implicit def FormatLabel = Json.format[Label]
  implicit def FormatPageGeometry = Json.format[PageGeometry]
  implicit def FormatZone = Json.format[Zone]
  implicit def FormatZoneAndLabel = Json.format[ZoneAndLabel]
  implicit def FormatCharBox = Json.format[CharBox]
  implicit def FormatPageChars = Json.format[PageChars]
  implicit def FormatZoneRecords = Json.format[ZoneRecords]
  implicit def FormatLable = Json.format[Label]


}
