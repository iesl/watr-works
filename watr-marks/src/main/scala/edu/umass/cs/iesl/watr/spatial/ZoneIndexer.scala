package edu.umass.cs.iesl.watr
package spatial

import scala.collection.mutable
import scala.collection.mutable

import net.sf.jsi.SpatialIndex
import net.sf.jsi.rtree.RTree


import watrmarks._
import TypeTags._
import Bounds._

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

  // See getComponentsUnfiltered() for explanation behind filterNot
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



object ZoneIndexer extends ComponentDataTypeFormats {
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
