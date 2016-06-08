package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable
import scala.collection.mutable

import net.sf.jsi
import net.sf.jsi.rtree.RTree


import watrmarks._
import TypeTags._
import scalaz.@@
// import ComponentOperations._
import IndexShapeOperations._
import ComponentTypeEnrichments._

case class PageInfo(
  pageId: Int@@PageID,
  rindex: jsi.SpatialIndex,
  geometry: PageGeometry,
  pageChars: mutable.ArrayBuffer[CharRegion],
  charBoxes: mutable.LongMap[CharRegion]
)


class ZoneIndexer  {
  val pageInfos = mutable.HashMap[Int@@PageID, PageInfo]()

  val zoneMap = mutable.HashMap[Int@@ZoneID, Zone]()
  val zoneLabelMap = mutable.HashMap[Int@@ZoneID, mutable.ArrayBuffer[Label]]()

  val componentMap = mutable.HashMap[Int@@ComponentID, Component]()
  val componentLabels = mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]]()

  val componentIdGen = utils.IdGenerator[ComponentID]()

  val regionToZone = mutable.HashMap[Int@@RegionID, Zone]()


  def concatComponents(components: Seq[Component]): Component = {
    val c = ConnectedComponents(
      componentIdGen.nextId, components, this
    )
    componentMap.put(c.id, c)
    c
  }

  def toComponent(region: PageRegion): Component = {
    val c = PageComponent(componentIdGen.nextId, region, this)
    componentMap.put(c.id, c)
    c
  }

  def concatRegions(regions: Seq[PageRegion]): Component = {
    concatComponents(regions.map(toComponent(_)))
  }

  def appendComponent(component: Component, app: Component): Component = {
    component match {
      case c: PageComponent =>
        concatComponents(Seq(component, app))
      case c: ConnectedComponents =>
        c.copy(components = c.components :+ app)
    }
  }
  private def getComponentLabelBuffer(c: Component): mutable.ArrayBuffer[Label] = {
    componentLabels.getOrElseUpdate(c.id, mutable.ArrayBuffer[Label]())
  }
  def addLabel(c: Component, l: Label): Component = {
    getComponentLabelBuffer(c).append(l)
    c
  }

  def getLabels(c: Component): Set[Label] = {
    getComponentLabelBuffer(c).toSet
  }

  def removeLabel(c: Component, l: Label): Component = {
    getComponentLabelBuffer(c) -= l
    c
  }


  def getPageGeometry(p: Int@@PageID) = pageInfos(p).geometry

  def getComponent(pageId: Int@@PageID, charId: Int@@RegionID): CharRegion = {
    pageInfos(pageId).charBoxes(charId.unwrap.toLong)
  }

  // See getComponentsUnfiltered() for explanation behind filterNot
  def getComponents(pageId: Int@@PageID): Seq[CharRegion] = {
    pageInfos(pageId).pageChars.filterNot(_.isSpace)
  }


  // Some chars from a pdf are unuseable, either unprintable or embedded space characters.
  //   This will return all of them
  def getComponentsUnfiltered(pageId: Int@@PageID): Seq[CharRegion] = {
    pageInfos(pageId).pageChars.filterNot(_.isSpace)
  }

  def addLabels(zl: ZoneAndLabel): Unit = {
    val lls = zoneLabelMap.getOrElseUpdate(zl.zoneId, mutable.ArrayBuffer[Label]())
    lls.append(zl.label)
  }

  def getZoneLabels(id: Int@@ZoneID): Seq[Label] = {
    zoneLabelMap.get(id).getOrElse(Seq())
  }

  def getPages(): List[Int@@PageID] = {
    pageInfos.keys.toList.sortBy(PageID.unwrap(_))
  }


  def addPage(p: PageGeometry): Unit = {

    if(pageInfos.contains(p.id)) {
      sys.error("adding new page w/existing id")
    }
    val rtree: jsi.SpatialIndex = new RTree()
    rtree.init(null)

    pageInfos.put(p.id,
      PageInfo(p.id, rtree, p, mutable.ArrayBuffer(), mutable.LongMap())
    )
  }

  def addCharInfo(pageId: Int@@PageID, cb: CharRegion): Unit = {
    val rindex = pageInfos(pageId).rindex
    rindex.add(cb.region.bbox.toJsiRectangle, cb.region.id.unwrap.toInt)
    pageInfos(pageId).pageChars.append(cb)
    pageInfos(pageId).charBoxes.put(cb.region.id.unwrap.toLong, cb)
  }

  def addZone(zone: Zone): Unit = {
    val zoneId = zone.id
    zoneMap.put(zoneId, zone).map(existing => sys.error(s"zone already exists in zoneMap"))
    zone.regions.foreach{ targetedBounds  =>
      regionToZone.put(targetedBounds.id, zone)
      pageInfos(targetedBounds.target).rindex.add(
        targetedBounds.bbox.toJsiRectangle,
        RegionID.unwrap(targetedBounds.id)
      )
    }
  }


  def putCharRegion(pageId: Int@@PageID, cb: CharRegion): Unit = {
    pageInfos(pageId).charBoxes.put(cb.region.id.unwrap.toLong, cb)
  }

  def getCharRegion(pageId: Int@@PageID, id: Int): CharRegion = {
    pageInfos(pageId).
    charBoxes(id.toLong)
  }

  def getCharRegion(pageId: Int@@PageID, id: Int@@RegionID): CharRegion = {
    pageInfos(pageId).
    charBoxes(id.unwrap.toLong)
  }

  def getCharRegion(pageId: Int@@PageID, id: Long): CharRegion = {
    pageInfos(pageId).charBoxes(id)
  }

  def queryCharsIntersects(pageId: Int@@PageID, q: LTBounds): Seq[CharRegion] = {
    val rindex = pageInfos(pageId).rindex

    val collectRegions = ZoneIndexer.rtreeIdCollector()
    // rindex.intersects(x$1: Rectangle, x$2: TIntProcedure)
    rindex.intersects(q.toJsiRectangle, collectRegions)
    val neighbors = collectRegions.getIDs.filter{ id =>
      pageInfos(pageId).charBoxes.contains(id.toLong)
    }
    neighbors.map(cid =>
      pageInfos(pageId).charBoxes(cid.toLong))
  }

  def queryChars(pageId: Int@@PageID, q: LTBounds): Seq[CharRegion] = {
    val rindex = pageInfos(pageId).rindex

    val collectRegions = ZoneIndexer.rtreeIdCollector()
    rindex.contains(q.toJsiRectangle, collectRegions)
    val neighbors = collectRegions.getIDs.filter{ id =>
      pageInfos(pageId).charBoxes.contains(id.toLong)
    }

    neighbors map (getCharRegion(pageId, _))
  }

  def nearestNChars(pageId: Int@@PageID, fromChar: CharRegion, n: Int, radius: Float): Seq[CharRegion] = {
    val rindex = pageInfos(pageId).rindex
    val ctr = fromChar.region.bbox.toCenterPoint
    val searchRect = LTBounds(
      left   =ctr.x - radius,
      top    =ctr.y - radius,
      width  =(radius*2.0).toDouble,
      height =(radius*2.0).toDouble
    )

    val collectRegions = ZoneIndexer.rtreeIdCollector()
    // println(s" searching ${n} nearest from c=${fromChar.char} bbox ${searchRect.prettyPrint}, ctr=${ctr}, radius: ${radius}")

    rindex.intersects(searchRect.toJsiRectangle, collectRegions)
    // println(s""" found ${collectRegions.getIDs.mkString(",")} """)
    collectRegions.getIDs
      .map({ id =>
        val cbox = getCharRegion(pageId, id)
        (cbox.region.bbox.toCenterPoint.dist(ctr), cbox)
      })
      .filterNot(_._2.region.id == fromChar.region.id)
      .sortBy(_._1)
      .take(n)
      .map(_._2)
  }

  def query(pageId: Int@@PageID, q: LTBounds): Seq[Zone] = {
    println(s"ZoneIndex.query(${pageId}, ${q.prettyPrint})")
    val rindex = pageInfos(pageId).rindex

    val collectRegions = ZoneIndexer.rtreeIdCollector()
    rindex.contains(q.toJsiRectangle, collectRegions)
    // rindex.intersects(q.toJsiRectangle, collectRegions)
    val regions = collectRegions.getIDs
    val zones = collectRegions
      .getIDs.map{ regionId => regionToZone(RegionID(regionId)) }

    zones.sortBy { z => z.regions.head.bbox.left }
  }


}


object ZoneIndexer extends ComponentDataTypeFormats {
  def minMaxPairToPointWidth(minMax: (Double, Double)): (Double, Double) = {
    (minMax._1, minMax._2-minMax._1)
  }

  def loadSpatialIndices(regionsAndGeometry: Seq[(PageRegions, PageGeometry)]): ZoneIndexer = {
    loadSpatialIndices2(regionsAndGeometry.map(c => (c._1.regions, c._2)))
  }

  def loadSpatialIndices2(regionsAndGeometry: Seq[(Seq[PageRegion], PageGeometry)]): ZoneIndexer = {

    val zindexer = new ZoneIndexer()
    regionsAndGeometry.foreach { case(regions, geom)  =>
      zindexer.addPage(geom)

      regions.foreach { cb =>
        if (!cb.isSpace && cb.isChar) {
          zindexer.addCharInfo(geom.id, cb.asInstanceOf[CharRegion])
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

  import gnu.trove.procedure.TIntProcedure
  class CollectRegionIds extends TIntProcedure {
    import scala.collection.mutable
    val ids = mutable.ArrayBuffer[Int]()

    override def execute(id: Int): Boolean = {
      ids.append(id)
      true
    }

    def getIDs: Seq[Int] = ids
  }

  def rtreeIdCollector(): CollectRegionIds = {
    new CollectRegionIds()
  }

}
