package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import net.sf.jsi
import net.sf.jsi.rtree.RTree


import watrmarks._
import TypeTags._
import scalaz.@@
// import ComponentOperations._
// import IndexShapeOperations._
import ComponentTypeEnrichments._
import utils.IdGenerator



case class PageInfo(
  pageId: Int@@PageID,
  rCharIndex: SpatialIndex[CharAtom],
  rComponentIndex: jsi.SpatialIndex,
  geometry: PageGeometry,
  pageChars: mutable.ArrayBuffer[CharAtom],
  charBoxes: mutable.LongMap[CharAtom]
)


object BioLabeling {

  def isBegin(lb: Label, n: BioNode) = {
    n.pins.exists(p => p.label==lb && (p.isBegin || p.isUnit))
  }

  def hasID(lb: Label, id: Int, n: BioNode) = {
    n.pins.exists(p => p.label==lb && p.id == id)
  }


  def selectBioLabelings(l: Label, seq: Seq[BioNode]): Seq[Seq[BioNode]] = {

    def loop(ns: Seq[BioNode]): Seq[Seq[BioNode]] = {
      var currID: Int = 0
      val atBegin = ns
        .dropWhile({ node => !isBegin(l, node) })

      atBegin.headOption
        .map ({ node =>
          node.pins
            .filter(_.label==l)
            .foreach(p => currID = p.id.unwrap)

          val (yes, after) = atBegin
            .span(node => hasID(l, currID, node))


          yes +: loop(after)
        })
        .getOrElse({
          Seq.empty[Seq[BioNode]]
        })
    }

    loop(seq)
  }
}




class ZoneIndexer  {
  import SpatialIndex._

  val pageInfos = mutable.HashMap[Int@@PageID, PageInfo]()

  // Zones are on the way out
  val zoneMap = mutable.HashMap[Int@@ZoneID, Zone]()
  val zoneLabelMap = mutable.HashMap[Int@@ZoneID, mutable.ArrayBuffer[Label]]()

  type BioSpine = mutable.MutableList[BioNode]
  val bioSpines = mutable.Map[String, BioSpine]()

  def bioSpine(name: String): BioSpine = {
    bioSpines.getOrElseUpdate(name, mutable.MutableList[BioNode]())
  }


  val componentMap = mutable.HashMap[Int@@ComponentID, Component]()
  val componentLabels = mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]]()

  val componentIdGen = utils.IdGenerator[ComponentID]()
  val labelIdGen = IdGenerator[LabelID]()

  def empty(): Component = {
    concatComponents(Seq())
  }


  val regionToZone = mutable.HashMap[Int@@RegionID, Zone]()


  def concatComponents(components: Seq[Component], l: Label*): Component = {
    val c = ConnectedComponent(componentIdGen.nextId, components, this)
    l.foreach(c.addLabel)
    componentMap.put(c.id, c)
    c
  }

  def toComponent(region: PageAtom): Component = {
    val c = PageComponent(componentIdGen.nextId, region, this)
    componentMap.put(c.id, c)
    c
  }

  def concatRegions(regions: Seq[PageAtom], l: Option[Label]=None): Component = {
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

  def getLabeledComponents(l: Label): Seq[Component] = for {
    (k, v)  <- componentLabels.toSeq
    if v.contains(l)
  } yield componentMap(k)

  def removeLabel(c: Component, l: Label): Component = {
    getComponentLabelBuffer(c) -= l
    c
  }


  def getPageGeometry(p: Int@@PageID) = pageInfos(p).geometry

  def getAtom(pageId: Int@@PageID, charId: Int@@RegionID): CharAtom = {
    pageInfos(pageId).charBoxes(charId.unwrap.toLong)
  }

  // See getAtomsUnfiltered() for explanation behind filterNot
  def getAtoms(pageId: Int@@PageID): Seq[CharAtom] = {
    pageInfos(pageId).pageChars.filterNot(_.isSpace)
  }


  // Some chars from a pdf are unuseable, either unprintable or embedded space characters.
  //   This will return all of them
  def getAtomsUnfiltered(pageId: Int@@PageID): Seq[CharAtom] = {
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


  def createSpatialIndex(): jsi.SpatialIndex = {
    val rtree: jsi.SpatialIndex = new RTree()
    rtree.init(null)
    rtree
  }

  def addPage(pageGeometry: PageGeometry): Unit = {
    if(pageInfos.contains(pageGeometry.id)) {
      sys.error("adding new page w/existing id")
    }

    pageInfos.put(pageGeometry.id,
      PageInfo(pageGeometry.id,
        SpatialIndex.createFor[CharAtom](pageGeometry.bounds),
        createSpatialIndex(),
        pageGeometry,
        mutable.ArrayBuffer(),
        mutable.LongMap())
    )
  }

  def addCharInfo(pageId: Int@@PageID, cb: CharAtom): Unit = {
    pageInfos(pageId)
      .rCharIndex
      .add(cb)
  }

  // def addZone(zone: Zone): Unit = {
  //   val zoneId = zone.id
  //   zoneMap.put(zoneId, zone).map(existing => sys.error(s"zone already exists in zoneMap"))
  //   zone.regions.foreach{ targetedBounds  =>
  //     regionToZone.put(targetedBounds.id, zone)
  //     pageInfos(targetedBounds.target).rCharIndex.add(
  //       targetedBounds.bbox.toJsiRectangle,
  //       RegionID.unwrap(targetedBounds.id)
  //     )
  //   }
  // }


  def putCharAtom(pageId: Int@@PageID, cb: CharAtom): Unit = {
    pageInfos(pageId).charBoxes.put(cb.region.id.unwrap.toLong, cb)
  }

  def getCharAtom(pageId: Int@@PageID, id: Int): CharAtom = {
    pageInfos(pageId).
    charBoxes(id.toLong)
  }

  def getCharAtom(pageId: Int@@PageID, id: Int@@RegionID): CharAtom = {
    pageInfos(pageId).
    charBoxes(id.unwrap.toLong)
  }

  def getCharAtom(pageId: Int@@PageID, id: Long): CharAtom = {
    pageInfos(pageId).charBoxes(id)
  }

  // def queryForIntersected(pageId: Int@@PageID, q: LTBounds): Seq[CharAtom] = {
  //   queryForIntersectedIDs(pageInfos(pageId).rCharIndex, q)
  //     .filter{ id => pageInfos(pageId).charBoxes.contains(id.toLong) }
  //     .map(cid => pageInfos(pageId).charBoxes(cid.toLong))
  // }

  // def queryCharsIntersects(pageId: Int@@PageID, q: LTBounds): Seq[CharAtom] = {
  //   queryForIntersectedIDs(pageInfos(pageId).rCharIndex, q)
  //       .filter{ id => pageInfos(pageId).charBoxes.contains(id.toLong) }
  //       .map(cid => pageInfos(pageId).charBoxes(cid.toLong))
  // }


  // def queryChars(pageId: Int@@PageID, q: LTBounds): Seq[CharAtom] = {
  //   queryForContainedIDs(pageInfos(pageId).rCharIndex, q)
  //     .filter(id => pageInfos(pageId).charBoxes.contains(id.toLong))
  //     .map( getCharAtom(pageId, _) )
  // }

  // def queryComponents(pageId: Int@@PageID, q: LTBounds): Seq[Component] = {
  //   queryForContainedIDs(pageInfos(pageId).rComponentIndex, q)
  //     .map(getCharAtom(pageId, _) )

  //   ???
  // }

  // def query(pageId: Int@@PageID, q: LTBounds): Seq[Zone] = {
  //   println(s"ZoneIndex.query(${pageId}, ${q.prettyPrint})")
  //   val rCharIndex = pageInfos(pageId).rCharIndex

  //   val collectRegions = ZoneIndexer.rtreeIdCollector()
  //   rCharIndex.contains(q.toJsiRectangle, collectRegions)
  //   // rCharIndex.intersects(q.toJsiRectangle, collectRegions)
  //   val regions = collectRegions.getIDs
  //   val zones = collectRegions
  //     .getIDs.map{ regionId => regionToZone(RegionID(regionId)) }

  //   zones.sortBy { z => z.regions.head.bbox.left }
  // }

  def addBioLabels(label: Label, node: BioNode): Unit = {
    addBioLabels(label, Seq(node))
  }

  def addBioLabels(label: Label, nodes: Seq[BioNode]): Unit = {
    val labelId = labelIdGen.nextId

    if (nodes.length==1) {
      nodes.foreach(_.pins += label.U(labelId))
    } else if (nodes.length > 1) {
      nodes.head.pins += label.B(labelId)
      nodes.last.pins += label.L(labelId)

      nodes.drop(1).dropRight(1).foreach(
        _.pins += label.I(labelId)
      )
    }
  }

}


object ZoneIndexer extends ComponentDataTypeFormats {
  def minMaxPairToPointWidth(minMax: (Double, Double)): (Double, Double) = {
    (minMax._1, minMax._2-minMax._1)
  }

  def loadSpatialIndices(regionsAndGeometry: Seq[(PageAtoms, PageGeometry)]): ZoneIndexer = {
    loadSpatialIndices2(regionsAndGeometry.map(c => (c._1.regions, c._2)))
  }

  def loadSpatialIndices2(regionsAndGeometry: Seq[(Seq[PageAtom], PageGeometry)]): ZoneIndexer = {

    val zindexer = new ZoneIndexer()
    regionsAndGeometry.foreach { case(regions, geom)  =>
      zindexer.addPage(geom)

      regions.foreach { cb =>
        if (!cb.isSpace && cb.isChar) {
          zindexer.addCharInfo(geom.id, cb.asInstanceOf[CharAtom])
        }
      }
    }
    zindexer
  }

  def loadSpatialIndices(zoneRec: ZoneRecords): ZoneIndexer = {

    // val zindexer = new ZoneIndexer()

    // println("added zone indexer")

    // zoneRec.pageGeometries.foreach { p =>
    //   zindexer.addPage(p)
    // }
    // println("added zone pages")

    // zoneRec.zones.foreach { z =>
    //   zindexer.addZone(z)
    // }
    // println("added zones")
    // zoneRec.labels.foreach { zl =>
    //   zindexer.addLabels(zl)
    // }
    // println("added zone labels")

    // zindexer
    ???
  }


}
