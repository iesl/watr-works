package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import watrmarks._
import TypeTags._
import scalaz.@@
import ComponentTypeEnrichments._
import utils.IdGenerator



case class PageInfo(
  pageId: Int@@PageID,
  charAtomIndex: SpatialIndex[CharAtom],
  componentIndex: SpatialIndex[Component],
  geometry: PageGeometry,
  componentToLabels: mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]] = mutable.HashMap(),
  labelToComponents: mutable.HashMap[Label, mutable.ArrayBuffer[Int@@ComponentID]] = mutable.HashMap()
) {


  def getComponentLabels(cid: Int@@ComponentID): Seq[Label] = {
    componentToLabels.get(cid)
      .getOrElse(Seq())
  }

  def getComponentsWithLabel(l: Label): Seq[Component] = {
    labelToComponents.get(l)
      .map(_.map(id => componentIndex.getItem(id.unwrap)))
      .getOrElse(Seq())
  }

  private def getComponentLabelBuffer(c: Component): mutable.ArrayBuffer[Label] = {
    componentToLabels.getOrElseUpdate(c.id, mutable.ArrayBuffer[Label]())
  }

  private def getLabelToComponentBuffer(l: Label): mutable.ArrayBuffer[Int@@ComponentID] = {
    labelToComponents.getOrElseUpdate(l, mutable.ArrayBuffer[Int@@ComponentID]())
  }

  def addLabel(c: Component, l: Label): Component = {
    getComponentLabelBuffer(c) += l
    getLabelToComponentBuffer(l) += c.id
    c
  }

  def removeLabel(c: Component, l: Label): Component = {
    getComponentLabelBuffer(c) -= l
    getLabelToComponentBuffer(l) -= c.id
    c
  }

  def getLabels(c: Component): Set[Label] = {
    getComponentLabelBuffer(c).toSet
  }




  // def getLabeledComponents(l: Label): Seq[Component] = (for {
  //   (cid, labels)  <- componentToLabels.toSeq
  //   if labels.contains(l)
  // } yield {
  //   pageInfos
  //     .flatMap({ case (_, info) =>
  //       info.componentIndex.get(cid.unwrap)
  //     }).toSeq
  // }).flatten



}


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
  // val zoneMap = mutable.HashMap[Int@@ZoneID, Zone]()
  // val zoneLabelMap = mutable.HashMap[Int@@ZoneID, mutable.ArrayBuffer[Label]]()

  type BioSpine = mutable.MutableList[BioNode]
  val bioSpines = mutable.Map[String, BioSpine]()

  def bioSpine(name: String): BioSpine = {
    bioSpines.getOrElseUpdate(name, mutable.MutableList[BioNode]())
  }


  // val componentMap = mutable.HashMap[Int@@ComponentID, Component]()
  // val componentLabels = mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]]()

  val componentIdGen = utils.IdGenerator[ComponentID]()
  val labelIdGen = IdGenerator[LabelID]()

  def getPageForComponent(c: Component): Int@@PageID = {
    c.targetRegions
      .headOption.map(_.target)
      .getOrElse(sys.error("no page specified for component"))
  }


  def addLabel(c: Component, l: Label): Component = {
    val pageId = getPageForComponent(c)
    val pinfo = getPageInfo(pageId)
    pinfo.addLabel(c, l)
  }

  def removeLabel(c: Component, l: Label): Component = {
    val pageId = getPageForComponent(c)
    val pinfo = getPageInfo(pageId)
    pinfo.removeLabel(c, l)
  }


  def getLabels(c: Component): Set[Label] = {
    val pageId = getPageForComponent(c)
    val pinfo = getPageInfo(pageId)
    pinfo.getLabels(c)
  }

  def empty(): Component = {
    concatComponents(Seq())
  }


  val regionToZone = mutable.HashMap[Int@@RegionID, Zone]()


  def getPageInfo(pageId: Int@@PageID) = pageInfos(pageId)

  def concatComponents(components: Seq[Component], l: Label*): Component = {
    val targetPages = components.flatMap(_.atoms).map(_.region.target.unwrap)
    val numOfTargetPages =  targetPages.toSet.size

    if (numOfTargetPages != 1) {
      sys.error(s"""cannot concatComponents() from different pages (got pages=${targetPages.mkString(", ")})""")
    }

    val targetPage = targetPages.head

    val c = ConnectedComponent(componentIdGen.nextId, components, this)
    val pinfo = getPageInfo(getPageForComponent(c))

    pinfo.componentIndex.add(c)

    l.foreach(c.addLabel)

    c
  }

  def toComponent(pageAtom: PageAtom): Component = {
    val c = PageComponent(componentIdGen.nextId, pageAtom, this)
    val pageId = pageAtom.region.target
    getPageInfo(pageId).componentIndex.add(c)

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
        c.components += app
        c
    }
  }

  def getPageGeometry(p: Int@@PageID) = pageInfos(p).geometry


  def getPages(): List[Int@@PageID] = {
    pageInfos.keys.toList.sortBy(PageID.unwrap(_))
  }



  def addPage(pageGeometry: PageGeometry): PageInfo = {
    val pageInfo = PageInfo(pageGeometry.id,
      SpatialIndex.createFor[CharAtom](pageGeometry.bounds),
      SpatialIndex.createFor[Component](pageGeometry.bounds),
      pageGeometry
    )
    val existing = pageInfos.put(pageGeometry.id, pageInfo)

    existing.foreach { e =>
      sys.error("adding new page w/existing id")
    }
    pageInfo
  }


  // def addZone(zone: Zone): Unit = {
  //   val zoneId = zone.id
  //   zoneMap.put(zoneId, zone).map(existing => sys.error(s"zone already exists in zoneMap"))
  //   zone.regions.foreach{ targetedBounds  =>
  //     regionToZone.put(targetedBounds.id, zone)
  //     pageInfos(targetedBounds.target).charAtomIndex.add(
  //       targetedBounds.bbox.toJsiRectangle,
  //       RegionID.unwrap(targetedBounds.id)
  //     )
  //   }
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
      val pageInfo = zindexer.addPage(geom)
      val charAtomIndex =  pageInfo.charAtomIndex
      // zindexer.getPageInfo(pageId: <refinement>[Int, PageID])

      regions.foreach {
        case cb:CharAtom if !cb.isSpace =>
          charAtomIndex.add(cb)

        case cb:ImgAtom =>
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
