package edu.umass.cs.iesl.watr
package spindex

import java.net.URI
import scala.collection.mutable

import geometry._
import GeometricFigure._
import EnrichGeometricFigures._

import watrmarks._
import TypeTags._
import scalaz.@@
import ComponentTypeEnrichments._
import utils.IdGenerator

import tracing.VisualTracer
import predsynth._
import textreflow._

class ZoneIndexer(
  srcUri: URI
)  {

  def getSrcUri():URI = srcUri

  val vtrace: VisualTracer = new VisualTracer()

  import SpatialIndex._

  val pageInfos = mutable.HashMap[Int@@PageID, PageIndex]()


  def dbgFilterComponents(pg: Int@@PageID, include: GeometricFigure.LTBounds): Unit ={
    pageInfos.get(pg).foreach ({ pageInfo =>
      val keep = pageInfo.componentIndex.queryForIntersects(include).map(_.id)
      // println(s"dbgFilterComponents(): keeping ${keep.length} components")
      pageInfo.componentIndex.getItems
        .filterNot(c => keep.contains(c.id))
        .foreach({ c =>
          pageInfo.componentIndex.remove(c)
        })
    })
  }
  def dbgFilterPages(pg: Int@@PageID): Unit ={
    getPages
      .filterNot(_ == pg)
      .foreach ({ p =>
        pageInfos.remove(p)
      })
  }

  // ID generators
  val componentIdGen = utils.IdGenerator[ComponentID]()
  val labelIdGen = IdGenerator[LabelID]()
  val regionIdGen = IdGenerator[RegionID]()

  // Zone-related
  val zoneIdGen = IdGenerator[ZoneID]()
  val labelToZones: mutable.HashMap[Label, mutable.ArrayBuffer[Int@@ZoneID]] = mutable.HashMap()
  val zoneMap = mutable.HashMap[Int@@ZoneID, Zone]()

  val componentToTextReflow: mutable.HashMap[Int@@ComponentID, TextReflow] = mutable.HashMap()

  def setTextReflow(cc: Component, r: TextReflow): Unit = {
    componentToTextReflow.put(cc.id, r)
  }

  def getTextReflow(cc: Int@@ComponentID): Option[TextReflow] = {
    componentToTextReflow.get(cc)
  }

  // TODO: this should become the canonical way to get text reflows within a document
  def getTextReflows(labels: Label*): Seq[TextReflow]  = {
    import watrmarks.{StandardLabels => LB}

    for {
      pageId <- getPages
      pageTextBlocks <- getPageIndex(pageId).getComponentsWithLabel(LB.PageTextBlocks)
      textBlockCC <- pageTextBlocks.getChildren(LB.TextBlock)
      blockTextReflow <- getTextReflow(textBlockCC.id)
    } yield {
      blockTextReflow
    }
  }


  def getZones(): Seq[Zone] = {
    zoneMap.values.toSeq
  }

  private def getLabelToZoneBuffer(l: Label): mutable.ArrayBuffer[Int@@ZoneID] = {
    labelToZones.getOrElseUpdate(l, mutable.ArrayBuffer[Int@@ZoneID]())
  }

  def addZone(z: Zone): Zone =  {
    val zid = zoneIdGen.nextId
    val zupdate = z.copy(id=zid)
    zoneMap.put(zid, zupdate)
    getLabelToZoneBuffer(z.label) += zid
    zupdate
  }
  val relations = mutable.ArrayBuffer[Relation.Record]()
  val props = mutable.ArrayBuffer[Prop.PropRec]()

  def addRelations(rs: Seq[Relation.Record]): Unit = {
    relations ++= rs
  }
  def addProps(rs: Seq[Prop.PropRec]): Unit = {
    props ++= rs
  }


  type BioLabeling = mutable.MutableList[BioNode]
  val bioLabelings = mutable.Map[String, BioLabeling]()

  def bioLabeling(name: String): BioLabeling = {
    bioLabelings.getOrElseUpdate(name, mutable.MutableList[BioNode]())
  }


  def setChildrenWithLabel(c: Component, l: Label, tree: Seq[Int@@ComponentID]):Unit = {
    val pageInfo = pageInfos(getPageForComponent(c))
    pageInfo.setChildrenWithLabel(c.id, l, tree)
  }

  def getChildrenWithLabel(c: Component, l: Label): Option[Seq[Int@@ComponentID]] = {
    val pageInfo = pageInfos(getPageForComponent(c))
    val sdf = pageInfo.componentIndex.get(0)
    pageInfo.getChildrenWithLabel(c.id, l)
  }

  def getChildren(c: Component, l: Label): Option[Seq[Component]] = {
    val pageInfo = pageInfos(getPageForComponent(c))
    pageInfo.getChildrenWithLabel(c.id, l)
      .map(tree => tree.map{ cid =>
        pageInfo.componentIndex.get(cid.unwrap).getOrElse {
          sys.error(s"getChildren(${c}, ${l}) contained an invalid component id: ${cid}")
        }
      })
  }

  def getPageForComponent(c: Component): Int@@PageID = {
    c.targetRegions
      .headOption.map(_.target)
      .getOrElse(sys.error("no page specified for component"))
  }



  def addLabel(c: Component, l: Label): Component = {
    val pageId = getPageForComponent(c)
    val pinfo = getPageIndex(pageId)
    pinfo.addLabel(c, l)
  }

  def removeLabel(c: Component, l: Label): Component = {
    val pageId = getPageForComponent(c)
    val pinfo = getPageIndex(pageId)
    pinfo.removeLabel(c, l)
  }


  def getLabels(c: Component): Set[Label] = {
    val pageId = getPageForComponent(c)
    val pinfo = getPageIndex(pageId)
    pinfo.getLabels(c)
  }

  def getPageIndex(pageId: Int@@PageID) = pageInfos(pageId)

  def removeComponent(c: Component): Unit = {
    // vtrace.trace("removeComponent" withTrace showComponent(c))
    val pinfo = getPageIndex(getPageForComponent(c))
    pinfo.componentToLabels -= c.id
    pinfo.componentIndex.remove(c)
    // FIXME also delete label maps, etc.
  }

  def labelRegion(components: Seq[Component], role: Label): Option[RegionComponent] = {
    if (components.isEmpty) None else {
      val targetRegions = components.map(_.targetRegion)
      val targetPages = targetRegions.map(_.target.unwrap)
      val numOfTargetPages =  targetPages.toSet.size

      if (numOfTargetPages != 1) {
        sys.error(s"""cannot label connected components from different pages (got pages=${targetPages.mkString(", ")})""")
      }

      val totalRegion = targetRegions.reduce(_ union _)

      val region = createRegionComponent(totalRegion, role)

      // vtrace.trace(s"Label Region as ${role}" withTrace showComponent(region))

      Some(region)
    }
  }



  def createRegionComponent(tr: TargetRegion, role: Label): RegionComponent = {
    val region = RegionComponent(componentIdGen.nextId, role, tr, this)
    addComponent(region)

    // vtrace.trace("create RegionComponent" withTrace showComponent(region))
    region
  }

  def addPageAtom(pageAtom: PageAtom): AtomicComponent = {
    val c = AtomicComponent(componentIdGen.nextId, pageAtom, this)
    addComponent(c)
    c
  }

  def getComponent(id: Int@@ComponentID, pageId: Int@@PageID): Component = {
    getPageIndex(pageId).componentIndex.getItem(id.unwrap)
  }

  def addComponent(c: Component): Component = {
    val pageId = c.targetRegion.target
    getPageIndex(pageId)
      .addComponent(c)
  }

  def getPageGeometry(p: Int@@PageID) = pageInfos(p).geometry

  def getPages(): List[Int@@PageID] = {
    pageInfos.keys.toList.sortBy(PageID.unwrap(_))
  }



  def addPage(pageGeometry: PageGeometry): PageIndex = {
    val pageInfo = PageIndex(pageGeometry.id,
      SpatialIndex.createFor[Component](pageGeometry.bounds),
      pageGeometry
    )
    val existing = pageInfos.put(pageGeometry.id, pageInfo)

    existing.foreach { e =>
      sys.error("adding new page w/existing id")
    }
    pageInfo
  }


  def addBioLabels(label: Label, node: BioNode): Unit = {
    addBioLabels(label, Seq(node))
  }

  def addBioLabels(label: Label, nodes: Seq[BioNode]): Unit = {
    val labelId = labelIdGen.nextId
    val l = label.copy(id=labelId)

    if (nodes.length==1) {
      nodes.foreach(_.pins += l.U)
    } else if (nodes.length > 1) {
      nodes.head.pins += l.B
      nodes.last.pins += l.L

      nodes.drop(1).dropRight(1).foreach(
        _.pins += l.I
      )
    }
  }

}


object ZoneIndexer extends ComponentDataTypeFormats {

  def loadSpatialIndices(srcUri: URI, regionsAndGeometry: Seq[(Seq[PageAtom], PageGeometry)]): ZoneIndexer = {

    val zindexer = new ZoneIndexer(srcUri)
    regionsAndGeometry.foreach { case(regions, geom)  =>
      val pageInfo = zindexer.addPage(geom)

      regions.foreach {
        case cb:CharAtom if !cb.isSpace => zindexer.addPageAtom(cb)
        case cb:ImgAtom =>
      }
    }
    zindexer
  }



}
