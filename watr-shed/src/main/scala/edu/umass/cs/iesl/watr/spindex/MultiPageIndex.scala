package edu.umass.cs.iesl.watr
package spindex //;import acyclic.file

import scala.collection.mutable

import geometry._
import watrmarks._

import ComponentTypeEnrichments._
import utils.IdGenerator

import tracing.VisualTracer
import predsynth._
import textreflow._

/**

  MultiPageIndex manages:
    - PageIndexes, one per pdf page
    - TextReflow -> Component mapping (e.g., text contained in VisualLine, TextBlock) (TODO perhaps TextReflow should be Zone-associated)
    - Zones, each of which is a list of Components, potentially crossing PageIndexes
    - Relations and Props, e.g., {ClusterID -hasMember-> MentionID}, {id hasProp isTargetEntity}
      - nb. this is a kludge that will be removed at some point

    - BioLabeling, a BIOLU format list of TextBlock regions, for labeling Sections, Headers, and a few other things. Obsolete and on the chopping block

    - Interface to individual PageIndex functions

    - RegionComponent creation and indexing, per PageIndex
      - Creates a new bounding box around lists of other PageIndex components

    - Add/remove Pages/PageAtoms/ConnectedComponents


    TODO:
      - Handle PageIndexes across PDFs (or create another layered class over this one)
      - Caching, either within this class or attachable


  */
class MultiPageIndex(docId: String@@DocumentID) {

  def getDocumentID(): String@@DocumentID = docId

  val vtrace: VisualTracer = new VisualTracer()

  import SpatialIndex._

  val pageIndexes = mutable.HashMap[Int@@PageID, PageIndex]()


  def dbgFilterComponents(pg: Int@@PageID, include: LTBounds): Unit ={
    pageIndexes.get(pg).foreach ({ pageIndex =>
      val keep = pageIndex.componentIndex.queryForIntersects(include).map(_.id)
      // println(s"dbgFilterComponents(): keeping ${keep.length} components")
      pageIndex.componentIndex.getItems
        .filterNot(c => keep.contains(c.id))
        .foreach({ c =>
          pageIndex.componentIndex.remove(c)
        })
    })
  }
  def dbgFilterPages(pg: Int@@PageID): Unit ={
    getPages
      .filterNot(_ == pg)
      .foreach ({ p =>
        pageIndexes.remove(p)
      })
  }

  // ID generators
  val componentIdGen = IdGenerator[ComponentID]()
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

  // TODO: merge with other text reflow access function
  def getVisualLineTextReflows(labels: Label*): Seq[TextReflow]  = {
    for {
      pageId <- getPages
      pageTextBlocks <- getPageIndex(pageId).getComponentsWithLabel(LB.PageTextBlocks)
      textBlockCC <- pageTextBlocks.getChildren(LB.TextBlock)
      vline <- textBlockCC.getChildren(LB.VisualLine)
      vlineTextReflow <- getTextReflow(vline.id)
    } yield {
      vlineTextReflow
    }
  }

  // TODO: this should become the canonical way to get text reflows within a document
  def getTextReflows(labels: Label*): Seq[TextReflow]  = {
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
    val pageIndex = pageIndexes(getPageForComponent(c))
    pageIndex.setChildrenWithLabel(c.id, l, tree)
  }

  def getChildrenWithLabel(c: Component, l: Label): Option[Seq[Int@@ComponentID]] = {
    val pageIndex = pageIndexes(getPageForComponent(c))
    val sdf = pageIndex.componentIndex.get(0)
    pageIndex.getChildrenWithLabel(c.id, l)
  }

  def getChildren(c: Component, l: Label): Option[Seq[Component]] = {
    val pageIndex = pageIndexes(getPageForComponent(c))
    pageIndex.getChildrenWithLabel(c.id, l)
      .map(tree => tree.map{ cid =>
        pageIndex.componentIndex.get(cid.unwrap).getOrElse {
          sys.error(s"getChildren(${c}, ${l}) contained an invalid component id: ${cid}")
        }
      })
  }

  def getPageForComponent(c: Component): Int@@PageID = {
    c.targetRegions
      .headOption.map(_.pageId)
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

  def getPageIndex(pageId: Int@@PageID) = pageIndexes(pageId)

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
      val targetPages = targetRegions.map(_.pageId.unwrap)
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
    val pageId = c.targetRegion.pageId
    getPageIndex(pageId)
      .addComponent(c)
  }

  def getPageGeometry(p: Int@@PageID) = pageIndexes(p).pageGeometry

  def getPages(): List[Int@@PageID] = {
    pageIndexes.keys.toList.sortBy(PageID.unwrap(_))
  }


  def addPage(pageGeometry: PageGeometry): PageIndex = {
    val pageIndex = PageIndex(
      SpatialIndex.createFor[Component](pageGeometry.bounds),
      pageGeometry
    )
    val existing = pageIndexes.put(pageGeometry.id, pageIndex)

    existing.foreach { e =>
      sys.error("adding new page w/existing id")
    }
    pageIndex
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


object MultiPageIndex {

  def loadSpatialIndices(docId: String@@DocumentID, regionsAndGeometry: Seq[(Seq[PageAtom], PageGeometry)]): MultiPageIndex = {

    val zindexer = new MultiPageIndex(docId)
    regionsAndGeometry.foreach { case(regions, geom)  =>
      println(s"adding page w/geometry ${geom}")
      val pageIndex = zindexer.addPage(geom)

      regions.foreach {
        case cb:CharAtom if !cb.isSpace => zindexer.addPageAtom(cb)
        case cb:ImgAtom =>
      }
    }
    zindexer
  }

}
