package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import geometry._
import watrmarks._

import PageComponentImplicits._
import utils.IdGenerator

import tracing.VisualTracer
import predsynth._
// import textreflow._
import textreflow.data._
import TypeTags._
import rindex._
import corpora._

import watrmarks.{StandardLabels => LB}

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

class MultiPageIndex(
  stableId: String@@DocumentID,
  storage: DocumentCorpus
) {
  lazy val docId: Int@@DocumentID =
    storage.getDocument(stableId).getOrElse(sys.error("MultiPageIndex created for non-existent document"))

  def createZone(): Zone  = {
    storage.getZone(
      storage.createZone(docId)
    )
  }

  def getZones(): Seq[Zone] = {
    storage
      .getZonesForDocument(docId)
      .map(storage.getZone(_))
  }

  def getZone(zoneId: Int@@ZoneID): Zone = {
    storage.getZone(zoneId)
  }

  def addZoneTargetRegions(zoneId: Int@@ZoneID, targetRegions: Seq[TargetRegion]): Zone = {
    storage.setZoneTargetRegions(zoneId, targetRegions)
    storage.getZone(zoneId)
  }

  def addZoneLabel(zoneId: Int@@ZoneID, label: Label): Zone = {
    storage.addZoneLabel(zoneId, label)
    storage.getZone(zoneId)
  }

  def getStableId(): String@@DocumentID = stableId

  val vtrace: VisualTracer = new VisualTracer()

  val pageIndexes = mutable.HashMap[Int@@PageNum, PageIndex]()

  def dbgFilterComponents(pg: Int@@PageNum, include: LTBounds): Unit ={
    pageIndexes.get(pg).foreach ({ pageIndex =>
      val keep = pageIndex.componentIndex.queryForIntersects(include).map(_.id)
      pageIndex.componentIndex.getItems
        .filterNot(c => keep.contains(c.id))
        .foreach(c => pageIndex.componentIndex.remove(c))
    })
  }
  def dbgFilterPages(pg: Int@@PageNum): Unit ={
    getPages
      .filterNot(_ == pg)
      .foreach ({ p =>
        pageIndexes.remove(p)
      })
  }

  // ID generators
  val componentIdGen = IdGenerator[ComponentID]()
  val labelIdGen = IdGenerator[LabelID]()
  // NB this is only for Components, and it a kludge until Component/DocumentCorpus systems are unified
  val regionIdGen = IdGenerator[RegionID]()


  // FIXME: remove this kludge
  //   map component to zone when there is a 1-to-1 correspondence and a TextReflow for the Zone involved
  val componentIdToZoneId: mutable.HashMap[Int@@ComponentID, Int@@ZoneID] = mutable.HashMap()

  def getZoneForComponent(cc: Int@@ComponentID): Option[Int@@ZoneID] = {
    componentIdToZoneId.get(cc)
  }

  def setTextReflowForComponent(cc: Component, r: TextReflow): Unit = {
    val zone = createZone()
    componentIdToZoneId.put(cc.id, zone.id)
    storage.setTextReflowForZone(zone.id, r)
  }

  def getTextReflowForComponent(ccId: Int@@ComponentID): Option[TextReflow] = {
    // TODO kludge: find a zone that has this Component as it's sole member and return its TextReflow
    for {
      zoneId <- componentIdToZoneId.get(ccId)
      reflow <- storage.getTextReflowForZone(zoneId)
    } yield {
      reflow
    }
  }

  def getTextReflow(zoneId: Int@@ZoneID): Option[TextReflow] = {
    storage.getTextReflowForZone(zoneId)
  }

  def getPageVisualLines(pageId: Int@@PageNum): Seq[Component]  = for {
    pageLineCC <- getPageIndex(pageId).getComponentsWithLabel(LB.PageLines)
    vline <- pageLineCC.getChildren(LB.VisualLine)
  } yield vline

  def getDocumentVisualLines(): Seq[Seq[Component]] = for {
    pageId <- getPages
  } yield getPageVisualLines(pageId)

  // TODO: merge with other text reflow access function
  def getVisualLineTextReflows(): Seq[TextReflow] = for {
    vline  <- getDocumentVisualLines().flatten
    reflow <- getTextReflowForComponent(vline.id)
  } yield  reflow

  // TODO: this should become the canonical way to get text reflows within a document
  def getTextReflows(labels: Label*): Seq[TextReflow]  = {
    for {
      pageId <- getPages
      pageTextBlocks <- getPageIndex(pageId).getComponentsWithLabel(LB.PageTextBlocks)
      textBlockCC <- pageTextBlocks.getChildren(LB.TextBlock)
      blockTextReflow <- getTextReflowForComponent(textBlockCC.id)
    } yield {
      blockTextReflow
    }
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

  def getPageForComponent(c: Component): Int@@PageNum = {
    c.targetRegion.pageNum
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

  def getPageIndex(pageId: Int@@PageNum) = pageIndexes(pageId)

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
      val targetPages = targetRegions.map(_.pageNum.unwrap)
      val numOfTargetPages =  targetPages.toSet.size

      if (numOfTargetPages != 1) {
        sys.error(s"""cannot label connected components from different pages (got pages=${targetPages.mkString(", ")})""")
      }

      val pageNum =  PageNum(targetPages.head)

      val pageId = storage.getPage(docId, pageNum).get
      val totalBounds = targetRegions.reduce(_ union _).bbox
      val regionId = storage.addTargetRegion(pageId, totalBounds)
      val totalRegion = storage.getTargetRegion(regionId)

      val region = createRegionComponent(totalRegion, role)

      println(s"labelRegion: ccRegion: ${region}")

      val zone = createZone()

      addZoneTargetRegions(zone.id, Seq(region.targetRegion))
      addZoneLabel(zone.id, role)

      Some(region)
    }
  }



  def createRegionComponent(tr: TargetRegion, role: Label): RegionComponent = {
    val region = RegionComponent(componentIdGen.nextId, role, tr, this)
    addComponent(region)

    region
  }

  def addPageAtom(pageAtom: PageAtom): AtomicComponent = {
    val c = AtomicComponent(componentIdGen.nextId, pageAtom, this)
    addComponent(c)
    c
  }

  def getComponent(id: Int@@ComponentID, pageId: Int@@PageNum): Component = {
    getPageIndex(pageId).componentIndex.getItem(id.unwrap)
  }

  def addComponent(c: Component): Component = {
    val pageNum = c.targetRegion.pageNum
    getPageIndex(pageNum)
      .addComponent(c)
  }

  def getPageGeometry(p: Int@@PageNum) = pageIndexes(p).pageGeometry

  def getPages(): List[Int@@PageNum] = {
    pageIndexes.keys.toList.sortBy(PageNum.unwrap(_))
  }


  def addPage(pageGeometry: PageGeometry): PageIndex = {
    storage.addPage(docId, pageGeometry.id)

    val pageIndex = PageIndex(
      SpatialIndex.createFor[Component](),
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

  import PageComponentImplicits._

  def initDocument(
    stableId: String@@DocumentID,
    regionsAndGeometry: Seq[(Seq[PageAtom], PageGeometry)],
    docStore: DocumentCorpus
  ): MultiPageIndex = {

    val mpageIndex = new MultiPageIndex(stableId, docStore)
    val docId = docStore.getDocument(stableId).getOrElse {
      sys.error(s"initDocument: No Document found for ${stableId}")
    }

    regionsAndGeometry.foreach { case(regions, geom)  =>
      println(s"adding page w/geometry ${geom}")
      val pageIndex = mpageIndex.addPage(geom)

      regions.foreach {
        case cb:CharAtom if !cb.isSpace => mpageIndex.addPageAtom(cb)
        case cb:ImgAtom =>
        case cb => println(s"error adding ${cb}")
      }
    }
    mpageIndex
  }

}
