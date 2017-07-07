package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import geometry._
import watrmarks._

// import PageComponentImplicits._
import utils.IdGenerator
import geometry.syntax._

import tracing.VisualTracer
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
  val docStore: DocumentZoningApi
) {
  lazy val docId: Int@@DocumentID =
    docStore.getDocument(stableId).getOrElse(sys.error("MultiPageIndex created for non-existent document"))


  def getStableId(): String@@DocumentID = stableId

  val vtrace: VisualTracer = new VisualTracer()

  val pageIndexes = mutable.HashMap[Int@@PageNum, PageIndex]()

  def dbgFilterComponents(pg: Int@@PageNum, include: LTBounds): Unit ={
    pageIndexes.get(pg).foreach ({ pageIndex =>
      val keep = pageIndex.componentIndex.queryForIntersects(include).map(_.id.unwrap)
      pageIndex.componentIndex.getItems
        .filterNot(c => keep.contains(c.id.unwrap))
        .foreach(c => pageIndex.componentIndex.remove(c))
    })
  }
  def dbgFilterPages(pg: Int@@PageNum): Unit ={
    println(s"dbgFilterPages: $pg")
    getPages
      .filterNot(_.unwrap == pg.unwrap)
      .foreach ({ p =>
        println(s"removing page num $p")
        pageIndexes.remove(p)
      })
  }

  // ID generators
  val componentIdGen = IdGenerator[ComponentID]()
  val labelIdGen = IdGenerator[LabelID]()
  // NB this is only for Components, and it a kludge until Component/DocumentZoningApi systems are unified
  val regionIdGen = IdGenerator[RegionID]()


  // FIXME: remove this kludge
  //   map component to zone when there is a 1-to-1 correspondence and a TextReflow for the Zone involved
  // val componentIdToZoneId: mutable.HashMap[Int@@ComponentID, Int@@ZoneID] = mutable.HashMap()
  // val componentIdToRegionId: mutable.HashMap[Int@@ComponentID, Int@@RegionID] = mutable.HashMap()

  // def getTargetRegionForComponent(cc: Int@@ComponentID): Option[Int@@RegionID] = {
  //   componentIdToRegionId.get(cc)
  // }


  def getTextReflow(zoneId: Int@@ZoneID): Option[TextReflow] = {
    docStore.getTextReflowForZone(zoneId)
  }

  def getPageVisualLines(pageId: Int@@PageNum): Seq[Component]  = for {
    pageLineCC <- getPageIndex(pageId).getComponentsWithLabel(LB.PageLines)
    vline <- pageLineCC.getChildren(LB.VisualLine)
  } yield vline

  def getDocumentVisualLines(): Seq[Seq[Component]] = for {
    pageId <- getPages
  } yield getPageVisualLines(pageId)


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
    c.targetRegion.page.stable.pageNum
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

  def getPageIndex(pageNum: Int@@PageNum) = pageIndexes(pageNum)

  def removeComponent(c: Component): Unit = {
    // vtrace.trace("removeComponent" withTrace showComponent(c))
    val pinfo = getPageIndex(getPageForComponent(c))
    pinfo.componentToLabels -= c.id
    pinfo.componentIndex.remove(c)
    // FIXME also delete label maps, etc.
  }

  def labelRegion(components: Seq[Component], role: Label): Option[(RegionComponent, TargetRegion)] = {
    if (components.isEmpty) None else {
      val totalBounds = components.map(_.bounds).reduce(_ union _)
      val targetPages = components.map(_.pageNum.unwrap)
      val numOfTargetPages =  targetPages.toSet.size

      if (numOfTargetPages != 1) {
        sys.error(s"""cannot label connected components from different pages (got pages=${targetPages.mkString(", ")})""")
      }

      val pageNum =  PageNum(targetPages.head)

      val pageId = docStore.getPage(docId, pageNum).get
      val regionId = docStore.addTargetRegion(pageId, totalBounds)
      val targetRegion = docStore.getTargetRegion(regionId)
      val pageRegion = PageRegion(targetRegion.page, targetRegion.bbox)

      val region = createRegionComponent(pageRegion, role)
      // componentIdToRegionId.put(region.id, targetRegion.id)

      Some((region, targetRegion))
    }
  }


  def createRegionComponent(targetRegion: PageRegion, role: Label): RegionComponent = {
    val region = RegionComponent(componentIdGen.nextId, role, targetRegion, this)
    addComponent(region)

    region
  }

  def addCharAtom(pageAtom: CharAtom): AtomicComponent = {
    val c = AtomicComponent(componentIdGen.nextId, pageAtom, this)
    addComponent(c)
    c
  }

  def getPageAtoms(pageNum: Int@@PageNum): Seq[AtomicComponent] = {
    getPageIndex(pageNum).getPageAtoms
  }

  def getComponent(id: Int@@ComponentID, pageId: Int@@PageNum): Component = {
    getPageIndex(pageId).componentIndex.getItem(id.unwrap)
  }

  def addComponent(c: Component): Component = {
    val pageNum = c.targetRegion.page.stable.pageNum
    getPageIndex(pageNum)
      .addComponent(c)
  }

  def getPageGeometry(p: Int@@PageNum) = pageIndexes(p).pageGeometry

  def getPages(): List[Int@@PageNum] = {
    pageIndexes.keys.toList.sortBy(PageNum.unwrap(_))
  }


  def addPage(pageGeometry: PageGeometry): PageIndex = {
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
