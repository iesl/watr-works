package edu.umass.cs.iesl.watr
package spindex

import edu.umass.cs.iesl.watr.utils.VisualTracer
import scala.collection.mutable

import watrmarks._
import TypeTags._
import scalaz.@@
import ComponentTypeEnrichments._
import utils.IdGenerator

import VisualTracer._

import scalaz.Tree

// One or more indexes over a given page geometry, along with label maps
case class PageInfo(
  pageId: Int@@PageID,
  charAtomIndex: SpatialIndex[CharAtom],
  componentIndex: SpatialIndex[Component],
  geometry: PageGeometry,
  componentToLabels: mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]] = mutable.HashMap(),
  componentToChildTrees: mutable.HashMap[Int@@ComponentID, mutable.HashMap[Label, Tree[Int@@ComponentID]]] = mutable.HashMap(),
  labelToComponents: mutable.HashMap[Label, mutable.ArrayBuffer[Int@@ComponentID]] = mutable.HashMap()
) {

  def getPageAtoms(): Seq[PageAtom] = {
    charAtomIndex.getItems
  }

  def setChildTreeWithLabel(cid: Int@@ComponentID, l: Label, tree: Tree[Int@@ComponentID]):Unit = {
    val lmap = componentToChildTrees.getOrElse(cid, mutable.HashMap())
    val l0 = lmap.put(l, tree)
    componentToChildTrees.put(cid, lmap)
  }


  def getChildTreeWithLabel(cid: Int@@ComponentID, l: Label): Option[Tree[Int@@ComponentID]] = {
    for {
      lt <- componentToChildTrees.get(cid)
      t <- lt.get(l)
    } yield t
  }

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



class ZoneIndexer()  {

  val vtrace: VisualTracer = new VisualTracer()

  import SpatialIndex._

  val pageInfos = mutable.HashMap[Int@@PageID, PageInfo]()

  // ID generators
  val componentIdGen = utils.IdGenerator[ComponentID]()
  val labelIdGen = IdGenerator[LabelID]()

  type BioSpine = mutable.MutableList[BioNode]
  val bioSpines = mutable.Map[String, BioSpine]()

  def bioSpine(name: String): BioSpine = {
    bioSpines.getOrElseUpdate(name, mutable.MutableList[BioNode]())
  }

  def setChildTreeWithLabel(c: Component, l: Label, tree: Tree[Int@@ComponentID]):Unit = {
    val pageInfo = pageInfos(getPageForComponent(c))
    pageInfo.setChildTreeWithLabel(c.id, l, tree)
  }

  def getChildTreeWithLabel(c: Component, l: Label): Option[Tree[Int@@ComponentID]] = {
    val pageInfo = pageInfos(getPageForComponent(c))
    val sdf = pageInfo.componentIndex.get(0)
    pageInfo.getChildTreeWithLabel(c.id, l)
  }

  def getChildTree(c: Component, l: Label): Option[Tree[Component]] = {
    val pageInfo = pageInfos(getPageForComponent(c))
    pageInfo.getChildTreeWithLabel(c.id, l)
      .map(tree => tree.map{ cid =>
        pageInfo.componentIndex.get(cid.unwrap).getOrElse {
          sys.error(s"getChildTree(${c}, ${l}) contained an invalid component id: ${cid}")
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

  def getPageInfo(pageId: Int@@PageID) = pageInfos(pageId)

  def removeComponent(c: Component): Unit = {
    vtrace.trace("removeComponent" withTrace showComponent(c))
    val pinfo = getPageInfo(getPageForComponent(c))
    pinfo.componentIndex.remove(c)
  }

  def labelRegion(components: Seq[Component], l: Label): Option[RegionComponent] = {
    if (components.isEmpty) None else {
      val targetRegions = components.flatMap(_.targetRegions)
      val targetPages = targetRegions.map(_.target.unwrap)
      val numOfTargetPages =  targetPages.toSet.size

      if (numOfTargetPages != 1) {
        sys.error(s"""cannot label connected components from different pages (got pages=${targetPages.mkString(", ")})""")
      }

      val totalRegion = targetRegions.reduce(_ union _)

      val region = createRegionComponent(totalRegion, l)

      // l.foreach(region.addLabel)

      vtrace.trace("labelComponents" withTrace all(components.map(showComponent(_))))

      Some(region)
    }
  }


  def connectComponents(components: Seq[Component], l: Label): Option[Component] = {
    // labeledRegion.map({regionComponent =>
    //   regionComponent.setChildTree(l,
    //     Tree.Node(regionComponent, components.map(Tree.Leaf(_)).toStream)
    //   )

    //   vtrace.trace(message("connectComponents"))

    //   regionComponent
    // })
    ???
  }

  def createRegionComponent(tr: TargetRegion, role: Label): RegionComponent = {
    val region = RegionComponent(componentIdGen.nextId, role, tr, this)
    val pinfo = getPageInfo(getPageForComponent(region))
    pinfo.componentIndex.add(region)

    vtrace.trace("create RegionComponent" withTrace showComponent(region))

    region
  }

  def toComponent(pageAtom: PageAtom): Component = {
    val c = PageComponent(componentIdGen.nextId, pageAtom, this)
    val pageId = pageAtom.region.target
    getPageInfo(pageId).componentIndex.add(c)

    c
  }

  // def concatRegions(regions: Seq[PageAtom], l: Option[Label]=None): Component = {
  //   connectComponents(regions.map(toComponent(_)))
  // }


  // def appendComponent(component: Component, app: Component): Component = {
  //   component match {
  //     case c: PageComponent =>
  //       connectComponents(Seq(component, app))
  //     case c: ConnectedComponents =>
  //       c.components += app
  //       c

  //     case c: RegionComponent =>
  //       // val extRegion = c.region.bbox.union(app.bounds).targetRegionTo(c.region.target)
  //       // c.region = extRegion
  //       ???
  //   }
  // }

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



}
