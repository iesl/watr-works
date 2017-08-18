package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import watrmarks._
import geometry._

import watrmarks.{StandardLabels => LB}
import rindex._
import utils.OrderedDisjointSet

import textgrid._
import utils.ExactFloats._
import edu.umass.cs.iesl.watr.tracing._



/**
  A PageIndex wraps an RTree[Component], and adds:
  - Clustering components via:
    - Disjoint sets (with internal ordering)
       e.g., LB.VisualLines -> DisjointSet()

  - Ordering for components withing a single page

  - Weighted labels for components

  - Edges (relations) between components

  - Quick access to components with a particular label

  */

object UpgradedLabels extends TypeTagUtils {

  // import scalaz.Tag
  sealed trait DisjointSetL
  val DisjointSetL = Tag.of[DisjointSetL]


  sealed trait WeightedLabel
  val WeightedLabel = Tag.of[WeightedLabel]
  type Weight = String@@WeightedLabel
  // CategoryLabel =
  // WeightedLabel =

}

object PageIndex {
  import java.nio.file.Path
  import TypeTags._


  val noopTracer = new VisualTracer()

  def load(path: Path): PageIndex = {
    val rtree = RTreeIndex.load[Component](path)
    val pageGeometry = PageGeometry(PageNum(0), LTBounds.empty)
    new PageIndex(pageGeometry, rtree)
  }


  var activeTracingCallback: (PageIndex) => Unit = (pageIndex: PageIndex) => {
    println("called activeTracingCallback")
  }

  var activeTracer = noopTracer
  def tracer() = activeTracer
}

// class PageIndexTracer() extends VisualTracer { self =>
//   def addComponent(c: Component): Unit = {}
// }

class PageIndex(
  val pageGeometry: PageGeometry,
  val componentRTree: RTreeIndex[Component] = RTreeIndex.createFor[Component]()
) {


  val weightedComponentLabels: mutable.HashMap[
    Int@@ComponentID, mutable.HashMap[Label, Double]
  ] = mutable.HashMap()

  def assignWeight(cc: Int@@ComponentID, l: Label, initWeight: Double): Unit = ???
  def getWeight(cc: Int@@ComponentID, l: Label): Double = ???
  def modifyWeight(cc: Int@@ComponentID, l: Label, modf: Double => Double): Double = ???

  lazy val pageNum = pageGeometry.id
  val rtreeArtifactName = s"page-${pageNum}.rtree"

  def saveToBytes(): Array[Byte] = {
    RTreeIndex.saveBytes(componentRTree)
  }

  val componentToLabels: mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]] = mutable.HashMap()
  val labelToComponents: mutable.HashMap[Label, mutable.ArrayBuffer[Int@@ComponentID]] = mutable.HashMap()

  val componentToText: mutable.HashMap[Int@@ComponentID,
    mutable.HashMap[Label, TextGrid.Row]
  ] = mutable.HashMap()


  def allLabels(): Set[Label] = {
    labelToComponents.keySet.toSet
  }

  val disjointSets: mutable.HashMap[Label, OrderedDisjointSet[Component]] = mutable.HashMap()

  def initClustering(l: Label, f: Component => Boolean): Seq[Component] = {
    val toAdd = componentRTree.getItems.filter(f)
    disjointSets.getOrElseUpdate(l,
      OrderedDisjointSet.apply[Component](toAdd:_*)
    )
    toAdd
  }

  def setComponentText(c: Component, l: Label, t: TextGrid.Row): Unit = {
    val _ = componentToText
      .getOrElseUpdate(c.id, mutable.HashMap())
      .put(l, t)
  }

  // AttibuteLabel
  def getComponentText(c: Component, l: Label): Option[TextGrid.Row] = {
    componentToText.get(c.id).flatMap(_.get(l))
  }

  def unionAll(l: Label, cs: Seq[Component]): Unit = {
    val _ = addCluster(l, cs)
  }

  def union(l: Label, c1: Component, c2: Component): Unit = {
    val set = disjointSets.getOrElseUpdate(l,
      OrderedDisjointSet.apply[Component]()
    )
    set.union(c1, c2)
  }

  def setOrdering(l: Label, cs: Seq[Component]): Component = {
    assume(!disjointSets.contains(l))

    val canon = addCluster(l, cs)
    addLabel(canon, LB.Ordering)
    canon
  }

  // l: String@@Ordering
  def getOrdering(l: Label): Seq[Component] = {
    assume(disjointSets.contains(l))
    val canon = getComponentsWithLabel(LB.Ordering).filter(_.hasLabel(l)).head
    getClusterMembers(l, canon).get
  }

  // l: String@@Relation
  def addRelation(lhs: Component, l: Label, rhs: Component): Unit = {
    val _ = addCluster(l, Seq(lhs, rhs))
  }

  // l: String@@Relation
  def getRelations(lhs: Component, l: Label): Option[Seq[Component]] = {
    getClusterMembers(l, lhs).map{ rels =>
      rels.filter(_.id != lhs.id)
    }
  }

  // l: String@@DisjointCluster
  def addCluster(l: Label, cs: Seq[Component]): Component = {
    assume(cs.nonEmpty)

    // PageIndex..traceIf(true)()

    val set = disjointSets.getOrElseUpdate(l,
      OrderedDisjointSet.apply[Component]()
    )

    val c0 = cs.head
    set.add(c0)
    cs.tail.foreach { cn =>
      set.add(cn)
      set.union(c0, cn)
    }
    val canonical = set.getCanonical(c0)
    addLabel(canonical, LB.Canonical)
    addLabel(canonical, l)


    canonical
  }

  def getClusterSets(l: Label): Option[OrderedDisjointSet[Component]] = {
    disjointSets.get(l)
  }

  // l: String@@DisjointCluster
  def getClusters(l: Label): Seq[Seq[Component]] = {
    disjointSets.get(l).map{set =>
      set.sets.toSeq.map(_.toSeq)
    } getOrElse(Seq())
  }

  // l: String@@DisjointCluster
  def getClusterMembers(l: Label, cc: Component): Option[Seq[Component]] = {
    disjointSets.get(l).map{set =>
      set.sets.toSeq.map(_.toSeq)
        .filter(_.contains(cc))
        .headOption
    } getOrElse(None)
  }

  def getClusterRoots(l: Label): Seq[Component] = {
    disjointSets.get(l).map{set =>
      set.sets.toSeq.map(_.toSeq)
        .map{ cluster =>
          set.getCanonical(cluster.head)
        }
    } getOrElse(Seq())
  }

  def addComponent(c: Component): Component = {
    componentRTree.add(c)
    addLabel(c, c.roleLabel)
    c.labels().foreach { l =>
      addLabel(c, l)
    }
    c
  }

  def removeComponent(c: Component): Unit = {
    (c.roleLabel +: c.labels.toSeq).foreach { label =>
      labelToComponents.get(label)
        .map{ ccIds => ccIds -= c.id }
    }

    componentToLabels -= c.id
    componentRTree.remove(c)
  }

  def getPageAtoms(): Seq[AtomicComponent] = {
    componentRTree.getItems
      .filter(_.roleLabel==LB.PageAtom)
      .map(_.asInstanceOf[AtomicComponent])
  }

  def getImageAtoms(): Seq[RegionComponent] = {
    componentRTree.getItems
      .filter(_.roleLabel==LB.Image)
      .map(_.asInstanceOf[RegionComponent])
  }


  def getComponentLabels(cid: Int@@ComponentID): Set[Label] = {
    componentToLabels.get(cid)
      .map(_.toSet)
      .getOrElse(Set())
  }

  def getComponentsWithLabel(l: Label): Seq[Component] = {
    labelToComponents.get(l)
      .map(_.map(id => componentRTree.getItem(id.unwrap)))
      .getOrElse(Seq())
  }

  private def getComponentLabelBuffer(c: Component): mutable.ArrayBuffer[Label] = {
    componentToLabels.getOrElseUpdate(c.id, mutable.ArrayBuffer[Label]())
  }

  private def getLabelToComponentBuffer(l: Label): mutable.ArrayBuffer[Int@@ComponentID] = {
    labelToComponents.getOrElseUpdate(l, mutable.ArrayBuffer[Int@@ComponentID]())
  }

  def addLabel(c: Component, l: Label)  = {
    c.addLabel(l)
    getComponentLabelBuffer(c) += l
    getLabelToComponentBuffer(l) += c.id
  }

  def removeLabel(c: Component, l: Label) = {
    c.removeLabel(l)
    getComponentLabelBuffer(c) -= l
    getLabelToComponentBuffer(l) -= c.id
  }



  // Searching: TODO most of these functions should be pushed back into RTreeIndex class

  def rtreeSearchHasAllLabels(
    queryRegion: LTBounds,
    labels: Label*
  ): Seq[Component] = {

    componentRTree.search(queryRegion, {cc =>
      val overlapLR = (
        cc.bounds.left < queryRegion.right
          && cc.bounds.right > queryRegion.left
      )
      val overlapTB = (
        cc.bounds.top < queryRegion.bottom
          && cc.bounds.bottom > queryRegion.top
      )

      val overlapping = overlapLR && overlapTB

      overlapping && labels.forall(cc.hasLabel(_))
    })
  }

  def rtreeSearchHasLabel(
    queryRegion: LTBounds,
    label: Label
  ): Seq[Component] = {

    componentRTree.search(queryRegion, {cc =>
      cc.hasLabel(label)
    })
  }


  def rtreeSearch(
    queryRegion: LTBounds,
    label: Label, labels: Label*
  ): Seq[Component] = {
    val lbls = label :: labels.toList

    PageIndex.tracer.checkpoint("search")
    componentRTree.search(queryRegion, {cc =>
      lbls.contains(cc.roleLabel)
    })
  }

  def rtreeSearchOverlapping(
    queryRegion: LTBounds,
    label: Label, labels: Label*
  ): Seq[Component] = {
    val lbls = label :: labels.toList

    componentRTree.search(queryRegion, {cc =>
      val overlapLR = (
        cc.bounds.left < queryRegion.right
          && cc.bounds.right > queryRegion.left
      )
      val overlapTB = (
        cc.bounds.top < queryRegion.bottom
          && cc.bounds.bottom > queryRegion.top
      )

      val overlapping = overlapLR && overlapTB

      overlapping && lbls.contains(cc.roleLabel)
    })
  }

  def rtreeSearchLineHasLabel(
    queryRegion: Line,
    label: Label
  ): Seq[Component] = {

    componentRTree.searchLine(queryRegion, {cc =>
      cc.hasLabel(label)
    })
  }

  def rtreeSearchLine(
    queryRegion: Line,
    label: Label, labels: Label*
  ): Seq[Component] = {
    val lbls = label :: labels.toList

    componentRTree.searchLine(queryRegion, {cc =>
      lbls.contains(cc.roleLabel)
    })
  }
}
