package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import watrmarks._
import geometry._

import watrmarks.{StandardLabels => LB}
import rindex._
import utils.OrderedDisjointSet



/**
  A PageIndex wraps an RTree[Component], and adds:
  - Clustering components via:
    - Disjoint sets (with internal ordering)
       e.g., LB.VisualLine -> DisjointSet()


  */

class PageIndex(
  val pageGeometry: PageGeometry
) {
  val componentIndex: RTreeIndex[Component] = RTreeIndex.createFor[Component]()

  val componentToLabels: mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]] = mutable.HashMap()
  val labelToComponents: mutable.HashMap[Label, mutable.ArrayBuffer[Int@@ComponentID]] = mutable.HashMap()

  val componentToText: mutable.HashMap[Int@@ComponentID,
    mutable.HashMap[Label, TextGrid.Row]
  ] = mutable.HashMap()


  val disjointSets: mutable.HashMap[Label, OrderedDisjointSet[Component]] = mutable.HashMap()

  def initClustering(l: Label, f: Component => Boolean): Unit = {
    val toAdd = componentIndex.getItems.filter(f)
    disjointSets.getOrElseUpdate(l,
      OrderedDisjointSet.apply[Component](toAdd:_*)
    )
  }

  def setComponentText(c: Component, l: Label, t: TextGrid.Row): Unit = {
    componentToText
      .getOrElseUpdate(c.id, mutable.HashMap())
      .put(l, t)
  }

  def getComponentText(c: Component, l: Label): Option[TextGrid.Row] = {
    componentToText.get(c.id).flatMap(_.get(l))
  }

  def addCluster(l: Label, cs: Seq[Component]): Component = {
    assume(cs.nonEmpty)

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
    canonical.addLabel(LB.Canonical)
    canonical.addLabel(l)
    // println(s"addCluster(${canonical.labels})")
    canonical
  }

  def getClusterSets(l: Label): Option[OrderedDisjointSet[Component]] = {
    disjointSets.get(l)
  }

  def getClusters(l: Label): Seq[Seq[Component]] = {
    disjointSets.get(l).map{set =>
      set.sets.toSeq.map(_.toSeq)
    } getOrElse(Seq())
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
    componentIndex.add(c)
    addLabel(c, c.roleLabel)
    c
  }

  def updateComponent(c: Component, f: Component => Component): Component = {

    removeLabel(c, c.roleLabel)
    componentIndex.remove(c)

    val cUpdate = f(c)
    componentIndex.add(cUpdate)
    addLabel(cUpdate, cUpdate.roleLabel)
    cUpdate
  }

  def getPageAtoms(): Seq[AtomicComponent] = {
    componentIndex.getItems
      .filter(_.roleLabel==LB.PageAtom)
      .map(_.asInstanceOf[AtomicComponent])
  }

  def getImageAtoms(): Seq[RegionComponent] = {
    componentIndex.getItems
      .filter(_.roleLabel==LB.Image)
      .map(_.asInstanceOf[RegionComponent])
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
