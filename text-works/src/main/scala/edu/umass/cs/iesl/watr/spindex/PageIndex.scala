package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import watrmarks._
import geometry._

import watrmarks.{StandardLabels => LB}
import rindex._
import utils.DisjointSet

/**
  A PageIndex wraps an RTree[Component], and adds:
  - Connecting components via:
    - Disjoint sets
       LB.VisualLine -> DisjointSet()
       LB.Formula -> DisjointSet()

    - Ordering (List[Component])

  */

class PageIndex(
  val pageGeometry: PageGeometry
) {
  val componentIndex: RTreeIndex[Component] = RTreeIndex.createFor[Component]()

  val componentToLabels: mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]] = mutable.HashMap()
  val componentToChildren: mutable.HashMap[Int@@ComponentID, mutable.HashMap[Label, Seq[Int@@ComponentID]]] = mutable.HashMap()
  val labelToComponents: mutable.HashMap[Label, mutable.ArrayBuffer[Int@@ComponentID]] = mutable.HashMap()

  val disjointSets: mutable.HashMap[Label, DisjointSet[Component]] = mutable.HashMap()

  def initDisjointCluster(l: Label, f: Component => Boolean): Unit = {
    val toAdd = componentIndex.getItems.filter(f)
    disjointSets.getOrElseUpdate(l,
      DisjointSet.apply[Component](toAdd:_*)
    )
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
