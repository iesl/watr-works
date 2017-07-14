package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import watrmarks._
import geometry._

import watrmarks.{StandardLabels => LB}
import rindex._

/**
  A PageIndex wraps a RTreeIndex for Components, and adds:
    - the ability to associate labels with components
    - A tree-like parent/child relationship between components, which provides, e.g., a reading order

  */

case class PageIndex(
  componentIndex: RTreeIndex[Component],
  startingGeometry: PageGeometry,
  componentToLabels: mutable.HashMap[Int@@ComponentID, mutable.ArrayBuffer[Label]] = mutable.HashMap(),
  componentToChildren: mutable.HashMap[Int@@ComponentID, mutable.HashMap[Label, Seq[Int@@ComponentID]]] = mutable.HashMap(),
  labelToComponents: mutable.HashMap[Label, mutable.ArrayBuffer[Int@@ComponentID]] = mutable.HashMap()
) {

  var pageGeometry: PageGeometry = startingGeometry
  def getPageGeometry(): PageGeometry = pageGeometry
  def adjustBounds(bbox: LTBounds): Unit = pageGeometry = pageGeometry.copy(bounds=bbox)

  def addComponent(c: Component): Component = {
    componentIndex.add(c)
    addLabel(c, c.roleLabel)
    c
  }

  def getPageAtoms(): Seq[AtomicComponent] = {
    componentIndex.getItems
      .filter(_.roleLabel==LB.PageAtom)
      .map(_.asInstanceOf[AtomicComponent])
  }


  def setChildrenWithLabel(cid: Int@@ComponentID, l: Label, tree: Seq[Int@@ComponentID]):Unit = {
    val lmap = componentToChildren.getOrElse(cid, mutable.HashMap())
    lmap.put(l, tree)
    componentToChildren.put(cid, lmap)
  }


  def getChildrenWithLabel(cid: Int@@ComponentID, l: Label): Option[Seq[Int@@ComponentID]] = {
    for {
      lt <- componentToChildren.get(cid)
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
