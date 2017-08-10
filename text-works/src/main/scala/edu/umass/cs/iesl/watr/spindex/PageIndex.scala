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


/**
  A PageIndex wraps an RTree[Component], and adds:
  - Clustering components via:
    - Disjoint sets (with internal ordering)
       e.g., LB.VisualLines -> DisjointSet()

  - Ordering components

  - Quick access to components with a particular label

  */

object PageIndex {
  // import segment._
  // import java.io.ByteArrayInputStream
  // import java.io.ByteArrayOutputStream
  // import java.io.IOException
  // import java.io.ObjectInputStream
  // import java.io.ObjectOutputStream
  // import java.io.Serializable
  // import java.nio.charset.Charset
  // import java.nio.file.{Files, Paths}

  // def rtreeAccess(entry: String, pageNum: Int@@PageNum) = for {
  //   entry     <- corpus.entry(entry)
  //   group     <- entry.getArtifactGroup("rtrees")
  //   rtreeBlob <- group.getArtifact(s"page-${pageNum}.rtree")
  //   rtreePath <- rtreeBlob.asPath
  // } {
  //   rindex.RTreeIndex.load(rtreePath.toNIO)



  // val rtree = pageIndex.componentRTree.spatialIndex
  // val rtreeSerializer = DocumentSegmenter.createRTreeSerializer()
  // val baos = new java.io.ByteArrayOutputStream()
  // rtreeSerializer.write(rtree, baos)
  // baos.toByteArray()

}
class PageIndex(
  val pageGeometry: PageGeometry
) {

  lazy val pageNum = pageGeometry.id
  val rtreeArtifactName = s"page-${pageNum}.rtree"

  def saveToBytes(): Array[Byte] = {
    ???
  }

  val componentRTree: RTreeIndex[Component] = RTreeIndex.createFor[Component]()

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
    componentToText
      .getOrElseUpdate(c.id, mutable.HashMap())
      .put(l, t)
  }

  def getComponentText(c: Component, l: Label): Option[TextGrid.Row] = {
    componentToText.get(c.id).flatMap(_.get(l))
  }

  def unionAll(l: Label, cs: Seq[Component]): Unit = {
    addCluster(l, cs)
  }

  def union(l: Label, c1: Component, c2: Component): Unit = {
    val set = disjointSets.getOrElseUpdate(l,
      OrderedDisjointSet.apply[Component]()
    )
    set.union(c1, c2)
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
    componentRTree.add(c)
    addLabel(c, c.roleLabel)
    c
  }

  def removeComponent(c: Component): Unit = {
    c.labels.foreach { label =>
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
