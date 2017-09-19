package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import watrmarks._
import geometry._
import geometry.syntax._

import watrmarks.{StandardLabels => LB}
import rindex._
import utils.OrderedDisjointSet

import textgrid._
// import utils.ExactFloats._
import textboxing.{TextBoxing => TB}
import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import com.google.{common => guava}
import guava.{collect => gcol}



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


object PageIndex {
  // import java.nio.file.Path
  // import TypeTags._

  // def load(path: Path): PageIndex = {
  //   val rtree = RTreeIndex.load[Component](path)
  //   val pageGeometry = PageGeometry(PageNum(0), LTBounds.empty)
  //   new PageIndex(pageGeometry, rtree)
  // }

  def qualifyCluster(l: Label): Label = { l.qualifiedAs("cluster") }
  def qualifyOrdering(l: Label): Label = { l.qualifiedAs("ordering") }
  def qualifyRelation(l: Label): Label = { l.qualifiedAs("relation") }
  def qualifyRep(l: Label): Label = { l.qualifiedAs("rep") }

}



case class LabeledShape(
  shape: GeometricFigure,
  labels: Set[Label]
) {
  def hasLabel(l: Label) = labels.exists(_ == l)
}


object LabeledShape {
  implicit object ShapeIndexable extends RTreeIndexable[LabeledShape] {
    def id(t: LabeledShape): Int = 0
    def ltBounds(t: LabeledShape): LTBounds = {
      minBoundingRect(t.shape)
    }
  }

}

class PageIndex(
  val pageGeometry: PageGeometry,
) {

  import PageIndex._

  val componentRTree: RTreeIndex[Component] = RTreeIndex.createFor[Component]()
  val shapeRIndex: RTreeIndex[LabeledShape] = RTreeIndex.createFor[LabeledShape]()

  val componentMap: mutable.LongMap[Component] = {
    val initMap = componentRTree.getItems
      .map { item => (item.id.unwrap.toLong, item) }

    mutable.LongMap[Component](initMap:_*)
  }


  def getById(id: Int@@ComponentID): Component = {
    componentMap(id.unwrap.toLong)
  }


  lazy val pageNum = pageGeometry.pageNum
  val rtreeArtifactName = s"page-${pageNum}.rtree"

  def saveToBytes(): Array[Byte] = {
    RTreeIndex.saveBytes(componentRTree)
  }

  val componentToLabelMMap = gcol.ArrayListMultimap.create[Int@@ComponentID, Label]()

  val textTable = gcol.HashBasedTable.create[Int@@ComponentID, Label, TextGrid.Row]()

  val orderingTable = mutable.HashMap[Label, mutable.ListBuffer[Int@@ComponentID]]()

  def setComponentText(cc: Component, l: Label, t: TextGrid.Row): Unit = {
    textTable.put(cc.id, l, t)
  }

  def getComponentText(cc: Component, l: Label): Option[TextGrid.Row] = {
    if (textTable.contains(cc, l)) Some(textTable.get(cc, l))
    else None
  }


  val attributeTable = gcol.HashBasedTable.create[Int@@ComponentID, Label, Any]()

  def setAttribute[A](cc: Int@@ComponentID, l: Label, attr: A)
    (implicit act: ClassTag[A]): Unit = {
    val typedLabel = l.qualifiedAs(act.runtimeClass.getSimpleName)
    attributeTable.put(cc, typedLabel, attr)
  }

  def getAttribute[A](cc: Int@@ComponentID, l: Label)
    (implicit act: ClassTag[A]): Option[A] = {
    val typedLabel = l.qualifiedAs(act.runtimeClass.getSimpleName)

    if (attributeTable.contains(cc, typedLabel)) {
      Some(attributeTable.get(cc, typedLabel).asInstanceOf[A])
    } else None
  }

  val disjointSets: mutable.HashMap[Label, OrderedDisjointSet[Component]] = mutable.HashMap()


  def initClustering(l: Label, f: Component => Boolean): Seq[Component] = {
    assume(!disjointSets.contains(l))
    val toAdd = componentRTree.getItems.filter(f)
    disjointSets.getOrElseUpdate(l,
      OrderedDisjointSet.apply[Component](toAdd:_*)
    )
    toAdd
  }

  def reportClusters(): Unit = {
    import TB._
    val allSets = disjointSets.keys.toList
      .map{ l =>
        val dsets = disjointSets(l)
        val setStrs = dsets.sets().map{ set =>
          val canon = dsets.getCanonical(set.head)
          val members = set.take(2).map(_.id).mkString(", ")
          val membersFull = set.take(8).map(_.toString().box)

          s"[${canon}] = (${set.length}) ${members} ..." atop indent(2)(vcat(membersFull))
        }

        vjoin(left)(
          s"$l => ",
          indent(2)(vsep(setStrs, 1)),
          "~"*20
        )
      }

    val res = vjoin(left)(indent(4)(vcat(allSets)))

    println(res)
  }

  def unionAll(l: Label, cs: Seq[Component]): Unit = {
    val _ = _addCluster(l, cs)
  }

  def union(l: Label, c1: Component, c2: Component): Unit = {
    val set = disjointSets.getOrElseUpdate(l,
      OrderedDisjointSet.apply[Component]()
    )
    set.union(c1, c2)
  }




  def appendToOrdering(l: Label, cc: Component): Unit = {
    val orderingBuffer = orderingTable.getOrElseUpdate(qualifyOrdering(l), mutable.ListBuffer())
    orderingBuffer.append(cc.id)
  }

  def setOrdering(l: Label, cs: Seq[Component]): Unit = {
    val orderingBuffer = mutable.ListBuffer[Int@@ComponentID]()
    orderingBuffer.appendAll(cs.map(_.id))
    orderingTable.put(qualifyOrdering(l),orderingBuffer)
  }

  def getOrdering(l: Label): Seq[Component] = {
    val orderingBuffer = orderingTable.getOrElseUpdate(qualifyOrdering(l), mutable.ListBuffer())
    orderingBuffer.map(id => getById(id))
  }

  val relationTable = gcol.HashBasedTable.create[Int@@ComponentID, Label, Int@@ComponentID]()
  // l: String@@Relation
  def addRelation(lhs: Component, l: Label, rhs: Component): Unit = {
    relationTable.put(lhs.id, qualifyRelation(l), rhs.id)
  }

  def getRelation(lhs: Component, l: Label): Option[Component] = {
    if (relationTable.contains(lhs.id, qualifyRelation(l))) {
      Some(getById(relationTable.get(lhs.id, qualifyRelation(l))))
    } else None
  }

  // l: String@@DisjointCluster
  def addCluster(l: Label, cs: Seq[Component]): Component = {
    assume(cs.nonEmpty)

    _addCluster(qualifyCluster(l), cs)
  }

  private def _addCluster(l: Label, cs: Seq[Component]): Component = {
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

    addLabel(canonical, qualifyRep(l))

    canonical
  }

  def getClusterMembers(l: Label, cc: Component): Option[Seq[Component]] = {
    _getClusterMembers(l.qualifiedAs("cluster"), cc)
  }
  private def _getClusterMembers(l: Label, cc: Component): Option[Seq[Component]] = {
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

  def addShape(shape: GeometricFigure, labels: Label*): Unit = {
    val lshape = LabeledShape(shape, labels.toSet)
    shapeRIndex.add(lshape)
  }

  def removeShape(shape: GeometricFigure, labels: Label*): Unit = {
    val lshape = LabeledShape(shape, labels.toSet)
    shapeRIndex.remove(lshape)
  }

  def removeShapes(): Unit = {
    shapeRIndex.clearAll()
  }

  def addComponent(c: Component): Component = {
    componentMap.put(c.id.unwrap.toLong, c)
    componentRTree.add(c)
    addLabel(c, c.roleLabel)
    c.labels().foreach { l =>
      addLabel(c, l)
    }
    c
  }

  def removeComponent(c: Component): Unit = {
    componentToLabelMMap.removeAll(c.id)
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
    componentToLabelMMap.get(cid).asScala.toSet
  }

  def getComponentsWithLabel(l: Label): Seq[Component] = {
    val empty = guava.collect.HashMultimap.create[Label, Int@@ComponentID]()
    val inv = guava.collect.Multimaps.invertFrom[Label, Int@@ComponentID, gcol.Multimap[Label, Int@@ComponentID]](componentToLabelMMap, empty)

    inv.get(l).asScala.toSeq.map(id =>getById(id))
  }


  def addLabel(c: Component, l: Label)  = {
    if (!c.hasLabel(l)) {
      c.addLabel(l)
      componentToLabelMMap.put(c.id, l)
    }
  }

  def removeLabel(c: Component, l: Label) = {
    c.removeLabel(l)
    componentToLabelMMap.remove(c.id, l)
  }

  def searchShapes(
    queryRegion: LTBounds,
    intersectFunc: (GeometricFigure, GeometricFigure) => Boolean,
    labels: Label*
  ): Seq[LabeledShape] = {
    shapeRIndex.search(queryRegion, { lshape =>
      (intersectFunc(lshape.shape, queryRegion)
        && labels.forall(lshape.hasLabel(_)))
    })
  }

  def searchComponents(
    queryRegion: LTBounds,
    intersectFunc: (GeometricFigure, GeometricFigure) => Boolean,
    labels: Label*
  ): Seq[Component] = {

    componentRTree.search(queryRegion, {cc =>
      (intersectFunc(cc.bounds(), queryRegion)
        && labels.forall(cc.hasLabel(_)))
    })
  }

  def searchIntersecting(queryRegion: LTBounds, labels: Label*): Seq[Component] =
    searchComponents(queryRegion, shapesIntersect(_, _), labels:_*)

  def searchOverlapping(queryRegion: LTBounds, labels: Label*): Seq[Component] =
    searchComponents(queryRegion, shapesIntersect(_, _), labels:_*)

  def searchTouching(queryRegion: LTBounds, labels: Label*): Seq[Component] =
    searchComponents(queryRegion, shapesIntersect(_, _), labels:_*)

}
