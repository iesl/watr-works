package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import watrmarks._
import geometry._
import geometry.syntax._

import segment.{SegmentationLabels => LB}
import rindex._
import utils.OrderedDisjointSet

import textgrid._
import utils.ExactFloats._
import textboxing.{TextBoxing => TB}
import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import com.google.{common => guava}
import guava.{collect => gcol}
import extract.ExtractedItem
import com.github.davidmoten.rtree.{geometry => RG}



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




case class LabeledShape[+T <: GeometricFigure](
  shape: T,
  labels: Set[Label],
  id: Int@@ShapeID
) {
  def hasLabel(l: Label) = labels.exists(_ == l)

  lazy val rtreeGeometry: RG.Geometry =
    RGeometryConversions.geometricFigureToRtreeGeometry(shape)
}


object LabeledShape {
  implicit object ShapeIndexable extends RTreeIndexable[LabeledShape[GeometricFigure]] {
    def id(t: LabeledShape[GeometricFigure]): Int = t.id.unwrap
    def ltBounds(t: LabeledShape[GeometricFigure]): LTBounds =
      minBoundingRect(t.shape)

    def rtreeGeometry(t: LabeledShape[GeometricFigure]): RG.Geometry =
      t.rtreeGeometry
  }

}

class PageIndex(
  val pageGeometry: PageGeometry,
  val extractedItems: Array[ExtractedItem],
  firstItemOffset: Int,
  itemsLen: Int
) {

  import PageIndex._

  lazy val pageNum = pageGeometry.pageNum
  val rtreeArtifactName = s"page-${pageNum}.rtree"

  lazy val pageItems: Array[ExtractedItem] =
    extractedItems.slice(firstItemOffset, firstItemOffset+itemsLen)

  lazy val lastItemOffset = firstItemOffset + itemsLen

  def saveToBytes(): Array[Byte] = {
    // RTreeIndex.saveBytes(components.componentRTree)
    ???
  }

  var pageVerticalJumps: mutable.ListBuffer[Int@@FloatRep] = mutable.ListBuffer()

  def addPageVerticalJumps(jumps: Seq[Int@@FloatRep]): Unit = {
    pageVerticalJumps ++= jumps
  }

  object shapes {
    type Shape = LabeledShape[GeometricFigure]

    val shapeMap: mutable.LongMap[Shape] = {
      mutable.LongMap[Shape]()
    }

    def getById(id: Int@@ShapeID): Shape = {
      shapeMap(id.unwrap.toLong)
    }

    val extractedItemShapes = gcol.HashBasedTable.create[Int@@CharID, Label, Shape]()

    val shapeIDGen = utils.IdGenerator[ShapeID]()
    val shapeRIndex: RTreeIndex[Shape] = RTreeIndex.createFor[Shape]()

    def indexShape(shape: GeometricFigure, labels: Label*): Shape = {
      val lshape = LabeledShape(shape, labels.toSet, shapeIDGen.nextId)
      shapeRIndex.add(lshape)
      shapeMap.put(lshape.id.unwrap.toLong, lshape)
      lshape
    }

    def unindexShape(lshape: Shape): Unit = {
      shapeRIndex.remove(lshape)
    }


    def deleteShape(lshape: Shape): Unit = {
      unindexShape(lshape)
      shapeMap.remove(lshape.id.unwrap.toLong)
    }

    def deleteShapes(): Unit = {
      shapeMap.clear()
      shapeRIndex.clearAll()
    }

    def getShapesWithLabel(l: Label): Seq[Shape] = {
      shapeRIndex.getItems
        .filter { item =>
          item.labels.contains(l)
        }
    }

    def searchShapes(
      query: GeometricFigure,
      intersectFunc: (GeometricFigure, GeometricFigure) => Boolean,
      labels: Label*
    ): Seq[Shape] = {
      shapeRIndex.search(query, { lshape =>
        (intersectFunc(lshape.shape, query)
          && labels.forall(lshape.hasLabel(_)))
      })
    }
    def searchShapes(
      query: GeometricFigure,
      labels: Label*
    ): Seq[Shape] = {
      shapeRIndex.search(query, { lshape =>
        labels.forall(lshape.hasLabel(_))
      })
    }

    val shapeAttributeTable = gcol.HashBasedTable.create[Int@@ShapeID, Label, Any]()

    def setShapeAttribute[A](shapeId: Int@@ShapeID, l: Label, attr: A)
      (implicit act: ClassTag[A]): Unit = {
      val typedLabel = l.qualifiedAs(act.runtimeClass.getSimpleName)
      shapeAttributeTable.put(shapeId, typedLabel, attr)
    }

    def getShapeAttribute[A](shapeId: Int@@ShapeID, l: Label)
      (implicit act: ClassTag[A]): Option[A] = {
      val typedLabel = l.qualifiedAs(act.runtimeClass.getSimpleName)

      if (shapeAttributeTable.contains(shapeId, typedLabel)) {
        Some(shapeAttributeTable.get(shapeId, typedLabel).asInstanceOf[A])
      } else None
    }

    val disjointSets: mutable.HashMap[Label, OrderedDisjointSet[Shape]] = mutable.HashMap()

    def ensureCluster[T <: GeometricFigure](l: Label): OrderedDisjointSet[Shape] = {
      disjointSets.getOrElseUpdate(l,
        OrderedDisjointSet.apply[Shape]()
      )
    }

    def addCluster[T <: GeometricFigure](l: Label, shapes: Seq[LabeledShape[T]]): LabeledShape[T] = {
      assume(shapes.nonEmpty)
      _addCluster(l, shapes)
    }

    def initClustering(l: Label, f: Shape => Boolean): Seq[Shape] = {
      assume(!disjointSets.contains(l))
      val toAdd = shapeRIndex.getItems.filter(f)
      disjointSets.getOrElseUpdate(l,
        OrderedDisjointSet.apply[Shape](toAdd:_*)
      )
      toAdd
    }


    private def _addCluster[T <: GeometricFigure, R <: GeometricFigure](
      l: Label, cs: Seq[LabeledShape[T]]
    ): LabeledShape[R] = {
      assume(cs.nonEmpty)

      val set = ensureCluster(l)

      val c0 = cs.head
      set.ensure(c0)
      cs.tail.foreach { cn =>
        set.ensure(cn)
        set.union(c0, cn)
      }
      val canonical = set.getCanonical(c0)
      canonical.asInstanceOf[LabeledShape[R]]
    }

    def unionAll(l: Label, cs: Seq[Shape]): Unit = {
      val _ = _addCluster(l, cs)
    }

    def union(l: Label, c1: Shape, c2: Shape): Unit = {
      val _ = _addCluster(l, Seq(c1, c2))
    }

    def getClusterMembers(l: Label, cc: Shape): Option[Seq[Shape]] = {
      _getClusterMembers(l, cc)
    }

    private def _getClusterMembers(l: Label, cc: Shape): Option[Seq[Shape]] = {
      disjointSets.get(l).map{set =>
        set.sets.toSeq.map(_.toSeq)
          .filter(_.contains(cc))
          .headOption
      } getOrElse(None)
    }


    def getClusters(l: Label): Option[Seq[Seq[Shape]]] = {
      disjointSets.get(l)
        .map{ set => set.sets }
    }

    def getClustersWithReprID(l: Label): Seq[(Int@@ShapeID, Seq[Shape])] = {
      disjointSets.get(l).map { disjointSet =>
        disjointSet.sets.map{ cluster =>
          val repr = disjointSet.getCanonical(cluster.head)
          (repr.id, cluster)
        }
      } getOrElse(Seq())
    }

    def getClusterRoots(l: Label): Seq[Shape] = {
      disjointSets.get(l).map{set =>
        set.sets.map{ cluster =>
          set.getCanonical(cluster.head)
        }
      } getOrElse(Seq())
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

            s"[${canon}] = (${set.length}) ${members} ..." atop indent(2, vcat(left,membersFull))
          }

          vjoin(left,
            s"$l => ",
            indent(2, vjoinWith(left, vspace(1), setStrs)),
            "~"*20
          )
        }

      val res = vjoin(indent(4, vcat(left, allSets)))

      println(res)
    }

    val relationTable = gcol.HashBasedTable.create[Int@@ShapeID, Label, Int@@ShapeID]()

    def addRelation(lhs: Int@@ShapeID, l: Label, rhs: Int@@ShapeID): Unit = {
      relationTable.put(lhs, l, rhs)
    }

    def getRelation(lhs: Shape, l: Label): Option[Shape] = {
      if (relationTable.contains(lhs.id, l)) {
        Some(getById(relationTable.get(lhs.id, l)))
      } else None
    }


    val orderingTable = mutable.HashMap[Label, mutable.ListBuffer[Int@@ShapeID]]()
    def appendToOrdering(l: Label, cc: Shape): Unit = {
      val orderingBuffer = orderingTable.getOrElseUpdate(qualifyOrdering(l), mutable.ListBuffer())
      orderingBuffer.append(cc.id)
    }

    def setOrdering(l: Label, cs: Seq[Shape]): Unit = {
      val orderingBuffer = mutable.ListBuffer[Int@@ShapeID]()
      orderingBuffer.appendAll(cs.map(_.id))
      orderingTable.put(qualifyOrdering(l),orderingBuffer)
    }

    def getOrdering(l: Label): Seq[Shape] = {
      val orderingBuffer = orderingTable.getOrElseUpdate(qualifyOrdering(l), mutable.ListBuffer())
      orderingBuffer.map(id => getById(id))
    }
  }


  object components {
    val componentRTree: RTreeIndex[Component] = RTreeIndex.createFor[Component]()

    val componentMap: mutable.LongMap[Component] = {
      val initMap = componentRTree.getItems
        .map { item => (item.id.unwrap.toLong, item) }

      mutable.LongMap[Component](initMap:_*)
    }

    def getById(id: Int@@ComponentID): Component = {
      componentMap(id.unwrap.toLong)
    }



    val componentToLabelMMap = gcol.ArrayListMultimap.create[Int@@ComponentID, Label]()

    val textTable = gcol.HashBasedTable.create[Int@@ComponentID, Label, TextGrid.Row]()


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

            s"[${canon}] = (${set.length}) ${members} ..." atop indent(2, vcat(left,membersFull))
          }

          vjoin(left,
            s"$l => ",
            indent(2, vjoinWith(left, vspace(1), setStrs)),
            "~"*20
          )
        }

      val res = vjoin(indent(4, vcat(left,allSets)))

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




    val orderingTable = mutable.HashMap[Label, mutable.ListBuffer[Int@@ComponentID]]()
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


    def searchComponents(
      queryRegion: GeometricFigure,
      intersectFunc: (GeometricFigure, GeometricFigure) => Boolean,
      labels: Label*
    ): Seq[Component] = {
      componentRTree.search(queryRegion, {cc =>
        (intersectFunc(cc.bounds(), queryRegion)
          && labels.forall(cc.hasLabel(_))
        )
      })
    }
    def searchComponents(
      queryRegion: GeometricFigure,
      labels: Label*
    ): Seq[Component] = {
      componentRTree.search(queryRegion, {cc =>
        labels.forall(cc.hasLabel(_))
      })
    }

    // def searchComponents(
    //   queryRegion: LTBounds,
    //   intersectFunc: (GeometricFigure, GeometricFigure) => Boolean,
    //   labels: Label*
    // ): Seq[Component] = {

    //   componentRTree.search(queryRegion, {cc =>
    //     (intersectFunc(cc.bounds(), queryRegion)
    //       && labels.forall(cc.hasLabel(_)))
    //   })
    // }

    def searchIntersecting(queryRegion: LTBounds, labels: Label*): Seq[Component] =
      searchComponents(queryRegion, shapesIntersect(_, _), labels:_*)

    def searchOverlapping(queryRegion: LTBounds, labels: Label*): Seq[Component] =
      searchComponents(queryRegion, shapesIntersect(_, _), labels:_*)

    def searchTouching(queryRegion: LTBounds, labels: Label*): Seq[Component] =
      searchComponents(queryRegion, shapesIntersect(_, _), labels:_*)

  }




}
