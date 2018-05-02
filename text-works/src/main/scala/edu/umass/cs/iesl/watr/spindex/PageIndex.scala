package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import watrmarks._
import geometry._
import geometry.syntax._

import rindex._
import utils.OrderedDisjointSet

import utils.ExactFloats._
import textboxing.{TextBoxing => TB}
import scala.reflect.ClassTag
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

  var _isIndexed: Boolean = false
  def isIndexed(): Boolean = _isIndexed
  def setIndexed(b: Boolean): Unit = _isIndexed = b

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

  lazy val pageItems: Array[ExtractedItem] =
    extractedItems.slice(firstItemOffset, firstItemOffset+itemsLen)

  lazy val lastItemOffset = firstItemOffset + itemsLen

  var pageVerticalJumps: mutable.ListBuffer[Int@@FloatRep] = mutable.ListBuffer()

  def addPageVerticalJumps(jumps: Seq[Int@@FloatRep]): Unit = {
    pageVerticalJumps ++= jumps
  }

  object shapes {
    type Shape = LabeledShape[GeometricFigure]

    val shapeMap: mutable.LongMap[Shape] = {
      mutable.LongMap[Shape]()
    }

    def getAllShapes(): Seq[Shape] = {
      shapeMap.values.toSeq
    }

    def getById(id: Int@@ShapeID): Shape = {
      shapeMap(id.unwrap.toLong)
    }

    // val extractedItemShapes = gcol.HashBasedTable.create[Int@@CharID, Label, Shape]()

    val shapeIDGen = utils.IdGenerator[ShapeID]()
    val shapeRIndex: RTreeIndex[Shape] = RTreeIndex.createFor[Shape]()

    def indexShape(shape: GeometricFigure, labels: Label*): Shape = {
      val lshape = LabeledShape(shape, labels.toSet, shapeIDGen.nextId)
      shapeRIndex.add(lshape)
      shapeMap.put(lshape.id.unwrap.toLong, lshape)
      lshape.setIndexed(true)
      lshape
    }

    def unindexShape(lshape: Shape): Unit = {
      shapeRIndex.remove(lshape)
      lshape.setIndexed(false)
    }

    def reindexShapes(l: Label): Unit = {
      getAllShapes().foreach { shape =>
        if (shape.hasLabel(l) && !shape.isIndexed()) {
          shapeRIndex.add(shape)
          shape.setIndexed(true)
        }
      }
    }


    def deleteShape(lshape: Shape): Unit = {
      unindexShape(lshape)
      lshape.setIndexed(false)
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

    def getClusterRoot(l: Label, s: Shape): Option[Shape] = {
      disjointSets.get(l).flatMap{set =>
        if (set.contains(s)) Some(set.getCanonical(s)) else None
      }
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


}
