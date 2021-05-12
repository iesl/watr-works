package org.watrworks
package rsearch

import scala.collection.mutable
import watrmarks._
import geometry._

import utils.OrderedDisjointSet

import textboxing.{TextBoxing => TB}
import com.google.{common => guava}
import guava.{collect => gcol}
import utils.IdGenerator

object LabeledShapeIndex {

  def qualifyCluster(l: Label): Label = { l.qualifiedAs("cluster") }
  def qualifyOrdering(l: Label): Label = { l.qualifiedAs("ordering") }
  def qualifyRelation(l: Label): Label = { l.qualifiedAs("relation") }
  def qualifyRep(l: Label): Label = { l.qualifiedAs("rep") }

  def empty[A <: GeometricFigure, Shape <: LabeledShape[A]](
    idGenerator: IdGenerator[ShapeID]
  ): LabeledShapeIndex[A, Shape] =
    new LabeledShapeIndex[A, Shape] {
      val shapeIDGen                        = idGenerator
      val shapeRIndex: RTreeIndex[A, Shape] = RTreeIndex.empty[A, Shape]()

    }

  def withRTree[A <: GeometricFigure, Shape <: LabeledShape[A]](
    rtree: RTreeIndex[A, Shape]
  ): LabeledShapeIndex[A, Shape] = {
    new LabeledShapeIndex[A, Shape] {
      val shapeIDGen = utils.IdGenerator[ShapeID]()

      val items = rtree.getItems()
      val maxId =
        if (items.isEmpty) 0
        else {
          items.maxBy(_.id.unwrap).id.unwrap
        }
      val shapeRIndex: RTreeIndex[A, Shape] = rtree
      shapeIDGen.setNextId(maxId + 1)
      shapeMap ++= items.map { i => (i.id.unwrap.toLong, i) }
    }
  }
}

trait LabeledShapeTypes[A <: GeometricFigure, Shape <: LabeledShape[A]] {
  type LineShape      = LabeledShape[Line]
  type PointShape     = LabeledShape[Point]
  type RectShape      = LabeledShape[Rect]
  type TrapezoidShape = LabeledShape[Trapezoid]
  type AnyShape       = Shape
}

abstract class LabeledShapeIndex[A <: GeometricFigure, Shape <: LabeledShape[A]]
  extends LabeledShapeTypes[A, Shape]
  with ShapeDisjointSets[A, Shape]
  with ShapeAttributes {

  def shapeIDGen: utils.IdGenerator[ShapeID]
  def shapeRIndex: RTreeIndex[A, Shape]

  def allLabeledShapes(): Seq[Shape] = shapeRIndex.getItems()

  val shapeMap: mutable.LongMap[Shape] = {
    mutable.LongMap[Shape]()
  }

  def getAllShapes(): Seq[Shape] = {
    shapeMap.values.toSeq
  }

  def getById(id: Int @@ ShapeID): Shape = {
    shapeMap(id.unwrap.toLong)
  }

  def initShape[S <: Shape](f: Int @@ ShapeID => S): S = {
    val lshape = f(shapeIDGen.nextId)
    shapeMap.put(lshape.id.unwrap.toLong, lshape)
    lshape.setIndexed(false)
    lshape
  }

  def indexShape[S <: Shape](f: Int @@ ShapeID => S): S = {
    val lshape = initShape(f)
    shapeRIndex.add(lshape)
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
    val _ = shapeMap.remove(lshape.id.unwrap.toLong)
  }

  def deleteShapes(): Unit = {
    shapeMap.clear()
    shapeRIndex.clearAll()
  }

  def getShapesWithLabel(l: Label): Seq[Shape] = {
    shapeRIndex
      .getItems()
      .filter { item =>
        item.labels.contains(l)
      }
  }

  def searchShapes(
    query: GeometricFigure,
    intersectFunc: (GeometricFigure, GeometricFigure) => Boolean,
    labels: Label*
  ): Seq[Shape] = {
    shapeRIndex.search(
      query,
      { lshape =>
        (intersectFunc(lshape.shape, query)
        && labels.forall(lshape.hasLabel(_)))
      }
    )
  }

  def searchShapes(
    query: GeometricFigure,
    labels: Label*
  ): Seq[Shape] = {
    shapeRIndex.search(
      query,
      { lshape =>
        labels.forall(lshape.hasLabel(_))
      }
    )
  }

}

trait AttrWitness[ValueT] {
  def key: String
  type AttrT = ValueT
}

trait MkAttrWitness[ValueT] {
  def apply()(implicit name: sourcecode.Name): AttrWitness[ValueT] = {
    new AttrWitness[ValueT] {
      def key: String = name.value
      override def toString() = s"<AttrWitness:${key}>"
    }
  }
}

object AttrWitness {

  def Mk[ValueT]: MkAttrWitness[ValueT] = {
    new MkAttrWitness[ValueT] {}
  }

}

trait ShapeAttributes {

  val shapeAttributeTable = gcol.HashBasedTable.create[Int @@ ShapeID, String, Any]()

  def setAttr[A](
    shapeId: Int @@ ShapeID,
    witness: AttrWitness[A],
    attr: A
  ): Unit = {
    shapeAttributeTable.put(shapeId, witness.key, attr)
  }

  def getAttr[A](
    shapeId: Int @@ ShapeID,
    witness: AttrWitness[A],
  ): Option[A] = {
    val key = witness.key

    if (shapeAttributeTable.contains(shapeId, key)) {
      Some(shapeAttributeTable.get(shapeId, key).asInstanceOf[A])
    } else None
  }
}

trait ShapeDisjointSets[A <: GeometricFigure, Shape <: LabeledShape[A]]
  extends LabeledShapeTypes[A, Shape] {

  def allLabeledShapes(): Seq[Shape]

  val disjointSets: mutable.HashMap[Label, OrderedDisjointSet[AnyShape]] = mutable.HashMap()

  def ensureCluster[T <: GeometricFigure](l: Label): OrderedDisjointSet[Shape] = {
    disjointSets.getOrElseUpdate(l, OrderedDisjointSet.apply[Shape]())
  }

  def addCluster[T <: GeometricFigure](l: Label, shapes: Seq[Shape]): Shape = {
    assume(shapes.nonEmpty)
    _addCluster(l, shapes)
  }

  def initClustering(l: Label, f: Shape => Boolean): Seq[Shape] = {
    assume(!disjointSets.contains(l))
    val toAdd = allLabeledShapes().filter(f)
    disjointSets.getOrElseUpdate(l, OrderedDisjointSet.apply[Shape](toAdd: _*))
    toAdd
  }

  private def _addCluster[T <: GeometricFigure, R <: GeometricFigure](
    l: Label,
    cs: Seq[Shape]
  ): Shape = {
    assume(cs.nonEmpty)

    val set = ensureCluster(l)

    val c0 = cs.head
    set.ensure(c0)
    cs.tail.foreach { cn =>
      set.ensure(cn)
      set.union(c0, cn)
    }
    val canonical = set.getCanonical(c0)
    canonical.asInstanceOf[Shape]
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
    disjointSets.get(l).map { set =>
      set
        .sets()
        .toSeq
        .map(_.toSeq)
        .filter(_.contains(cc))
        .headOption
    } getOrElse (None)
  }

  def getClusters(l: Label): Option[Seq[Seq[Shape]]] = {
    disjointSets
      .get(l)
      .map { set => set.sets() }
  }

  def getClustersWithReprID(l: Label): Seq[(Int @@ ShapeID, Seq[Shape])] = {
    disjointSets.get(l).map { disjointSet =>
      disjointSet.sets().map { cluster =>
        val repr = disjointSet.getCanonical(cluster.head)
        (repr.id, cluster)
      }
    } getOrElse (Seq())
  }

  def getClusterRoots(l: Label): Seq[Shape] = {
    disjointSets.get(l).map { set =>
      set.sets().map { cluster =>
        set.getCanonical(cluster.head)
      }
    } getOrElse (Seq())
  }

  def getClusterRoot(l: Label, s: Shape): Option[Shape] = {
    disjointSets.get(l).flatMap { set =>
      if (set.contains(s)) Some(set.getCanonical(s)) else None
    }
  }

  def reportClusters(): Unit = {
    import TB._

    val allSets = disjointSets.keys.toList
      .map { l =>
        val dsets = disjointSets(l)
        val setStrs = dsets.sets().map { set =>
          val canon       = dsets.getCanonical(set.head)
          val members     = set.take(2).map(_.id).mkString(", ")
          val membersFull = set.take(8).map(_.toString().box)

          s"[${canon}] = (${set.length}) ${members} ..." atop indent(2, vcat(left, membersFull))
        }

        vjoin(left, s"$l => ", indent(2, vjoinWith(left, vspace(1), setStrs)), "~" * 20)
      }

    val res = vjoin(indent(4, vcat(left, allSets)))

    println(res)
  }

}
