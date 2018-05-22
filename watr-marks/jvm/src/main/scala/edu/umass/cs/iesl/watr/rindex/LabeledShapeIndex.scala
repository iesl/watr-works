package edu.umass.cs.iesl.watr
package rtrees

import scala.collection.mutable

import watrmarks._
import geometry._
import geometry.syntax._

import rtrees._
import utils.OrderedDisjointSet

import utils.ExactFloats._
import textboxing.{TextBoxing => TB}
import scala.reflect.ClassTag
import com.google.{common => guava}
import guava.{collect => gcol}
import com.github.davidmoten.rtree.{geometry => RG}


/**
  *
  **/



trait LabeledShape[+T <: GeometricFigure, W] {
  def id: Int@@ShapeID
  def shape: T
  def labels(): Set[Label]
  def attr: W

  def hasLabel(l: Label) = labels.exists(_ == l)
  def addLabels(l: Label*): LabeledShape[T, W]

  private var _isIndexed: Boolean = false
  def isIndexed(): Boolean = _isIndexed
  def setIndexed(b: Boolean): Unit = _isIndexed = b

  val rtreeGeometry: RG.Geometry =
    RGeometryConversions.geometricFigureToRtreeGeometry(shape)

  val bounds: LTBounds = minBoundingRect(shape)

}


object LabeledShape {
  type Aux[T <: GeometricFigure, W] = LabeledShape[T, W]
}

object LabeledShapeIndex {

  def qualifyCluster(l: Label): Label = { l.qualifiedAs("cluster") }
  def qualifyOrdering(l: Label): Label = { l.qualifiedAs("ordering") }
  def qualifyRelation(l: Label): Label = { l.qualifiedAs("relation") }
  def qualifyRep(l: Label): Label = { l.qualifiedAs("rep") }

  // type Shape[A <: GeometricFigure] = LabeledShape[A, W]
}

class LabeledShapeIndex[A <: GeometricFigure, W, Shape <: LabeledShape.Aux[A, W]] {

  import LabeledShapeIndex._

  type LineShape  = LabeledShape[Line, W]
  type PointShape = LabeledShape[Point, W]
  type RectShape  = LabeledShape[LTBounds, W]
  type AnyShape   = Shape

  // implicit class RicherLabeledShape(val theShape: Shape) {
  //   def asLineShape: LineShape = theShape.asInstanceOf[LineShape]
  //   def asPointShape: PointShape = theShape.asInstanceOf[PointShape]
  //   def asRectShape: RectShape = theShape.asInstanceOf[RectShape]
  // }

  val shapeMap: mutable.LongMap[Shape] = {
    mutable.LongMap[Shape]()
  }

  def getAllShapes(): Seq[Shape] = {
    shapeMap.values.toSeq
  }

  def getById(id: Int@@ShapeID): Shape = {
    shapeMap(id.unwrap.toLong)
  }

  val shapeIDGen = utils.IdGenerator[ShapeID]()
  val shapeRIndex: RTreeIndex[A, W, Shape] = RTreeIndex.empty[A, W, Shape]()
  // val shapeRIndex: RTreeIndex[A, W, Shape] = RTreeIndex.empty[A, W, LabeledShape.Aux[A, W]]()

  def indexShape(f: Int@@ShapeID => Shape): Shape = {
    val lshape = f(shapeIDGen.nextId)
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


  val disjointSets: mutable.HashMap[Label, OrderedDisjointSet[Shape]] = mutable.HashMap()

  def ensureCluster[T <: GeometricFigure](l: Label): OrderedDisjointSet[Shape] = {
    disjointSets.getOrElseUpdate(l,
      OrderedDisjointSet.apply[Shape]()
    )
  }

  def addCluster[T <: GeometricFigure](l: Label, shapes: Seq[Shape]): Shape = {
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
    l: Label, cs: Seq[Shape]
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

  val shapeAttributeTable = gcol.HashBasedTable.create[Int@@ShapeID, Label, Any]()

  def setShapeAttribute[C](shapeId: Int@@ShapeID, l: Label, attr: C)
    (implicit act: ClassTag[C]): Unit = {
    val typedLabel = l.qualifiedAs(act.runtimeClass.getSimpleName)
    shapeAttributeTable.put(shapeId, typedLabel, attr)
  }

  def getShapeAttribute[C](shapeId: Int@@ShapeID, l: Label)
    (implicit act: ClassTag[C]): Option[C] = {
    val typedLabel = l.qualifiedAs(act.runtimeClass.getSimpleName)
    if (shapeAttributeTable.contains(shapeId, typedLabel)) {
      Some(shapeAttributeTable.get(shapeId, typedLabel).asInstanceOf[C])
    } else None
  }


}
