package org.watrworks
package rsearch

import RGeometryConversions._
import rx.Observable
import rx.functions.{Func1}

import scala.jdk.CollectionConverters._

import com.github.davidmoten.rtree.{geometry => RG, _}
import org.watrworks.{geometry => G}
import java.lang.{Boolean => JBool}

import geometry._

trait RTreeSearch[T <: GeometricFigure, Shape <: LabeledShape[T]] {

  def rtreeIndex: RTree[Shape, RG.Geometry]

  def search(
    queryFig: G.GeometricFigure,
    filter: Shape => Boolean,
    intersectFunc: (RG.Geometry, RG.Geometry) => Boolean = (_, _) => true
  ): Seq[Shape] = {
    val filterFunc = new Func1[Entry[Shape, RG.Geometry], JBool]() {
      override def call(entry: Entry[Shape, RG.Geometry]): JBool = {
        filter(entry.value())
      }
    }

    val hits0 = queryFig match {
      case f: G.Rect => rtreeIndex.search(toRGRectangle(f))
      case f: G.Line     => rtreeIndex.search(toRGLine(f))
      case f: G.Point    => rtreeIndex.search(toRGPoint(f))
      case _             => sys.error("unsupported query shape")
    }

    val hits = hits0.filter(filterFunc)

    toScalaSeq(hits)
  }

  // def queryForIntersects(q: G.Rect): Seq[Shape] = {
  //   toScalaSeq(rtreeIndex.search(toRGRectangle(q)))
  // }

  // def queryForIntersectedIDs(q: G.Rect): Seq[Int] = {
  //   toEntrySeq(rtreeIndex.search(toRGRectangle(q)))
  //     .map { entry => entry.value().id.unwrap }
  // }

  protected def toScalaSeq(obs: Observable[Entry[Shape, RG.Geometry]]): Seq[Shape] = {
    toEntrySeq(obs).toSeq.map { _.value() }
  }

  protected def toEntrySeq(
    obs: Observable[Entry[Shape, RG.Geometry]]
  ): Seq[Entry[Shape, RG.Geometry]] = {
    obs.toBlocking().toIterable().asScala.toSeq
  }

  protected def toIdSeq(obs: Observable[Entry[Shape, RG.Geometry]]): Seq[Int] = {
    toEntrySeq(obs).map { entry => entry.value().id.unwrap }
  }
}
