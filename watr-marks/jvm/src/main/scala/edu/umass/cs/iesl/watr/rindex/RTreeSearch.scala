package edu.umass.cs.iesl.watr
package rindex

import RGeometryConversions._
import rx.Observable
import rx.functions.{
  Func1
  // , Func2
}

import scala.collection.JavaConverters._

import com.github.davidmoten.rtree.{geometry => RG, _}
import edu.umass.cs.iesl.watr.{geometry => G}
import java.lang.{Boolean => JBool}

abstract class RTreeSearch[T: RTreeIndexable] {

  def rtreeIndex: RTree[T, RG.Geometry]

  val si = implicitly[RTreeIndexable[T]]

  // Search TODO: open/closed intervals for search inclusion
  def searchLine(q: G.Line, filter: T=>Boolean): Seq[T] = {
    val query0 = toRGLine(q)
    val filterFunc = new Func1[Entry[T, RG.Geometry], JBool]() {
      override def call(entry: Entry[T, RG.Geometry]): JBool = {
        filter(entry.value())
      }
    }

    val hits = rtreeIndex.search(query0).filter(filterFunc)
    toScalaSeq(hits)
  }


  def search(q:G.LTBounds, filter: T=>Boolean): Seq[T] = {
    val query0 = toRGRectangle(q)
    val filterFunc = new Func1[Entry[T, RG.Geometry], JBool]() {
      override def call(entry: Entry[T, RG.Geometry]): JBool = {
        filter(entry.value())
      }
    }

    val hits = rtreeIndex.search(query0).filter(filterFunc)

    toScalaSeq(hits)

  }

  // def queryForContainedIDs(q:G.LTBounds): Seq[Int] = {

  //   val query0 = toRGRectangle(q)
  //   val x1 = query0.x1()
  //   val x2 = query0.x2()
  //   val y1 = query0.y1()
  //   val y2 = query0.y2()

  //   val queryCornerPts = List(
  //     RG.Geometries.point(x1, y1),
  //     RG.Geometries.point(x1, y2),
  //     RG.Geometries.point(x2, y1),
  //     RG.Geometries.point(x2, y2)
  //   )

  //   val contains: Func2[RG.Geometry, RG.Rectangle, JBool] = {
  //     new Func2[RG.Geometry, RG.Rectangle, JBool]() {
  //       override def call(entryRect: RG.Geometry, queryRect: RG.Rectangle): JBool = {
  //         queryCornerPts.forall(entryRect.intersects(_))
  //       }
  //     }
  //   }
  //   toIdSeq(rtreeIndex.search(query0, contains))
  // }

  def queryForIntersects(q: G.LTBounds): Seq[T] = {
    toScalaSeq(rtreeIndex.search(toRGRectangle(q)))
  }

  def queryForIntersectedIDs(q:G.LTBounds): Seq[Int] = {
    toEntrySeq(rtreeIndex.search(toRGRectangle(q)))
      .map{ entry => si.id(entry.value()) }
  }

  protected def toScalaSeq(obs: Observable[Entry[T, RG.Geometry]]): Seq[T]  = {
    toEntrySeq(obs).toSeq.map{ _.value() }
  }

  protected def toEntrySeq(obs: Observable[Entry[T, RG.Geometry]]): Seq[Entry[T, RG.Geometry]]  = {
    obs.toBlocking().toIterable().asScala.toSeq
  }

  protected def toIdSeq(obs: Observable[Entry[T, RG.Geometry]]): Seq[Int]  = {
    toEntrySeq(obs).map{ entry => si.id(entry.value()) }
  }
}
