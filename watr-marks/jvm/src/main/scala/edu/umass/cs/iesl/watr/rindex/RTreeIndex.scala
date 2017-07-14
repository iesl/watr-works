package edu.umass.cs.iesl.watr
package rindex

import _root_.edu.umass.cs.iesl.watr.{geometry => G}

import scala.collection.mutable
import com.github.davidmoten.rtree
import rtree._
import rtree.{geometry => RG}
import scala.collection.JavaConverters._
import scala.collection.JavaConverters

trait RTreeIndexable[T] {
  def id(t: T): Int
  def ltBounds(t: T): G.LTBounds
}

class RTreeIndex[T: RTreeIndexable](
  initItems: Seq[T]
) {
  import G._

  val si = implicitly[RTreeIndexable[T]]

  val itemMap: mutable.LongMap[T] = mutable.LongMap[T]()

  var spatialIndex: RTree[T, RG.Geometry] = {
    val entries: Seq[rtree.Entry[T, RG.Geometry]] = initItems.map{ t =>
      rtree.Entries.entry(t,
        RGeometry.rectangle(si.ltBounds(t)).asInstanceOf[RG.Geometry]
      )
    }
    val asJava = JavaConverters.asJavaCollectionConverter(entries)
    val l = new java.util.ArrayList(asJava.asJavaCollection)

    RTree.create[T, RG.Geometry](l)
  }


  def toJsiRectangle(tb: LTBounds): RG.Rectangle = {
    val LTBounds.Floats(l, t, w, h) = tb
    RGeometry.rectangle(l, t, w, h)
  }


  def remove(item: T): Unit = {
    spatialIndex = spatialIndex.delete(
      item,
      toJsiRectangle(si.ltBounds(item))
    )
    itemMap.remove(si.id(item).toLong)
  }

  def add(item: T): Unit = {
    spatialIndex = spatialIndex.add(
      item,
      toJsiRectangle(si.ltBounds(item))
    )
    itemMap.put(si.id(item).toLong, item)
  }

  def getItems(): Seq[T] = {
    itemMap.values.toSeq
  }

  def get(id: Int): Option[T] = {
    itemMap.get(id.toLong)
  }

  def getItem(id: Int): T = {
    itemMap(id.toLong)
  }


  def queryForIntersectedIDs(q:LTBounds): Seq[Int] = {
    spatialIndex.search(toJsiRectangle(q))
      .toBlocking().toIterable().asScala
      .toSeq.map{ entry =>
        si.id(entry.value())
      }
  }

  import java.lang.{Boolean => JBool}
  import rx.functions.Func2
  import rx.functions.Func1

  // def search(q:LTBounds, filter: (T, LTBounds)=>Boolean): Seq[T] = {
  def search(q:LTBounds, filter: T=>Boolean): Seq[T] = {
    val query0 = toJsiRectangle(q)
    val filterFunc = new Func1[Entry[T, RG.Geometry], JBool]() {
      override def call(entry: Entry[T, RG.Geometry]): JBool = {
        filter(entry.value())
      }
    }

    val hits = spatialIndex.search(query0).filter(filterFunc)

    hits.toBlocking()
      .toIterable().asScala
      .toSeq.map{ _.value() }

  }
  def queryForContainedIDs(q:LTBounds): Seq[Int] = {

    val query0 = toJsiRectangle(q)
    val x1 = query0.x1()
    val x2 = query0.x2()
    val y1 = query0.y1()
    val y2 = query0.y2()
    val p1 = RG.Geometries.point(x1, y1)
    val p2 = RG.Geometries.point(x1, y2)
    val p3 = RG.Geometries.point(x2, y1)
    val p4 = RG.Geometries.point(x2, y2)


    val contains: Func2[RG.Geometry, RG.Rectangle, JBool] = {
      new Func2[RG.Geometry, RG.Rectangle, JBool]() {
        override def call(g: RG.Geometry, h: RG.Rectangle): JBool = {
          (g.intersects(p1)
            && g.intersects(p2)
            && g.intersects(p3)
            && g.intersects(p4)
          )
        }
      }
    }


    spatialIndex.search(query0, contains)
      .toBlocking().toIterable().asScala
      .toSeq.map{ entry =>
        si.id(entry.value())
      }
  }

  def queryForIntersects(q: LTBounds): Seq[T] = {
    queryForIntersectedIDs(q).map(cid => itemMap(cid.toLong))
  }

  def queryForContained(q: LTBounds): Seq[T] = {
    queryForContainedIDs(q).map(cid => itemMap(cid.toLong))
  }

}

object RTreeIndex {

  def createFor[T : RTreeIndexable](
    initItems: Seq[T] = Seq()
  ): RTreeIndex[T] = {
    new RTreeIndex[T](initItems)
  }

}


object RGeometry {
  def rectangle(
    x: Double, y: Double, width: Double, height: Double
  ): RG.Rectangle = rectangle(
    x.toFloat, y.toFloat, width.toFloat, height.toFloat
  )

  def rectangle(
    x: Float, y: Float, width: Float, height: Float
  ): RG.Rectangle = {
    RG.Geometries.rectangle(
      x, y, x+width, y+height
    )
  }

  def rectangle(ltBounds: G.LTBounds): RG.Rectangle = {
    val G.LTBounds.Floats(l, t, w, h) = ltBounds
    RG.Geometries.rectangle(
      l, t, l+w, t+h
    )
  }

  def toLTBounds(r: RG.Rectangle): G.LTBounds = {
    G.LTBounds.Floats(
      left = r.x1,
      top =  r.y1,
      width = (r.x2 - r.x1),
      height = (r.y2 - r.y1)
    )
  }
}
