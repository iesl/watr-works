package edu.umass.cs.iesl.watr
package rindex

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.lang.{Boolean => JBool}
import rx.Observable

import scala.collection.JavaConverters
import scala.collection.JavaConverters._
import scala.collection.mutable

import com.github.davidmoten.rtree.{geometry => RG, _}
import com.github.davidmoten.rtree
import edu.umass.cs.iesl.watr.{geometry => G}
import rx.functions.{Func1, Func2}

trait RTreeIndexable[T] {
  def id(t: T): Int
  def ltBounds(t: T): G.LTBounds

  def serialize[C <: T](value: C): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close
    stream.toByteArray
  }

  def deserialize[C <: T](bytes: Array[Byte]): C = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close
    value.asInstanceOf[C]
  }
}

class RTreeIndex[T: RTreeIndexable](
  var spatialIndex: RTree[T, RG.Geometry]
) {
  import RGeometry._
  import edu.umass.cs.iesl.watr.geometry._

  val si = implicitly[RTreeIndexable[T]]

  val itemMap: mutable.LongMap[T] = {
    val initMap = toScalaSeq(spatialIndex.entries())
      .map { item => (si.id(item).toLong, item) }

    mutable.LongMap[T](initMap:_*)
  }

  def get(id: Int): Option[T] = {
    itemMap.get(id.toLong)
  }
  def getItem(id: Int): T = {
    itemMap(id.toLong)
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
    toScalaSeq(spatialIndex.entries())
  }


  def searchLine(q: Line, filter: T=>Boolean): Seq[T] = {
    val query0 = toRGLine(q)
    val filterFunc = new Func1[Entry[T, RG.Geometry], JBool]() {
      override def call(entry: Entry[T, RG.Geometry]): JBool = {
        filter(entry.value())
      }
    }

    val hits = spatialIndex.search(query0).filter(filterFunc)
    toScalaSeq(hits)
  }


  def search(q:LTBounds, filter: T=>Boolean): Seq[T] = {
    val query0 = toJsiRectangle(q)
    val filterFunc = new Func1[Entry[T, RG.Geometry], JBool]() {
      override def call(entry: Entry[T, RG.Geometry]): JBool = {
        filter(entry.value())
      }
    }

    val hits = spatialIndex.search(query0).filter(filterFunc)

    toScalaSeq(hits)

  }


  def queryForContainedIDs(q:LTBounds): Seq[Int] = {

    val query0 = toJsiRectangle(q)
    val x1 = query0.x1()
    val x2 = query0.x2()
    val y1 = query0.y1()
    val y2 = query0.y2()

    val queryCornerPts = List(
      RG.Geometries.point(x1, y1),
      RG.Geometries.point(x1, y2),
      RG.Geometries.point(x2, y1),
      RG.Geometries.point(x2, y2)

    )


    val contains: Func2[RG.Geometry, RG.Rectangle, JBool] = {
      new Func2[RG.Geometry, RG.Rectangle, JBool]() {
        override def call(entryRect: RG.Geometry, queryRect: RG.Rectangle): JBool = {
          queryCornerPts.forall(entryRect.intersects(_))
        }
      }
    }


    toIdSeq(spatialIndex.search(query0, contains))
  }

  def queryForIntersectedIDs(q:LTBounds): Seq[Int] = {
    toEntrySeq(spatialIndex.search(toJsiRectangle(q)))
      .map{ entry => si.id(entry.value()) }
  }

  private def toEntrySeq(obs: Observable[Entry[T, RG.Geometry]]): Seq[Entry[T, RG.Geometry]]  = {
    obs.toBlocking().toIterable().asScala.toSeq
  }

  private def toIdSeq(obs: Observable[Entry[T, RG.Geometry]]): Seq[Int]  = {
    toEntrySeq(obs).map{ entry => si.id(entry.value()) }
  }

  private def toScalaSeq(obs: Observable[Entry[T, RG.Geometry]]): Seq[T]  = {
    toEntrySeq(obs).toSeq.map{ _.value() }
  }

  def queryForIntersects(q: LTBounds): Seq[T] = {
    toScalaSeq(spatialIndex.search(toJsiRectangle(q)))
  }

}

object RTreeIndex {
  def createFor[T : RTreeIndexable](initItems: Seq[T] = Seq()): RTreeIndex[T] = {
    val si = implicitly[RTreeIndexable[T]]

    val entries: Seq[rtree.Entry[T, RG.Geometry]] = initItems.map{ t =>
      rtree.Entries.entry(t,
        RGeometry.rectangle(si.ltBounds(t)).asInstanceOf[RG.Geometry]
      )
    }
    val asJava = JavaConverters.asJavaCollectionConverter(entries)
    val l = new java.util.ArrayList(asJava.asJavaCollection)

    val spatialIndex: RTree[T, RG.Geometry] = RTree.create[T, RG.Geometry](l)

    new RTreeIndex[T](spatialIndex)
  }

  def createFor[T : RTreeIndexable](initRtree: RTree[T, RG.Geometry]): RTreeIndex[T] = {
    new RTreeIndex[T](initRtree)
  }


  def createRTreeSerializer[T : RTreeIndexable](): Serializer[T, RG.Geometry] = {

    val si = implicitly[RTreeIndexable[T]]

    val ccSerializer = new Func1[T, Array[Byte]]() {
      override def call(entry: T): Array[Byte] = {
        si.serialize(entry)
      }
    }

    val ccDeSerializer = new Func1[Array[Byte], T]() {
      override def call(bytes: Array[Byte]): T = {
        si.deserialize(bytes)
      }
    }

    val ser: Serializer[T, RG.Geometry] = Serializers
      .flatBuffers[T, RG.Geometry]()
      .serializer(ccSerializer)
      .deserializer(ccDeSerializer)
      .create()

    ser
  }


  import java.nio.file.Files
  import java.nio.file.Path

  def load[T: RTreeIndexable](path: Path): RTreeIndex[T] = {
    val ser = createRTreeSerializer[T]
    val byteArray = Files.readAllBytes(path)
    val bais = new ByteArrayInputStream(byteArray)
    val rt  = ser.read(bais, byteArray.length.toLong, rtree.InternalStructure.DEFAULT)
    createFor(rt)
  }

  def saveBytes[T: RTreeIndexable](path: Path, rtree: RTreeIndex[T]): Array[Byte] = {
    val ser = createRTreeSerializer[T]
    val baos = new java.io.ByteArrayOutputStream()
    ser.write(rtree.spatialIndex, baos)
    baos.toByteArray()
  }
}


object RGeometry {
  import edu.umass.cs.iesl.watr.geometry._

  def toJsiRectangle(tb: LTBounds): RG.Rectangle = {
    val LTBounds.Floats(l, t, w, h) = tb
    RGeometry.rectangle(l, t, w, h)
  }

  def toRGLine(tb: Line): RG.Line = {
    val Line(Point.Doubles(x1, y1), Point.Doubles(x2, y2)) = tb
    RG.Geometries.line(x1, y1, x2, y2)
  }

  def toRGPoint(tb: Point): RG.Point = {
    val Point.Doubles(x1, y1) = tb
    RG.Geometries.point(x1, y1)
  }

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
