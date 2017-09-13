package edu.umass.cs.iesl.watr
package rindex

import java.lang.{Boolean => JBool}
import rx.Observable

import scala.collection.JavaConverters
import scala.collection.JavaConverters._
import scala.collection.mutable

import com.github.davidmoten.rtree.{geometry => RG, _}
import com.github.davidmoten.rtree
import edu.umass.cs.iesl.watr.{geometry => G}
import rx.functions.{Func1, Func2}

class RTreeIndex[T: RTreeIndexable](
  var spatialIndex: RTree[T, RG.Geometry]
) {
  import RGeometryConversions._

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
      toRGRectangle(si.ltBounds(item))
    )

    itemMap -= si.id(item).toLong
  }

  def add(item: T): Unit = {
    spatialIndex = spatialIndex.add(
      item,
      toRGRectangle(si.ltBounds(item))
    )
    itemMap += (si.id(item).toLong, item)
  }

  def getItems(): Seq[T] = {
    toScalaSeq(spatialIndex.entries())
  }


  // Search TODO: open/closed intervals for search inclusion
  def searchLine(q: G.Line, filter: T=>Boolean): Seq[T] = {
    val query0 = toRGLine(q)
    val filterFunc = new Func1[Entry[T, RG.Geometry], JBool]() {
      override def call(entry: Entry[T, RG.Geometry]): JBool = {
        filter(entry.value())
      }
    }

    val hits = spatialIndex.search(query0).filter(filterFunc)
    toScalaSeq(hits)
  }


  def search(q:G.LTBounds, filter: T=>Boolean): Seq[T] = {
    val query0 = toRGRectangle(q)
    val filterFunc = new Func1[Entry[T, RG.Geometry], JBool]() {
      override def call(entry: Entry[T, RG.Geometry]): JBool = {
        filter(entry.value())
      }
    }

    val hits = spatialIndex.search(query0).filter(filterFunc)

    toScalaSeq(hits)

  }


  def queryForContainedIDs(q:G.LTBounds): Seq[Int] = {

    val query0 = toRGRectangle(q)
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

  def queryForIntersectedIDs(q:G.LTBounds): Seq[Int] = {
    toEntrySeq(spatialIndex.search(toRGRectangle(q)))
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

  def queryForIntersects(q: G.LTBounds): Seq[T] = {
    toScalaSeq(spatialIndex.search(toRGRectangle(q)))
  }

}

object RTreeIndex {
  import RGeometryConversions._

  def createFor[T : RTreeIndexable](initItems: Seq[T] = Seq()): RTreeIndex[T] = {
    val si = implicitly[RTreeIndexable[T]]

    val entries: Seq[rtree.Entry[T, RG.Geometry]] = initItems.map{ t =>
      rtree.Entries.entry(t,
        toRGRectangle(si.ltBounds(t)).asInstanceOf[RG.Geometry]
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
  import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

  def load[T: RTreeIndexable](path: Path): RTreeIndex[T] = {
    val ser = createRTreeSerializer[T]
    val byteArray = Files.readAllBytes(path)
    val bais = new ByteArrayInputStream(byteArray)
    val rt  = ser.read(bais, byteArray.length.toLong, rtree.InternalStructure.DEFAULT)
    createFor(rt)
  }

  def saveBytes[T: RTreeIndexable](rtree: RTreeIndex[T]): Array[Byte] = {
    val ser = createRTreeSerializer[T]
    val baos = new ByteArrayOutputStream()
    ser.write(rtree.spatialIndex, baos)
    baos.toByteArray()
  }
}


