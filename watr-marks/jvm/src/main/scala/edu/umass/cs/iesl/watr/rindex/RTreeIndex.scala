package edu.umass.cs.iesl.watr
package rindex


import scala.collection.JavaConverters

import com.github.davidmoten.rtree.{geometry => RG, _}
import com.github.davidmoten.rtree
import rx.functions.Func1

/**
  * + R-Tree index supporting shapes: rects, lines, points
  * + Arbitrary attributes may be attached to shapes
  * + Shapes can be clustered via disjoint sets
  * + Search:
  *   - Open/closed interval search support inclusion/exclusion of shapes that share an edge, but have no common area
  *
  * + Serialization
  *   - Trees may be written to disk
  *
  **/

class RTreeIndex[T: RTreeIndexable](
  var spatialIndex: RTree[T, RG.Geometry]
) extends RTreeSearch[T] {
  import RGeometryConversions._

  override val indexable: RTreeIndexable[T] = implicitly[RTreeIndexable[T]]

  override def rtreeIndex: RTree[T, RG.Geometry] = spatialIndex

  def clearAll(): Unit = {
    spatialIndex = RTree.create[T, RG.Geometry]()
  }

  def remove(item: T): Unit = {
    spatialIndex = spatialIndex.delete(
      item,
      toRGRectangle(si.ltBounds(item))
    )

  }

  def add(item: T): Unit = {
    spatialIndex = spatialIndex.add(
      item,
      toRGRectangle(si.ltBounds(item))
    )
  }

  def getItems(): Seq[T] = {
    toScalaSeq(spatialIndex.entries())
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
    createFor[T](RTree.create[T, RG.Geometry](l))
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
