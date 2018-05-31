package edu.umass.cs.iesl.watr
package rtrees

import scala.collection.JavaConverters

import com.github.davidmoten.rtree.{geometry => RG, _}
import com.github.davidmoten.rtree
import rx.functions.Func1
import geometry._
import geometry.syntax._

/**
  *
  **/

// class RTreeIndex[T: RTreeIndexable](
class RTreeIndex[T <: GeometricFigure, W, Shape <: LabeledShape.Aux[T, W]](
  var spatialIndex: RTree[Shape, RG.Geometry]
) extends RTreeSearch[T, W, Shape] {
  import RGeometryConversions._

  // override val indexable: RTreeIndexable[T] = implicitly[RTreeIndexable[T]]

  override def rtreeIndex: RTree[Shape, RG.Geometry] = spatialIndex

  def clearAll(): Unit = {
    spatialIndex = RTree.create[Shape, RG.Geometry]()
  }

  def remove(item: Shape): Unit = {
    spatialIndex = spatialIndex.delete(
      item,
      toRGRectangle(item.bounds)
    )

  }

  def add(item: Shape): Unit = {
    spatialIndex = spatialIndex.add(
      item,
      toRGRectangle(item.bounds)
    )
  }

  def getItems(): Seq[Shape] = {
    toScalaSeq(spatialIndex.entries())
  }

}

object RTreeIndex {
  import RGeometryConversions._

  def empty[T <: GeometricFigure, W, Shape <: LabeledShape.Aux[T, W]](): RTreeIndex[T, W, Shape] = {
    val init = RTree.create[Shape, RG.Geometry]()
    new RTreeIndex[T, W, Shape](init)
  }

  // def createFor[T <: GeometricFigure, W](initItems: Seq[T]): RTreeIndex[T] = {
  // def createFor[T <: GeometricFigure, W](): RTreeIndex[T] = {
  //   // val si = implicitly[RTreeIndexable[T]]

  //   val entries: Seq[rtree.Entry[T, RG.Geometry]] = initItems.map{ t =>
  //     rtree.Entries.entry(t,
  //       // toRGRectangle(si.ltBounds(t)).asInstanceOf[RG.Geometry]
  //         toRGRectangle(minBoundingRect(t)).asInstanceOf[RG.Geometry]
  //     )
  //   }
  //   val asJava = JavaConverters.asJavaCollectionConverter(entries)
  //   val l = new java.util.ArrayList(asJava.asJavaCollection)
  //   createFor[T](RTree.create[T, RG.Geometry](l))
  // }

  // def createFor[T](initRtree: RTree[T, RG.Geometry]): RTreeIndex[T] = {
  //   new RTreeIndex[T](initRtree)
  // }


  // def createRTreeSerializer[T : RTreeIndexable](): Serializer[T, RG.Geometry] = {

  //   val si = implicitly[RTreeIndexable[T]]

  //   val ccSerializer = new Func1[T, Array[Byte]]() {
  //     override def call(entry: T): Array[Byte] = {
  //       si.serialize(entry)
  //     }
  //   }

  //   val ccDeSerializer = new Func1[Array[Byte], T]() {
  //     override def call(bytes: Array[Byte]): T = {
  //       si.deserialize(bytes)
  //     }
  //   }

  //   val ser: Serializer[T, RG.Geometry] = Serializers
  //     .flatBuffers[T, RG.Geometry]()
  //     .serializer(ccSerializer)
  //     .deserializer(ccDeSerializer)
  //     .create()

  //   ser
  // }


  // import java.nio.file.Files
  // import java.nio.file.Path
  // import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

  // def load[T: RTreeIndexable](path: Path): RTreeIndex[T] = {
  //   val ser = createRTreeSerializer[T]
  //   val byteArray = Files.readAllBytes(path)
  //   val bais = new ByteArrayInputStream(byteArray)
  //   val rt  = ser.read(bais, byteArray.length.toLong, rtree.InternalStructure.DEFAULT)
  //   createFor(rt)
  // }

  // def saveBytes[T: RTreeIndexable](rtree: RTreeIndex[T]): Array[Byte] = {
  //   val ser = createRTreeSerializer[T]
  //   val baos = new ByteArrayOutputStream()
  //   ser.write(rtree.spatialIndex, baos)
  //   baos.toByteArray()
  // }
}
