package org.watrworks
package rsearch

import utils.DoOrDieHandlers._
import com.github.davidmoten.rtree.{geometry => RG, _}
import geometry._

/**
  * Wrapper around Java-based R-Tree implementation, for better scala interop
  * Supports shapes based on watrmarks geometry types, with an optional associated attribute
  * Provides JSon serialization
  **/
class RTreeIndex[A <: GeometricFigure, Shape <: LabeledShape.Aux[A]](
  var spatialIndex: RTree[Shape, RG.Geometry]
) extends RTreeSearch[A, Shape] {
  import RGeometryConversions._

  override def rtreeIndex: RTree[Shape, RG.Geometry] = spatialIndex

  def clearAll(): Unit = {
    spatialIndex = RTree.create[Shape, RG.Geometry]()
  }

  def remove(item: Shape): Unit = {
    spatialIndex = spatialIndex.delete(
      item,
      toRGRectangle(item.minBounds)
    )
  }

  def add(item: Shape): Unit = {
    spatialIndex = spatialIndex.add(
      item,
      toRGRectangle(item.minBounds)
    )
  }

  def getItems(): Seq[Shape] = {
    toScalaSeq(spatialIndex.entries())
  }
}

object RTreeIndex {
  // import RGeometryConversions._

  def empty[A <: GeometricFigure, Shape <: LabeledShape.Aux[A]](): RTreeIndex[A, Shape] = {
    val init = RTree.create[Shape, RG.Geometry]()
    new RTreeIndex[A, Shape](init)
  }

  import _root_.io.circe
  import circe._
  import circe.syntax._
  import circe.literal._

  implicit def RTreeEncoder[
    A <: GeometricFigure,
    W,
    Shape <: LabeledShape.Aux[A] : Encoder
  ]: Encoder[RTreeIndex[A, Shape]] =
    Encoder.instance[RTreeIndex[A, Shape]]{ shapeIndex =>
      val shapes = shapeIndex.getItems()

      Json.obj(
        "shapes" := shapes.sortBy(_.id.unwrap)
      )
    }

  implicit def RTreeDecoder[
    A <: GeometricFigure,
    W,
    Shape <: LabeledShape.Aux[A] : Decoder
  ]: Decoder[RTreeIndex[A, Shape]] =
    Decoder.instance[RTreeIndex[A, Shape]]{ c =>

      val rtreeIndex = RTreeIndex.empty[A, Shape]()
      val shapeJson = c.downField("shapes").focus.orDie("no shapes field found")
      val shapes = shapeJson.decodeOrDie[List[Shape]]("Invalid shape list")
      shapes.foreach { shape =>
        rtreeIndex.add(shape)
      }

      Right(rtreeIndex)
    }

}
