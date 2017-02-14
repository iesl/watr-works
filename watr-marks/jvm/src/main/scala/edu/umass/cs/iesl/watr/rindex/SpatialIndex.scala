package edu.umass.cs.iesl.watr
package rindex

import net.sf.jsi
import net.sf.jsi.rtree.RTree

import scala.collection.mutable

import geometry._
import GeometryImplicits._


trait SpatialIndexable[T] {
  def id(t: T): Int
  def ltBounds(t: T): LTBounds
}

class SpatialIndex[T: SpatialIndexable](
  // bounds: LTBounds,
  spatialIndex: jsi.SpatialIndex,
  items: mutable.LongMap[T]
) {
  def toJsiRectangle(tb: LTBounds): jsi.Rectangle = {
    jsiRectangle(tb.left, tb.top, tb.width, tb.height)
  }

  def jsiCenterPoint(tb: LTBounds): jsi.Point = {
    new jsi.Point(
      (tb.left+tb.width/2).toFloat,
      (tb.top+tb.height/2).toFloat
    )
  }


  def itemMap: mutable.LongMap[T] = items

  def remove(item: T): Unit = {
    val si = implicitly[SpatialIndexable[T]]
    spatialIndex.delete(
      toJsiRectangle(si.ltBounds(item)),
      si.id(item)
    )
    itemMap.remove(si.id(item).toLong)
  }

  def add(item: T): Unit = {
    val si = implicitly[SpatialIndexable[T]]
    spatialIndex.add(
      toJsiRectangle(si.ltBounds(item)),
      si.id(item)
    )
    items.put(si.id(item).toLong, item)
  }

  def getItems(): Seq[T] = {
    items.values.toSeq
  }

  def get(id: Int): Option[T] = {
    items.get(id.toLong)
  }

  def getItem(id: Int): T = {
    items(id.toLong)
  }

  def queryForIntersectedIDs(q:LTBounds): Seq[Int] = {
    val collectRegions = SpatialIndex.rtreeIdCollector()
    spatialIndex.intersects(toJsiRectangle(q), collectRegions)
    collectRegions.getIDs
  }


  def queryForContainedIDs(q:LTBounds): Seq[Int] = {
    val collectRegions = SpatialIndex.rtreeIdCollector()
    spatialIndex.contains(toJsiRectangle(q), collectRegions)
    collectRegions.getIDs
  }

  def queryForIntersects(q: LTBounds): Seq[T] = {
    queryForIntersectedIDs(q).map(cid =>items(cid.toLong))
  }

  def queryForContained(q: LTBounds): Seq[T] = {
    queryForContainedIDs(q).map(cid =>items(cid.toLong))
  }

  def nearestNItems(fromItem: T, n: Int, radius: Float): Seq[T] = {
    val si = implicitly[SpatialIndexable[T]]

    val ctr = si.ltBounds(fromItem).toCenterPoint

    val searchRect = LTBounds(
      left   = ctr.x - radius,
      top    = ctr.y - radius,
      width  = (radius*2.0).toDouble,
      height = (radius*2.0).toDouble
    )

    queryForIntersects(searchRect)
      .filterNot(itm => si.id(itm) == si.id(fromItem))
      .sortBy({itm => ctr.dist( si.ltBounds(itm).toCenterPoint )})
      .take(n)
  }

}

object jsiRectangle {
  def apply(
    x: Double, y: Double, width: Double, height: Double
  ): jsi.Rectangle = apply(
    x.toFloat, y.toFloat, width.toFloat, height.toFloat
  )

  def apply(
    x: Float, y: Float, width: Float, height: Float
  ): jsi.Rectangle = new jsi.Rectangle(
    x, y, x+width, y+height
  )

  def toLTBounds(r: jsi.Rectangle): LTBounds = {
    LTBounds(
      left = r.minX.toDouble,
      top =  r.minY.toDouble,
      width = (r.maxX - r.minX).toDouble,
      height = (r.maxY - r.minY).toDouble
    )
  }
}

object SpatialIndex {


  def createSpatialIndex(): jsi.SpatialIndex = {
    val rtree: jsi.SpatialIndex = new RTree()
    rtree.init(null)
    rtree
  }

  def createFor[T : SpatialIndexable](): SpatialIndex[T] = {
    new SpatialIndex[T](
      // bounds,
      createSpatialIndex(),
      mutable.LongMap[T]()
    )
  }

  import gnu.trove.procedure.TIntProcedure
  class CollectRegionIds extends TIntProcedure {
    import scala.collection.mutable
    val ids = mutable.ArrayBuffer[Int]()

    override def execute(id: Int): Boolean = {
      ids.append(id)
      true
    }

    def getIDs: Seq[Int] = ids
  }

  def rtreeIdCollector(): CollectRegionIds = {
    new CollectRegionIds()
  }
}
