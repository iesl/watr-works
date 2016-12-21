package edu.umass.cs.iesl.watr
package spindex

import net.sf.jsi
import net.sf.jsi.rtree.RTree

import scala.collection.mutable

import TypeTags._
import geometry._
import GeometricFigure._
import EnrichGeometricFigures._


trait SpatialIndexable[T] {
  def id(t: T): Int
  def ltBounds(t: T): LTBounds
}

class SpatialIndex[T: SpatialIndexable](
  bounds: LTBounds,
  spatialIndex: jsi.SpatialIndex,
  items: mutable.LongMap[T]
) {

  def itemMap: mutable.LongMap[T] = items

  def remove(item: T): Unit = {
    val si = implicitly[SpatialIndexable[T]]
    spatialIndex.delete(
      si.ltBounds(item).toJsiRectangle,
      si.id(item)
    )
    itemMap.remove(si.id(item).toLong)
  }

  def add(item: T): Unit = {
    val si = implicitly[SpatialIndexable[T]]
    spatialIndex.add(
      si.ltBounds(item).toJsiRectangle,
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
    spatialIndex.intersects(q.toJsiRectangle, collectRegions)
    collectRegions.getIDs
  }


  def queryForContainedIDs(q:LTBounds): Seq[Int] = {
    val collectRegions = SpatialIndex.rtreeIdCollector()
    spatialIndex.contains(q.toJsiRectangle, collectRegions)
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
      left   =ctr.x - radius,
      top    =ctr.y - radius,
      width  =(radius*2.0).toDouble,
      height =(radius*2.0).toDouble
    )

    queryForIntersects(searchRect)
      .filterNot(itm => si.id(itm) == si.id(fromItem))
      .sortBy({itm => ctr.dist( si.ltBounds(itm).toCenterPoint )})
      .take(n)
  }

}

object SpatialIndex {

  implicit object ComponentIndexable extends SpatialIndexable[Component] {
    def id(t: Component): Int = t.id.unwrap
    def ltBounds(t: Component): LTBounds = t.bounds
  }

  implicit object CharAtomIndexable extends SpatialIndexable[CharAtom] {
    def id(t: CharAtom): Int = t.targetRegion.id.unwrap
    def ltBounds(t: CharAtom): LTBounds = t.targetRegion.bbox
  }

  def createSpatialIndex(): jsi.SpatialIndex = {
    val rtree: jsi.SpatialIndex = new RTree()
    rtree.init(null)
    rtree
  }

  def createFor[T : SpatialIndexable](bounds: LTBounds): SpatialIndex[T] = {
    new SpatialIndex[T](
      bounds,
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
