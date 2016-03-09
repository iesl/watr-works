package edu.umass.cs.iesl.watr
package watrmarks

import spatialindex.{SpatialIndex, regions}
import scala.collection.mutable

// case class SpatialPageInfo(
//   sindex: SpatialIndex = SpatialIndex.create(),
//   charRegions: mutable.Map[Long, Char] = mutable.Map(),
//   pageBounds: watrmarks.TextBounds
// ) {
//   var _nextId = 0L
//   def nextId = {
//     _nextId += 1
//     _nextId
//   }

//   val labels = mutable.Map[Long, String]()

//   // def insert(ch: Char, x: Double, y: Double, w: Double, h: Double): Unit = {
//   //   val nid = nextId
//   //   sindex.insert(nid, regions.bbox(x=x , y=y, w=w, h=h))
//   //   charRegions(nid) = ch
//   // }

//   // def labelBbox(bbox: spatialindex.Bounds, label: String): Unit = {
//   //   val nid = nextId
//   //   val data = sindex.insert(nid, bbox)
//   //   val _ = labels.put(nid, label)
//   // }

// }

trait PageSpatialInfo {
  def sindex: spatialindex.SpatialIndex = SpatialIndex.create()

  var _nextId = 0L
  def nextId = {
    _nextId += 1
    _nextId
  }

  def addBoundingBox(bbox: spatialindex.Bounds, label: String): Unit = {
    val nid = nextId
    // sindex.insert(nid, regions.bbox(x=x , y=y, w=w, h=h))
    sindex.insert(nid, bbox)

    ???
  }

  def getBoundingBoxes(): Seq[spatialindex.Bounds] = {

    ???
  }

}
object PageSpatialInfo {
  def apply(): PageSpatialInfo = new PageSpatialInfo {


  }
}




