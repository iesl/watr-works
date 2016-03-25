package edu.umass.cs.iesl.watr
package watrmarks

import net.sf.jsi.Rectangle
import scala.collection.mutable

// import gnu.trove.procedure.TIntProcedure;

import net.sf.jsi.SpatialIndex
import net.sf.jsi.rtree.RTree
import net.sf.jsi.Rectangle

case class Annotation(
  id: String,
  target: String,
  bboxes: Seq[Rectangle]
)

// id: String,
// target: String,

// case class BoundingBox(
//   x: Double, y: Double, width: Double, height: Double
// ) {

//   def union(b: BoundingBox): BoundingBox = {
//     val minx = math.min(x, b.x)
//     val miny = math.min(y, b.y)

//     val maxx = math.max(x+width, b.x+b.width)
//     val maxy = math.max(y+height, b.y+b.height)

//     // BoundingBox(id, target, minx, maxx-minx, miny, maxy-miny)
//     BoundingBox(minx, maxx-minx, miny, maxy-miny)

//   }

// }

trait UniqIds {
  def startingId = 0l

  var _nextId = 0L
  def nextId = {
    _nextId += 1
    _nextId
  }
}


trait PageSpatialInfo extends UniqIds {
  // import PageSpatialInfo._

  def sindex: SpatialIndex = new RTree()

  val annotationsById = mutable.HashMap[Long, watrmarks.Annotation]()

  def extents: Rectangle


  def addAnnotation(annot: Annotation): Unit = {
    val nid = nextId
    annotationsById.put(nid, annot)
    annot.bboxes.foreach{bbox =>
      sindex.add(bbox, 0)// TODO add id#
    }
  }

  def getAnnotations: Seq[Annotation] = {
    annotationsById.map(_._2).toSeq
  }
}



object PageSpatialInfo {
  def minMaxPairToPointWidth(minMax: (Double, Double)): (Double, Double) = {
    (minMax._1, minMax._2-minMax._1)
  }

  // public Rectangle(float x1, float y1, float x2, float y2) {
  def rect(
    x: Float, y: Float, width: Float, height: Float
  ): Rectangle = new Rectangle(
    x, y, x+width, y+height
  )
  // implicit class RicherBounds(val bb: Rectangle) extends AnyVal {
  //   def mins = bb.minMaxPairs.map(_._1).toArray
  //   def maxs = bb.minMaxPairs.map(_._2).toArray

  //   def toBoundingBox: BoundingBox = {
  //     val pws = bb.minMaxPairs.map(minMaxPairToPointWidth(_))
  //     BoundingBox(
  //       x=pws(0)._1, width=pws(0)._2,
  //       y=pws(1)._1, height=pws(1)._2
  //     )
  //   }
  // }

  // implicit class RicherBoundingBox(val bb: BoundingBox) extends AnyVal {
  //   def toBounds: spatialindex.Bounds = {
  //     spatialindex.regions.bbox(bb.x, bb.y, bb.width, bb.height)
  //   }
  // }


  def apply(ext: Rectangle): PageSpatialInfo = new PageSpatialInfo {
    override val extents = ext
  }


  def vconcat(psis: PageSpatialInfo*): PageSpatialInfo = {
    psis.reduce { (p1, p2) =>

      val p12extents = p1.extents.union(p2.extents)

      val psi = PageSpatialInfo(p12extents)

      p1.getAnnotations.foreach { a =>
        psi.addAnnotation(a)
      }

      psi
    }

    val totalExtents = psis.map(_.extents).reduce { (bb1, bb2) =>
      bb1
    }


    ???
  }
}
