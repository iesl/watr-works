package edu.umass.cs.iesl.watr
package watrmarks

import spatialindex.{SpatialIndex}
import scala.collection.mutable


case class Annotation(
  id: String,
  target: String,
  // labelName: BioLabel,
  bboxes: Seq[BoundingBox]
)

case class BoundingBox(
  id: String,
  target: String,
  x: Double, y: Double, width: Double, height: Double
) {
  def union(b: BoundingBox): BoundingBox = {

    val minx = math.min(x, b.x)
    val miny = math.min(y, b.y)

    val maxx = math.max(x+width, b.x+b.width)
    val maxy = math.max(y+height, b.y+b.height)

    BoundingBox(id, target, minx, maxx-minx, miny, maxy-miny)

  }

}

trait UniqIds {
  def startingId = 0l

  var _nextId = 0L
  def nextId = {
    _nextId += 1
    _nextId
  }
}



trait PageSpatialInfo extends UniqIds {
  import PageSpatialInfo._

  def sindex: spatialindex.SpatialIndex = SpatialIndex.create()

  val annotationsById = mutable.HashMap[Long, watrmarks.Annotation]()

  def extents: BoundingBox


  def addAnnotation(annot: Annotation): Unit = {
    val nid = nextId
    annotationsById.put(nid, annot)
    annot.bboxes.foreach{bbox =>
      sindex.insert(nid, bbox.toBounds)
    }
  }

  def getAnnotations: Seq[Annotation] = {
    annotationsById.map(_._2).toSeq
  }
}



object PageSpatialInfo {
  implicit class RicherBoundingBox(val bb: BoundingBox) extends AnyVal {
    def toBounds: spatialindex.Bounds = {
      spatialindex.regions.bbox(bb.x, bb.y, bb.width, bb.height)
    }
  }


  def apply(ext: spatialindex.Bounds): PageSpatialInfo = new PageSpatialInfo {
    override val extents = ext
  }


  def vconcat(psis: PageSpatialInfo*): PageSpatialInfo = {
    psis.reduce { (p1, p2) =>

      val p12extents = p1.extents.union(p2.extents)

      val psi = PageSpatialInfo(
        p12extents.toBounds
      )

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
