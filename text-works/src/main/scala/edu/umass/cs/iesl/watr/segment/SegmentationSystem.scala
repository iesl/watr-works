package edu.umass.cs.iesl.watr
package segment

import geometry._
import geometry.syntax._
import watrmarks.Label
import corpora.DocumentZoningApi
import spindex._
import utils.ExactFloats._
import extract._
import segment.{SegmentationLabels => LB}

trait DocumentScopeSegmenter extends SegmentationCommons { self =>

  lazy val docScope = self

  def pageAtomsAndGeometry: Seq[(Seq[ExtractedItem], PageGeometry)]

  def fontDefs: FontDefs

  def mpageIndex: MultiPageIndex

  def docStore: DocumentZoningApi

  def docStats: DocumentLayoutStats

  def stableId: String@@DocumentID
  def docId: Int@@DocumentID

  def pageSegmenters(): Seq[PageScopeSegmenter]

  def createZone(label: Label, pageRegions: Seq[PageRegion]): Option[Int@@ZoneID] = {
    docStore.labelRegions(label, pageRegions)
  }
}

trait PageScopeSegmenter extends PageScopeTracing with SegmentationCommons { self =>
  lazy val pageScope = self

  def docScope: DocumentScopeSegmenter

  def pageId: Int@@PageID
  def pageNum: Int@@PageNum
  def pageStats: PageLayoutStats

  def docStore: DocumentZoningApi = docScope.docStore

  def mpageIndex: MultiPageIndex = docScope.mpageIndex
  def pageGeometry = docStore.getPageGeometry(pageId)
  def pageIndex: PageIndex = mpageIndex.getPageIndex(pageNum)

  def labelRegion(bbox: LTBounds, label: Label, text: Option[String] = None): RegionComponent = {
    val regionId = docStore.addTargetRegion(pageId, bbox)
    val pageRegion = docStore.getTargetRegion(regionId)
    mpageIndex.createRegionComponent(pageRegion, label, text)
  }

  def deleteComponentsWithLabel(l: Label): Unit = {
    pageIndex.components.getComponentsWithLabel(l)
      .foreach { cc =>
        pageIndex.components.removeComponent(cc)
      }
  }

  protected def pageVerticalSlice(left: Double, width: Double): Option[LTBounds] = {
    pageGeometry.getVerticalSlice(left.toFloatExact(), width.toFloatExact())
  }

  protected def pageHorizontalSlice(top: Double, height: Double): Option[LTBounds] = {
    pageGeometry.getHorizontalSlice(top.toFloatExact(), height.toFloatExact())
  }

  protected def searchForPoints(query: GeometricFigure, l: Label): Seq[PointShape] = {
    pageIndex.shapes.searchShapes(query, l)
      .map {_.asInstanceOf[PointShape]}
  }

  protected def searchForLines(query: GeometricFigure, l: Label): Seq[LineShape] = {
    pageIndex.shapes.searchShapes(query, l)
      .map {_.asInstanceOf[LineShape]}
  }

  protected def getLabeledRects(l: Label): Seq[LineShape] = {
    pageIndex.shapes.getShapesWithLabel(l)
      .map(_.asInstanceOf[LineShape])
  }

  protected def getLabeledLines(l: Label): Seq[LineShape] = {
    pageIndex.shapes.getShapesWithLabel(l)
      .map(_.asInstanceOf[LineShape])
  }

  protected def getLabeledPoints(l: Label): Seq[PointShape] = {
    pageIndex.shapes.getShapesWithLabel(l)
      .map(_.asInstanceOf[PointShape])
  }

  protected def deleteLabeledShapes(l: Label): Unit = {
    deleteShapes(pageIndex.shapes.getShapesWithLabel(l))
  }

  protected def deleteShapes[T <: GeometricFigure](shapes: Seq[LabeledShape[T]]): Unit = {
    shapes.foreach { sh => pageIndex.shapes.deleteShape(sh) }
  }

  protected def indexShape[T <: GeometricFigure](shape: T, l: Label): LabeledShape[GeometricFigure] = {
    pageIndex.shapes.indexShape(shape, l)
  }

  protected def unindexShape[T <: GeometricFigure](shape: LabeledShape[T]): Unit = {
    pageIndex.shapes.unindexShape(shape)
  }
  protected def unindexShapes[T <: GeometricFigure](shapes: Seq[LabeledShape[T]]): Unit = {
    shapes.foreach { sh => pageIndex.shapes.unindexShape(sh) }
  }

  protected def addRelation(lhs: Int@@ShapeID, l: Label, rhs: Int@@ShapeID): Unit = {
    pageIndex.shapes.addRelation(lhs, l, rhs)
  }

  protected def getClusteredLines(l: Label): Seq[(Int@@ShapeID, Seq[LineShape])] = {
    pageIndex.shapes.getClustersWithReprID(l)
      .map{ case (id, shapes) =>
        (id, shapes.map {_.asInstanceOf[LineShape]})
      }
  }

  protected def cluster1(l: Label, shape: LabeledShape[GeometricFigure]): Unit = {
    pageIndex.shapes.addCluster(l, Seq(shape))
  }

  protected def cluster2(l: Label, shape1: LabeledShape[GeometricFigure], shape2: LabeledShape[GeometricFigure]): Unit = {
    pageIndex.shapes.union(l, shape1, shape2)
  }

  protected def clusterN(l: Label, shapes: Seq[LabeledShape[GeometricFigure]]): Unit = {
    pageIndex.shapes.addCluster(l, shapes)
  }

  protected def setExtractedItemsForShape(shape: LabeledShape[GeometricFigure], items: Seq[ExtractedItem] ): Unit = {
    pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](shape.id, LB.ExtractedItems, items)
  }

  protected def getExtractedItemsForShape(shape: LabeledShape[GeometricFigure]): Seq[ExtractedItem] = {
    pageIndex.shapes.getShapeAttribute[Seq[ExtractedItem]](shape.id, LB.ExtractedItems).get
  }

  protected def getCharRunBaselineItems(baselineMembers: Seq[LineShape]): Seq[Seq[ExtractedItem]] = {
    baselineMembers.map {charRun =>
      getExtractedItemsForShape(charRun)
    }
  }
}


object QuickNearestNeighbors {
  import TypeTags._
  import utils.ExactFloats._
  import utils.SlicingAndDicing._
  import scala.annotation.tailrec

  case class Bin(
    centroid: DataPoint,
    neighbors: Seq[DataPoint]
  ) {

    def size(): Int = neighbors.map(_.len).sum + centroid.len

    def range(): (Int@@FloatRep, Int@@FloatRep) = {
      val values = (centroid.value.unwrap +:
        neighbors.map(_.value.unwrap))

      (FloatRep(values.min), FloatRep(values.max))
    }
    def maxValue(): Int@@FloatRep = {
      val max = (centroid.value.unwrap +:
        neighbors.map(_.value.unwrap)).max

      FloatRep(max)
    }

    override def toString(): String = {
      val cstr = centroid.toString()
      val nstr = neighbors.map(_.toString()).mkString(", ")
      s"{${cstr} +[ ${nstr} ]}"
    }

  }

  case class DataPoint(
    value: Int@@FloatRep,
    len: Int
  ) {
    override def toString(): String = {
      val cpp = value.pp
      val clen = len
      s"[${cpp} x ${clen}]"
    }

  }

  def qnn(in: Seq[Int@@FloatRep], tolerance: Double = 0.5d): Seq[Bin] = {

    @tailrec
    def loop(init: List[(Int@@FloatRep, Int)], groups: List[Bin]): List[Bin] = {
      if (init.isEmpty) groups else {
        val (dmax, dmaxCount) = init.head
        val (grouped, others) = init.tail.partition { case (d, dcount) =>
          val inRange = d - tolerance < dmax && dmax < d + tolerance
          inRange
        }
        val bin = Bin(
          DataPoint(dmax, dmaxCount),
          grouped.map(g => DataPoint(g._1, g._2))
        )
        loop(others, bin::groups)
      }
    }

    val distsSortByCount: List[(Int@@FloatRep, Int)] = in
      .sorted.toList
      .groupByPairs(_ == _)
      .map(p => (p.head, p.length))
      .sortBy(_._2)
      .reverse
      .toList


    val binned = loop(distsSortByCount, List())

    binned.sortBy(_.size()).reverse
  }
}

trait SegmentationCommons {
  def modalValue(ccs: Seq[Component], f: Component => Int): Option[Int] = {
    ccs.groupBy{f(_)}.toSeq
      .sortBy({ case (_, atoms) => atoms.length })
      .reverse.headOption.map(_._1)
  }

}
