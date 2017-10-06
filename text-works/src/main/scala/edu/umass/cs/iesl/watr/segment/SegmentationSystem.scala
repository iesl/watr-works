package edu.umass.cs.iesl.watr
package segment

import geometry._
import geometry.syntax._
import watrmarks.Label
import corpora.DocumentZoningApi
import spindex._
import utils.ExactFloats._

trait DocumentScopeSegmenter extends SegmentationCommons { self =>

  lazy val docScope = self

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

  type LineShape = LabeledShape[Line]
  type PointShape = LabeledShape[Point]
  type RectShape = LabeledShape[LTBounds]
  type AnyShape = LabeledShape[GeometricFigure]


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


}

case class Bin(
  centroid: DataPoint,
  neighbors: Seq[DataPoint]
    // weightedAverage: Int@@FloatRep
)
case class DataPoint(
  value: Int@@FloatRep,
  len: Int
)

import TypeTags._
import utils.ExactFloats._
import utils.SlicingAndDicing._
import scala.annotation.tailrec

trait SegmentationCommons {
  def modalValue(ccs: Seq[Component], f: Component => Int): Option[Int] = {
    ccs.groupBy{f(_)}.toSeq
      .sortBy({ case (_, atoms) => atoms.length })
      .reverse.headOption.map(_._1)
  }

  def quickNearestNeighbors(in: Seq[Int@@FloatRep]): Seq[Bin] = {

    @tailrec
    def loop(init: List[(Int@@FloatRep, Int)], groups: List[Bin]): List[Bin] = {
      if (init.isEmpty) groups else {
        val (dmax, dmaxCount) = init.head
        val (grouped, others) = init.tail.partition { case (d, dcount) =>
          val inRange = d - 0.5d < dmax && dmax < d + 0.5d
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


    loop(distsSortByCount, List()).reverse
  }
}
