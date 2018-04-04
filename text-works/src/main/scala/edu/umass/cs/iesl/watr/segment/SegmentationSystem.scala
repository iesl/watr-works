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

  implicit class RicherLabeledShapes[A <: GeometricFigure](val theShapes: Seq[LabeledShape[A]]) {
    def asLineShapes: Seq[LineShape] = theShapes.asInstanceOf[Seq[LineShape]]
    def asPointShapes: Seq[PointShape] = theShapes.asInstanceOf[Seq[PointShape]]
    def asRectShapes: Seq[RectShape] = theShapes.asInstanceOf[Seq[RectShape]]
  }

  implicit class RicherLabeledShape[A <: GeometricFigure](val theShape: LabeledShape[A]) {
    def asLineShape: LineShape = theShape.asInstanceOf[LineShape]
    def asPointShape: PointShape = theShape.asInstanceOf[PointShape]
    def asRectShape: RectShape = theShape.asInstanceOf[RectShape]
  }

  def labelRegion(bbox: LTBounds, label: Label, text: Option[String] = None): RegionComponent = {
    val stablePage = docStore.getPageIdentifier(pageId)
    val pageRegion = PageRegion(stablePage, bbox)
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
    val texact = top.toFloatExact()
    val hexact = height.toFloatExact()
    val t = max(texact, pageGeometry.top)
    val b = min(texact + hexact, pageGeometry.bottom)
    pageGeometry.getHorizontalSlice(t, b-t)
  }

  protected def searchForPoints(query: GeometricFigure, l: Label): Seq[PointShape] = {
    pageIndex.shapes.searchShapes(query, l)
      .map {_.asPointShape}
  }

  protected def searchForLines(query: GeometricFigure, l: Label*): Seq[LineShape] = {
    pageIndex.shapes.searchShapes(query, l:_*)
      .map {_.asLineShape}
  }

  protected def searchForRects(query: GeometricFigure, l: Label*): Seq[RectShape] = {
    pageIndex.shapes.searchShapes(query, l:_*)
      .map {_.asRectShape}
  }


  protected def getLabeledRects(l: Label): Seq[LineShape] = {
    pageIndex.shapes.getShapesWithLabel(l)
      .map(_.asLineShape)
  }

  protected def getLabeledLines(l: Label): Seq[LineShape] = {
    pageIndex.shapes.getShapesWithLabel(l)
      .map(_.asLineShape)
  }

  protected def getLabeledPoints(l: Label): Seq[PointShape] = {
    pageIndex.shapes.getShapesWithLabel(l)
      .map(_.asPointShape)
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

  protected def indexShapeForItems[T <: GeometricFigure](shape: T, l: Label, items: ExtractedItem*): LabeledShape[GeometricFigure] = {
    val s = pageIndex.shapes.indexShape(shape, l)
    setExtractedItemsForShape(s, items.toSeq)
    s
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



trait SegmentationCommons {
  def modalValue(ccs: Seq[Component], f: Component => Int): Option[Int] = {
    ccs.groupBy{f(_)}.toSeq
      .sortBy({ case (_, atoms) => atoms.length })
      .reverse.headOption.map(_._1)
  }

}
