package org.watrworks
package segment

import geometry._
import geometry.syntax._
import watrmarks.Label
import utils.ExactFloats._
import extract._
import TypeTags._
import watrmarks._
import utils.intervals.Interval
import textgrid._

import scala.reflect._

trait PageSegmenter
  extends ColumnFinding
  with TextReconstruction
  with GlyphRuns
  with GlyphTrees
  with TextBlockGrouping
  with ShapeFunctions
  with ReferenceBlockConverter
  with TrapezoidPagewiseAnalysis

object PageSegmenter {

  def apply(
    pageNum0: Int @@ PageNum,
    documentSegmenter0: BaseDocumentSegmenter
  ): PageSegmenter = new PageSegmenter {

    override val docScope: BaseDocumentSegmenter = documentSegmenter0

    override val pageNum: Int @@ PageNum = pageNum0
    override val pageStats: PageLayoutStats = new PageLayoutStats()
  }
}

// Trait from which other page-level segmentation trait can inherit
trait BasePageSegmenter extends PageScopeTracing { self =>

  lazy val pageScope = self

  def docScope: BaseDocumentSegmenter

  def pageNum: Int @@ PageNum
  def pageStats: PageLayoutStats

  def accumulateShapeStats(shape: GeometricFigure): Unit = {
    docScope.docStats.accumShapeStats(shape);
    pageStats.accumShapeStats(shape);
  }

  lazy val (pageItems, pageGeom) = docScope.pageAtomsAndGeometry(pageNum.unwrap)

  def pageGeometry = pageGeom.bounds

  lazy val shapeIndex: ShapeIndex = docScope.getLabeledShapeIndex(pageNum)

  protected def pageVerticalSlice(left: Double, width: Double): Option[LTBounds] = {
    pageGeometry.getVerticalSlice(left.toFloatExact(), width.toFloatExact())
  }

  protected def pageHorizontalSlice(top: Double, height: Double): Option[LTBounds] = {
    val texact = top.toFloatExact()
    val hexact = height.toFloatExact()
    val t      = max(texact, pageGeometry.top)
    val b      = min(texact + hexact, pageGeometry.bottom)
    pageGeometry.getHorizontalSlice(t, b - t)
  }

  protected def searchForPoints(query: GeometricFigure, l: Label): Seq[PointShape] = {
    shapeIndex
      .searchShapes(query, l)
      .map { _.asPointShape }
  }

  protected def searchForLines(query: GeometricFigure, l: Label*): Seq[LineShape] = {
    shapeIndex
      .searchShapes(query, l: _*)
      .map { _.asLineShape }
  }

  protected def searchForRects(query: GeometricFigure, l: Label*): Seq[RectShape] = {
    shapeIndex
      .searchShapes(query, l: _*)
      .map { _.asRectShape }
  }

  def getLabeledShapes(l: Label): Seq[AnyShape] = shapeIndex.getShapesWithLabel(l)

  def getLabeledRects(l: Label): Seq[RectShape]   = getLabeledShapes(l).map(_.asRectShape)
  def getLabeledTraps(l: Label): Seq[TrapShape]   = getLabeledShapes(l).map(_.asTrapShape)
  def getLabeledLines(l: Label): Seq[LineShape]   = getLabeledShapes(l).map(_.asLineShape)
  def getLabeledPoints(l: Label): Seq[PointShape] = getLabeledShapes(l).map(_.asPointShape)

  protected def deleteLabeledShapes(l: Label): Unit = {
    deleteShapes(shapeIndex.getShapesWithLabel(l))
  }

  protected def deleteShapes[T <: GeometricFigure](shapes: Seq[DocSegShape[T]]): Unit = {
    shapes.foreach { sh => shapeIndex.deleteShape(sh) }
  }

  protected def initShape[T <: GeometricFigure: ClassTag](
    shape: T,
    l: Label
  ): DocSegShape[GeometricFigure] = {
    shapeIndex.initShape { id =>
      DocSegShape.create(id, pageNum, shape, l)
    }
  }
  protected def indexShape[T <: GeometricFigure: ClassTag](
    shape: T,
    l: Label
  ): DocSegShape[GeometricFigure] = {
    shapeIndex.indexShape { id =>
      DocSegShape.create(id, pageNum, shape, l)
    }
  }

  protected def indexShapeAndSetItems[T <: GeometricFigure: ClassTag](
    shape: T,
    l: Label,
    items: ExtractedItem*
  ): DocSegShape[GeometricFigure] = {
    val s = shapeIndex.indexShape { id =>
      DocSegShape.create(id, pageNum, shape, l)
    }
    setExtractedItemsForShape(s, items.toSeq)
    s
  }

  protected def deleteShape[T <: GeometricFigure](shape: DocSegShape[T]): Unit = {
    shapeIndex.deleteShape(shape)
  }

  protected def reindexShapes(l: Label): Unit = {
    shapeIndex.reindexShapes(l)
  }

  protected def unindexShape[T <: GeometricFigure](shape: DocSegShape[T]): Unit = {
    shapeIndex.unindexShape(shape)
  }

  protected def unindexShapes[T <: GeometricFigure](shapes: Seq[DocSegShape[T]]): Unit = {
    shapes.foreach { sh => shapeIndex.unindexShape(sh) }
  }

  protected def getClusteredLines(l: Label): Seq[(Int @@ ShapeID, Seq[LineShape])] = {
    shapeIndex
      .getClustersWithReprID(l)
      .map { case (id, shapes) =>
        (id, shapes.map { _.asLineShape })
      }
  }

  protected def getClusteredRects(l: Label): Seq[(Int @@ ShapeID, Seq[RectShape])] = {
    shapeIndex
      .getClustersWithReprID(l)
      .map { case (id, shapes) =>
        (id, shapes.map { _.asRectShape })
      }
  }

  protected def cluster1(l: Label, shape: DocSegShape[GeometricFigure]): Unit = {
    val _ = shapeIndex.addCluster(l, Seq(shape))
  }

  protected def cluster2(
    l: Label,
    shape1: DocSegShape[GeometricFigure],
    shape2: DocSegShape[GeometricFigure]
  ): Unit = {
    shapeIndex.union(l, shape1, shape2)
  }

  protected def clusterN(l: Label, shapes: Seq[DocSegShape[GeometricFigure]]): Unit = {
    val _ = shapeIndex.addCluster(l, shapes)
  }

  protected def setAttrForShape[A: ClassTag](label: Label): (AnyShape, A) => Unit = { (shape, a) =>
    shapeIndex.setShapeAttribute[A](shape.id, label, a)
  }

  protected def getAttrForShape[A: ClassTag](label: Label): (AnyShape) => Option[A] = { (shape) =>
    shapeIndex.getShapeAttribute[A](shape.id, label)
  }

  protected def setExtractedItemsForShape(
    shape: DocSegShape[GeometricFigure],
    items: Seq[ExtractedItem]
  ): Unit = {
    shapeIndex.setShapeAttribute[Seq[ExtractedItem]](shape.id, LB.ExtractedItems, items)
  }

  protected def getExtractedItemsForShape(
    shape: DocSegShape[GeometricFigure]
  ): Seq[ExtractedItem] = {
    shapeIndex.getShapeAttribute[Seq[ExtractedItem]](shape.id, LB.ExtractedItems).get
  }

  protected def getExtractedItemsForShapes(
    shapes: Seq[DocSegShape[GeometricFigure]]
  ): Seq[Seq[ExtractedItem]] = {
    shapes.map { getExtractedItemsForShape(_) }
  }

  def getCharsForShape(shape: DocSegShape[GeometricFigure]): Seq[ExtractedItem.CharItem] = {
    getExtractedItemsForShape(shape)
      .collect { case i: ExtractedItem.CharItem => i }
  }

  protected def setTrapezoidForShape = setAttrForShape[Trapezoid](LB.LinePairTrapezoid)
  protected def getTrapezoidForShape = getAttrForShape[Trapezoid](LB.LinePairTrapezoid)

  protected def setWeightsForShape = setAttrForShape[WeightedLabeling](LB.WeightedLabels)
  protected def getWeightsForShape = getAttrForShape[WeightedLabeling](LB.WeightedLabels)

  protected def setTextForShape = setAttrForShape[TextGrid.Row](LB.TextGridRow)
  protected def getTextForShape = getAttrForShape[TextGrid.Row](LB.TextGridRow)

  protected def setLabeledIntervalsForShape(
    shape: DocSegShape[GeometricFigure],
    intervals: Seq[Interval[Int, Label]]
  ): Unit = {
    shapeIndex
      .setShapeAttribute[Seq[Interval[Int, Label]]](shape.id, LB.LabeledIntervals, intervals)
  }

  protected def getLabeledIntervalsForShape(
    shape: DocSegShape[GeometricFigure]
  ): Option[Seq[Interval[Int, Label]]] = {
    shapeIndex.getShapeAttribute[Seq[Interval[Int, Label]]](shape.id, LB.LabeledIntervals)
  }

  protected def setFontIndexForShape(shape: DocSegShape[GeometricFigure], i: Int): Unit = {
    shapeIndex.setShapeAttribute[Int](shape.id, LB.FontIndex, i)
  }

  protected def getFontIndexForShape(shape: DocSegShape[GeometricFigure]): Int = {
    shapeIndex.getShapeAttribute[Int](shape.id, LB.FontIndex).get
  }

  protected def setFontsForShape(
    shape: AnyShape,
    fontIds: Set[String @@ ScaledFontID]
  ): Unit = {
    shapeIndex.setShapeAttribute[Set[String @@ ScaledFontID]](shape.id, LB.Fonts, fontIds)
  }

  protected def getFontsForShape(
    shape: DocSegShape[GeometricFigure]
  ): Set[String @@ ScaledFontID] = {
    shapeIndex.getShapeAttribute[Set[String @@ ScaledFontID]](shape.id, LB.Fonts).get
  }

  protected def setLinkedShape(shape1: AnyShape, linkage: Label, shape2: AnyShape): Unit = {
    shapeIndex.setShapeAttribute[AnyShape](shape1.id, linkage, shape2)
  }

  protected def getLinkedShape(shape1: AnyShape, linkage: Label): Option[AnyShape] = {
    shapeIndex.getShapeAttribute[AnyShape](shape1.id, linkage)
  }

  protected def setPrimaryFontForShape(
    shape: DocSegShape[GeometricFigure],
    fontId: String @@ ScaledFontID
  ): Unit = {
    val someId = Some(fontId)
    shapeIndex.setShapeAttribute[Option[String @@ ScaledFontID]](shape.id, LB.PrimaryFont, someId)
  }

  protected def getPrimaryFontForShape(
    shape: DocSegShape[GeometricFigure]
  ): Option[String @@ ScaledFontID] = {
    val someId =
      shapeIndex.getShapeAttribute[Option[String @@ ScaledFontID]](shape.id, LB.PrimaryFont)
    someId.flatten
  }

  protected def setFontOffsetsForShape(
    shape: DocSegShape[GeometricFigure],
    offsets: FontBaselineOffsets
  ): Unit = {
    shapeIndex.setShapeAttribute[FontBaselineOffsets](shape.id, LB.FontBaselineOffsets, offsets)
  }

  protected def getFontOffsetsForShape(
    shape: DocSegShape[GeometricFigure]
  ): Option[FontBaselineOffsets] = {
    shapeIndex.getShapeAttribute[FontBaselineOffsets](shape.id, LB.FontBaselineOffsets)
  }

  protected def queriesAllEmpty(queryRect: LTBounds, labels: Label*): Boolean = {
    labels
      .map { l => searchForRects(queryRect, l).isEmpty }
      .forall(b => b)
  }

  protected def hasNoNonTextOverlaps(queryRect: LTBounds): Boolean = {
    queriesAllEmpty(queryRect, LB.Image, LB.PathBounds)
  }

  protected def hasNoOverlaps(queryRect: LTBounds): Boolean = {
    queriesAllEmpty(queryRect, LB.Image, LB.PathBounds, LB.Glyph)
  }

  protected def findDeltas(ns: Seq[Int @@ FloatRep]): Seq[Int @@ FloatRep] = {
    if (ns.length < 2) Seq()
    else {
      ns.zip(ns.tail)
        .map { case (n1, n2) => n2 - n1 }
    }
  }

  protected def getFontsSortedByHighestOccurrenceCount(): Seq[String @@ ScaledFontID] = {
    docScope
      .getFontsWithDocwideOccurrenceCounts()
      .sortBy(_._2)
      .reverse
      .map(_._1)
  }

  protected def findPairwiseVerticalJumps[G <: GeometricFigure](
    shapes: Seq[DocSegShape[G]],
    getY: (DocSegShape[G]) => Int @@ FloatRep
  ): Seq[(Int @@ FloatRep, (DocSegShape[G], DocSegShape[G]))] = {

    val sorted = shapes.sortBy { getY(_) }
    val yVals  = sorted.map(s => getY(s))
    val deltas = findDeltas(yVals)

    deltas.zip(sorted.zip(sorted.tail))

  }

  protected def pairwiseItemDistances(sortedLineCCs: Seq[PageItem]): Seq[FloatExact] = {
    val cpairs = sortedLineCCs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2) => (c2.bbox.left - c1.bbox.right)
      case _           => 0d.toFloatExact()
    })

    dists :+ 0d.toFloatExact()
  }

  def idsAreConsecutive(id1: Int @@ CharID, id2: Int @@ CharID): Boolean =
    id1.unwrap == id2.unwrap - 1

  def itemsAreConsecutive(item1: ExtractedItem, item2: ExtractedItem): Boolean =
    idsAreConsecutive(item1.id, item2.id)

  protected def clipRectBetween(
    x1: Int @@ FloatRep,
    x2: Int @@ FloatRep,
    rect: LTBounds
  ): Option[LTBounds] = {
    for {
      rightHalf <- rect.splitVertical(x1)._2
      leftHalf  <- rightHalf.splitVertical(x2)._1
    } yield leftHalf
  }

  implicit class RicherShapes[+A <: GeometricFigure](val theShape: DocSegShape[A]) {

    def setAttr[W: ClassTag](label: Label, w: W): Unit = {
      shapeIndex.setShapeAttribute[W](theShape.id, label, w)
    }
    def getAttr[W: ClassTag](label: Label): Option[W] = {
      shapeIndex.getShapeAttribute[W](theShape.id, label)
    }
  }
}
