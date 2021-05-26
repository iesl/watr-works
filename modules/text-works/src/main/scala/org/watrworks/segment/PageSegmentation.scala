package org.watrworks
package segment

import geometry._
import geometry.syntax._
import watrmarks.Label
import utils.ExactFloats._
import extract._
import TypeTags._

import scala.reflect._
import rsearch.AttrWitness
import utils.intervals.Interval
import textgrid.TextGrid
import org.watrworks.watrmarks.WeightedLabeling

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

    override val pageNum: Int @@ PageNum    = pageNum0
    override val pageStats: PageLayoutStats = new PageLayoutStats()
  }
}

trait AttributeTags {

  implicit val UpLeftChar       = AttrWitness.Mk[String]()
  implicit val ExtractedChars   = AttrWitness.Mk[Seq[ExtractedItem.CharItem]]()
  implicit val ExtractedChar    = AttrWitness.Mk[ExtractedItem.CharItem]()
  implicit val Fonts            = AttrWitness.Mk[Set[String @@ ScaledFontID]]()
  implicit val GlyphTreeEdges   = AttrWitness.Mk[List[Neighbor]]()
  implicit val PrimaryFont      = AttrWitness.Mk[String @@ ScaledFontID]()
  implicit val FontOffsets      = AttrWitness.Mk[FontBaselineOffsets]()
  implicit val LabeledIntervals = AttrWitness.Mk[Seq[Interval[Int, Label]]]()
  implicit val LinkedTrapezoid  = AttrWitness.Mk[Trapezoid]()
  implicit val LinkedTextGrid   = AttrWitness.Mk[TextGrid.Row]()
  implicit val WeightedLabels   = AttrWitness.Mk[WeightedLabeling]()

}

// Trait from which other page-level segmentation trait can inherit
trait BasePageSegmenter extends PageScopeTracing with AttributeTags { self =>

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

  protected def pageVerticalSlice(left: Double, width: Double): Option[Rect] = {
    pageGeometry.getVerticalSlice(left.toFloatExact(), width.toFloatExact())
  }

  protected def pageHorizontalSlice(top: Double, height: Double): Option[Rect] = {
    val texact = top.toFloatExact()
    val hexact = height.toFloatExact()
    val t      = max(texact, pageGeometry.top)
    val b      = min(texact + hexact, pageGeometry.bottom)
    pageGeometry.getHorizontalSlice(t, b - t)
  }

  protected def searchForShapes[F <: GeometricFigure](
    query: GeometricFigure,
    labels: Label*
  ): Seq[DocSegShape[F]] = {
    shapeIndex
      .searchShapes(query, labels: _*)
      .map { _.asInstanceOf[DocSegShape[F]] }
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

  def getLabeledShapes(l: Label): Seq[AnyShape]   = shapeIndex.getShapesWithLabel(l)
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
  ): DocSegShape[T] = {
    shapeIndex.initShape { id =>
      DocSegShape.create(id, pageNum, shape, l)
    }
  }

  protected def indexShape[T <: GeometricFigure: ClassTag](
    shape: T,
    l: Label
  ): DocSegShape[T] = {
    shapeIndex.indexShape { id =>
      DocSegShape.create(id, pageNum, shape, l)
    }
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

  def getCharsForShape(shape: DocSegShape[GeometricFigure]): Seq[ExtractedItem.CharItem] = {
    shape.getAttr(ExtractedChars).getOrElse(Nil)
  }

  protected def queriesAllEmpty(queryRect: Rect, labels: Label*): Boolean = {
    labels
      .map { l => searchForRects(queryRect, l).isEmpty }
      .forall(b => b)
  }

  protected def hasNoNonTextOverlaps(queryRect: Rect): Boolean = {
    queriesAllEmpty(queryRect, LB.Image, LB.PathBounds)
  }

  protected def hasNoOverlaps(queryRect: Rect): Boolean = {
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
    rect: Rect
  ): Option[Rect] = {
    for {
      rightHalf <- rect.splitVertical(x1)._2
      leftHalf  <- rightHalf.splitVertical(x2)._1
    } yield leftHalf
  }

  def sortShapesByFontOccurrence(
    shapes: Seq[AnyShape]
  ): Seq[(List[AnyShape], String @@ ScaledFontID)] = {

    def _loop(
      scaledFontIds: List[String @@ ScaledFontID],
      lineShapes: Seq[AnyShape]
    ): List[(List[AnyShape], String @@ ScaledFontID)] = scaledFontIds match {

      case headFontId :: tailFontIds =>
        val (linesForFont, others) = lineShapes.partition { lineShape =>
          lineShape.getAttr(Fonts).exists(_.contains(headFontId))
        }

        (linesForFont.to(List), headFontId) :: _loop(tailFontIds, others)

      case Nil => Nil
    }

    val fontsByMostOccuring = getFontsSortedByHighestOccurrenceCount()

    _loop(fontsByMostOccuring.toList, shapes)
  }

  implicit class RicherShapes[+A <: GeometricFigure](val theShape: DocSegShape[A]) {

    def setAttr[V](witness: AttrWitness[V])(attr: witness.AttrT): Unit = {
      shapeIndex.setAttr[witness.AttrT](theShape.id, witness, attr)
    }

    def modAttr[V](witness: AttrWitness[V])(mod: (Option[witness.AttrT]) => witness.AttrT): Unit = {
      val prev = shapeIndex.getAttr[V](theShape.id, witness)
      shapeIndex.setAttr[witness.AttrT](theShape.id, witness, mod(prev))
    }

    def getAttr[V](witness: AttrWitness[V]): Option[witness.AttrT] = {
      shapeIndex.getAttr[witness.AttrT](theShape.id, witness)
    }
  }
}
