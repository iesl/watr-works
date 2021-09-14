package org.watrworks
package segment

import prelude._

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
import watrmarks.WeightedLabeling

trait PageSegmenter
  extends ColumnFinding
  with TextReconstruction
  with GlyphRuns
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
    override protected def scopeTags        = s"page:${pageNum}." :: Nil
  }
}

trait AttributeTags {

  val ParentShape      = AttrWitness.Mk[AnyShape]()
  val ChildShapes      = AttrWitness.Mk[Seq[AnyShape]]()
  val UpLeftChar       = AttrWitness.Mk[String]()
  val ExtractedChars   = AttrWitness.Mk[Seq[ExtractedItem.CharItem]]()
  val ExtractedChar    = AttrWitness.Mk[ExtractedItem.CharItem]()
  val Fonts            = AttrWitness.Mk[Set[String @@ ScaledFontID]]()
  val GlyphTreeEdges   = AttrWitness.Mk[List[Neighbor]]()
  val PrimaryFont      = AttrWitness.Mk[String @@ ScaledFontID]()
  val FontOffsets      = AttrWitness.Mk[FontBaselineOffsets]()
  val LabeledIntervals = AttrWitness.Mk[Seq[Interval[Int, Label]]]()
  val LinkedTrapezoid  = AttrWitness.Mk[Trapezoid]()
  val LinkedTextGrid   = AttrWitness.Mk[TextGrid.Row]()
  val WeightedLabels   = AttrWitness.Mk[WeightedLabeling]()

}

trait PageScopeTracing extends ScopedTracing { self: BasePageSegmenter =>
  lazy val traceLog: PageScopeTracing = this

  def labeledShapes(labels: Label*): TraceLog = {
    def filterf(shape: AnyShape): Boolean = {
      labels.exists(shape.hasLabel(_))
    }

    val f =
      if (labels.nonEmpty) filterf(_)
      else (_: AnyShape) => true

    val filtered = shapeIndex.shapeRIndex.getItems().filter(f)
    shape(filtered: _*)
  }

}


// Trait from which other page-level segmentation trait can inherit
trait BasePageSegmenter extends PageScopeTracing with AttributeTags with TranscriptLabeling {
  self =>

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
    pageGeometry.clipLeftWidth(left.toFloatExact(), width.toFloatExact())
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

  def getLabeledShapes[T <: GeometricFigure](l: Label): Seq[DocSegShape[T]] =
    shapeIndex.getShapesWithLabel(l).map(_.asInstanceOf[DocSegShape[T]])

  def getLabeledRects(l: Label): Seq[RectShape]   = getLabeledShapes[Rect](l)
  def getLabeledTraps(l: Label): Seq[TrapShape]   = getLabeledShapes[Trapezoid](l)
  def getLabeledLines(l: Label): Seq[LineShape]   = getLabeledShapes[Line](l)
  def getLabeledPoints(l: Label): Seq[PointShape] = getLabeledShapes[Point](l)

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

  def hasFont(shape: AnyShape, fontId: String @@ ScaledFontID): Boolean =
    shape.getAttr(Fonts).exists(_.contains(fontId))

  def fontsExist(shape: AnyShape): Boolean =
    shape.getAttr(Fonts).isDefined

  def sortShapesByFontOccurrence[T <: GeometricFigure](
    shapes: Seq[DocSegShape[T]]
  ): Seq[(List[DocSegShape[T]], String @@ ScaledFontID)] = {

    def _loop(
      scaledFontIds: List[String @@ ScaledFontID],
      lineShapes: Seq[DocSegShape[T]]
    ): List[(List[DocSegShape[T]], String @@ ScaledFontID)] = scaledFontIds match {

      case headFontId :: tailFontIds =>
        val (linesForFont, others) = lineShapes.partition(hasFont(_, headFontId))

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
