package org.watrworks
package segment

import geometry._
import geometry.syntax._
import rsearch.{Octothorpe => Oct}
import scala.reflect._
import watrmarks.Label
import utils.ExactFloats._
import org.watrworks.transcripts.Transcript

// Neighborhood search types
sealed trait Neighbor

object Neighbor {
  case class ByShape(s: AnyShape)    extends Neighbor
  case class ByIDOffset(offset: Int) extends Neighbor

  trait SortBy[A] {
    def apply[B: Ordering](a: A): B
  }
  case class BySearch[Figure <: GeometricFigure](
    oct: Oct,
    search: Rect => Seq[DocSegShape[Figure]],
    // sortBy: SortBy[DocSegShape[Figure]],
    action: Seq[DocSegShape[Figure]] => Unit
  ) extends Neighbor

}

trait GlyphGraphSearch extends BasePageSegmenter with LineSegmentation {

  def lookDown = Oct
    .withSearchRegions(Oct.cell(Dir.Bottom))
    .withHorizon(pageGeometry)

  def lookDownFrom[Figure <: GeometricFigure](
    r: Rect,
    maxDistance: Int @@ FloatRep,
    cb: Seq[DocSegShape[Figure]] => Unit,
    forLabels: Label*
  ): Neighbor.BySearch[Figure] = {
    val r2      = r.translate(FloatExact.zero, maxDistance)
    val horizon = r union r2

    val octo = Oct
      .withSearchRegions(Oct.cell(Dir.Bottom))
      .withHorizon(horizon)
      .centeredOn(r)

    Neighbor.BySearch[Figure](
      octo,
      (area: Rect) => searchForShapes(area, forLabels: _*),
      cb
    )
  }

  def lookLeftFrom[Figure <: GeometricFigure](
    r: Rect,
    maxDistance: Int @@ FloatRep,
    cb: Seq[DocSegShape[Figure]] => Unit,
    forLabels: Label*
  ): Neighbor.BySearch[Figure] = {
    val r2      = r.translate(-maxDistance, FloatExact.zero)
    val horizon = r union r2

    val octo = Oct
      .withSearchRegions(Oct.cell(Dir.Left))
      .withHorizon(horizon)
      .centeredOn(r)

    Neighbor.BySearch[Figure](
      octo,
      (area: Rect) => searchForShapes(area, forLabels: _*),
      cb
    )
  }

  def lookRight = Oct
    .withSearchRegions(Oct.cell(Dir.Right))
    .withHorizon(pageGeometry)

  def lookBottomRight = Oct
    .withSearchRegions(Oct.cell(Dir.BottomRight))
    .withHorizon(pageGeometry)

  def lookBottomLeft = Oct
    .withSearchRegions(Oct.cell(Dir.BottomLeft))
    .withHorizon(pageGeometry)

  def initNodeShape[Figure <: GeometricFigure: ClassTag](
    area: Figure,
    label: Label,
    primaryFont: Option[String @@ ScaledFontID]
  ): DocSegShape[Figure] = {
    val ps = indexShape(area, label)
    // ps.setAttr(GlyphTreeEdges)(Nil)
    primaryFont.foreach(ps.setAttr(PrimaryFont)(_))
    ps
  }

  def boundingRect(pageSlice: Rect, c1Rect: Rect): Option[Rect] = {
    clipRectBetween(
      c1Rect.left,
      c1Rect.right,
      pageSlice
    )
  }
  def boundingRect(pageSlice: Rect, c1Rect: Rect, c2Rect: Rect): Option[Rect] = {
    val c12Rect = c1Rect union c2Rect
    clipRectBetween(
      c12Rect.left,
      c12Rect.right,
      pageSlice
    )
  }

  def fontBackslashAngle = pageScope.pageStats.fontBackslashAngle
  def fontVJump          = pageScope.pageStats.fontVJump

  def addFontVJumpEdge(fromNGram: RectShape): Unit = {
    val ngramFontId: String @@ ScaledFontID = fromNGram.getAttr(PrimaryFont).get
    val glyphNGramTop                       = fromNGram.asRectShape.shape.top

    _addEdge(fromNGram)(
      lookDownFrom(
        fromNGram.minBounds,
        pageGeometry.height,
        (_: Seq[RectShape]).headOption.foreach(shape => {
          val shapeFontId = shape.getAttr(PrimaryFont).get
          val shapeTop    = shape.shape.top
          fontVJump.add(ngramFontId, glyphNGramTop, shapeFontId, shapeTop)
        }),
        LB.GlyphBigram
      )
    )
  }

  def addFontBackslashEdge(
    fromBaseline: AnyShape,
    baselineFontId: String @@ ScaledFontID
  ): Unit = {
    val fontBaselineShape = fromBaseline.asLineShape
    val fontBaseline      = fontBaselineShape.shape

    _addEdge(fontBaselineShape)(
      lookDownFrom(
        fontBaseline.minBounds,
        pageGeometry.height,
        (_: Seq[LineShape]).headOption.foreach(shape => {
          val neighborFontId  = shape.getAttr(PrimaryFont).get
          val neighborShape   = shape.shape
          val neighborShapeP1 = neighborShape.p1
          val baselineP1      = fontBaseline.p1
          val angleTo         = neighborShapeP1.angleTo(baselineP1)
          val angleToDeg      = ((angleTo * 180) / math.Pi).toFloatExact()

          traceLog.trace(
            traceLog
              .figure(neighborShapeP1.lineTo(baselineP1))
              .tagged(s"Backslash Edge ${angleToDeg}")
          )

          fontBackslashAngle.add(baselineFontId, neighborFontId, angleToDeg)
        }),
        LB.CharRunFontLeadingBaseline
      )
    )
  }

  def addFindLeftmostEdge(
    fromBaseline: AnyShape,
    baselineFontId: String @@ ScaledFontID
  ): Unit = {

    val fontBaselineShape = fromBaseline.asLineShape
    val fontBaseline      = fontBaselineShape.shape

    val chars   = fontBaselineShape.getAttr(ExtractedChars).getOrElse(Nil)
    val maxDist = chars.headOption.map(_.fontBbox.width).getOrElse(10.toFloatExact())

    _addEdge(fontBaselineShape)(
      lookLeftFrom(
        fontBaseline.minBounds,
        maxDist,
        (foundShapes: Seq[AnyShape]) => {
          if (foundShapes.isEmpty) {
            fontBaselineShape.addLabels(LB.CharRunFontLeadingBaseline)
            addFontBackslashEdge(fromBaseline, baselineFontId)
          }
        }
      )
    )
  }

  import Neighbor._
  import scala.collection.mutable

  var openEdges: mutable.ArrayBuffer[Neighbor]    = mutable.ArrayBuffer()
  var workingEdges: mutable.ArrayBuffer[Neighbor] = mutable.ArrayBuffer()

  def _addEdge(shape: AnyShape)(nn: Neighbor) = {
    openEdges.append(nn)

    // shape.modAttr(GlyphTreeEdges)(att =>
    //   att match {
    //     case Some(edges) => edges :+ nn
    //     case None        => List(nn)
    //   }
    // )

  }

  protected def expandEdges(): Unit = {
    workingEdges = openEdges
    openEdges = mutable.ArrayBuffer()
    workingEdges.foreach(_ match {
      case BySearch(oct, search, cb) =>
        Transcript.Label(
          name = "Root",
          id = None,
          range = List(),
          props = Some(Map()),
          children = None
        )
        cb(oct.runSearch(search))
      case _ =>
    })

    if (!openEdges.isEmpty) {
      expandEdges()
    }
  }

  // protected def expandShapesWithLabel(label: Label): Unit = for {
  //   shapeNode <- getLabeledShapes(label)
  //   edgeList  <- shapeNode.getAttr(GlyphTreeEdges).to(List)
  //   neighbor  <- edgeList
  // } neighbor match {
  //   case BySearch(oct, search, cb) =>
  //     cb(oct.runSearch(search))
  //   case _ =>
  // }
}
