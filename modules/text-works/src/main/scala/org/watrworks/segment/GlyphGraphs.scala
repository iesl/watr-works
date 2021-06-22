package org.watrworks
package segment

import geometry._
import geometry.syntax._
import rsearch.{Octothorpe => Oct}
import rsearch.{Octothorpe}
import scala.reflect._
import watrmarks.Label
import utils.ExactFloats._
import transcripts.Transcript

// Neighborhood search types
sealed trait Neighbor {
  def tags: List[String]
}

object Neighbor {
  case class ByShape(
    s: AnyShape,
    tags: List[String]
  ) extends Neighbor

  case class ByIDOffset(
    offset: Int,
    tags: List[String]
  ) extends Neighbor

  case class SortWith[A, B: Ordering](
    f: A => B
  )
  case class BySearch[Figure <: GeometricFigure](
    oct: Oct,
    search: Rect => Seq[DocSegShape[Figure]],
    sortBy: SortWith[DocSegShape[Figure], Double],
    filterResults: Seq[DocSegShape[Figure]] => Seq[DocSegShape[Figure]],
    action: Seq[DocSegShape[Figure]] => Unit,
    tags: List[String]
  ) extends Neighbor

  implicit class RicherNeighbor(val theNeighbor: Neighbor) extends AnyVal {
    def withTags(tags: String*): Neighbor = theNeighbor match {
      case v: ByShape     => v.copy(tags = tags.to(List) ++ v.tags)
      case v: ByIDOffset  => v.copy(tags = tags.to(List) ++ v.tags)
      case v: BySearch[_] => v.copy(tags = tags.to(List) ++ v.tags)
    }
  }

}

trait GlyphGraphSearch extends BasePageSegmenter with LineSegmentation {
  def searchDownFor[Figure <: GeometricFigure](
    r: Rect,
    maxDistance: Int @@ FloatRep
  ): Octothorpe = {
    val r2      = r.translate(FloatExact.zero, maxDistance)
    val horizon = r union r2

    Oct
      .withSearchRegions(Oct.cell(Dir.Bottom))
      .withHorizon(horizon)
      .centeredOn(r)
  }

  def lookDownFor[Figure <: GeometricFigure](
    r: Rect,
    maxDistance: Int @@ FloatRep,
    cb: Seq[DocSegShape[Figure]] => Unit,
    forLabels: Label*
  ): Neighbor.BySearch[Figure] = {
    val octo = searchDownFor[Figure](r, maxDistance)

    val tags = s"""lookDownFor(${forLabels.map(_.fqn).mkString(" & ")})""" :: Nil

    val sortWithTop = Neighbor.SortWith[DocSegShape[Figure], Double](
      _.shape.minBounds.getTop
    )

    Neighbor.BySearch[Figure](
      octo,
      (area: Rect) => searchForShapes(area, forLabels: _*),
      sortBy = sortWithTop,
      filterResults = (ds: Seq[DocSegShape[Figure]]) => ds,
      cb,
      tags
    )
  }

  def lookLeftFor[Figure <: GeometricFigure](
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

    val tags = s"""oct:lookLeft(${forLabels.map(_.fqn).mkString(" & ")})""" :: Nil

    val sortWithTop = Neighbor.SortWith[DocSegShape[Figure], Double](
      _.shape.minBounds.getTop
    )
    Neighbor.BySearch[Figure](
      octo,
      (area: Rect) => searchForShapes(area, forLabels: _*),
      sortBy = sortWithTop,
      filterResults = (ds: Seq[DocSegShape[Figure]]) => ds.take(1),
      cb,
      tags
    )
  }

  def initNodeShape[Figure <: GeometricFigure: ClassTag](
    area: Figure,
    label: Label,
    primaryFont: Option[String @@ ScaledFontID]
  ): DocSegShape[Figure] = {
    val ps = indexShape(area, label)
    primaryFont.foreach(ps.setAttr(PrimaryFont)(_))
    ps
  }

  def boundingRect(pageSlice: Rect, c1Rect: Rect): Option[Rect] = {
    pageSlice.clipLeftRight(c1Rect.left, c1Rect.right)
  }
  def boundingRect(pageSlice: Rect, c1Rect: Rect, c2Rect: Rect): Option[Rect] = {
    val c12Rect = c1Rect union c2Rect
    pageSlice.clipLeftRight(c12Rect.left, c12Rect.right)
  }

  def fontBackslashAngle = pageScope.pageStats.fontBackslashAngle
  def fontVJump          = pageScope.pageStats.fontVJump

  def addFontVJumpEdge(fromNGram: RectShape): Unit = {
    val ngramFontId: String @@ ScaledFontID = fromNGram.getAttr(PrimaryFont).get
    val glyphNGramTop                       = fromNGram.asRectShape.shape.top

    _addEdge(fromNGram)(
      lookDownFor(
        fromNGram.minBounds,
        pageGeometry.height,
        (_: Seq[RectShape]).headOption.foreach(shape => {
          val shapeFontId = shape.getAttr(PrimaryFont).get
          val shapeTop    = shape.shape.top
          fontVJump.add(ngramFontId, glyphNGramTop, shapeFontId, shapeTop)
        }),
        LB.GlyphBigram
      ).withTags("findFontVJumps")
    )
  }

  def findIndentationShapes(
    fromBaseline: AnyShape,
    baselineFontId: String @@ ScaledFontID
  ): Unit = {
    val fontBaselineShape = fromBaseline.asLineShape
    val fontBaseline      = fontBaselineShape.shape

    val fontOffsets      = docScope.fontDefs.getScaledFontOffsets(baselineFontId)
    val fontHeight       = fontOffsets.distanceBetween(_.topLine, _.bottomLine)
    val lookDownDistance = fontHeight * 3
    val minIndent        = fontHeight // TODO choose better values for these parameters
    val maxIndent        = fontHeight * 10

    _addEdge(fontBaselineShape)(
      lookDownFor(
        fontBaseline.minBounds,
        lookDownDistance,
        (_: Seq[LineShape]).foreach(shape => {
          val neighborShape   = shape.shape
          val neighborShapeP1 = neighborShape.p1
          val baselineP1      = fontBaseline.p1

          val leftSideOffset = neighborShapeP1.x - baselineP1.x
          val isIndent       = minIndent < leftSideOffset && leftSideOffset <= maxIndent
          val isOutdent      = minIndent < -leftSideOffset && -leftSideOffset <= maxIndent
          if (leftSideOffset != FloatExact.zero) {
            println(s"leading traps: leftSideOffset=${leftSideOffset.pp()}, min/max = ${minIndent
              .pp()}/${maxIndent.pp()}, isIn/Out = ${isIndent || isOutdent} ")
          }
          if (isIndent || isOutdent) {
            val neighborFontId = shape.getAttr(PrimaryFont).get
            val angleTo        = neighborShapeP1.angleTo(baselineP1)
            val angleToDeg     = ((angleTo * 180) / math.Pi).toFloatExact()

            fontBackslashAngle.add(baselineFontId, neighborFontId, angleToDeg)

            val leadingTrapezoid = Trapezoid.fromHorizontals(fontBaseline, neighborShape)
            val trapType         = if (isIndent) "Indent" else "Outdent"

            traceLog.trace {
              figure(leadingTrapezoid) tagged s"Leading ${trapType}"
            }
          }

        }),
        LB.CharRunFontLeadingBaseline
      )
    )
  }

  protected def findLeadingRunBaselines(
    fromBaseline: AnyShape,
    baselineFontId: String @@ ScaledFontID
  ): Unit = {

    val fontBaselineShape = fromBaseline.asLineShape
    val fontBaseline      = fontBaselineShape.shape

    val chars   = fontBaselineShape.getAttr(ExtractedChars).getOrElse(Nil)
    val maxDist = chars.headOption.map(_.fontBbox.width).getOrElse(10.toFloatExact())

    _addEdge(fontBaselineShape)(
      lookLeftFor(
        fontBaseline.minBounds,
        maxDist,
        (foundShapes: Seq[AnyShape]) => {
          if (foundShapes.isEmpty) {
            fontBaselineShape.addLabels(LB.CharRunFontLeadingBaseline)
            findIndentationShapes(fromBaseline, baselineFontId)

            traceLog.trace {
              shape(fontBaselineShape)
            }
          }
        }
      ).withTags("findLeftmostEdge")
    )
  }

  import Neighbor._
  import scala.collection.mutable

  var openEdges: mutable.ArrayBuffer[Neighbor]    = mutable.ArrayBuffer()
  var workingEdges: mutable.ArrayBuffer[Neighbor] = mutable.ArrayBuffer()

  val createLabel = Transcript.Label.create(_)

  def createLabelOn[Fig <: GeometricFigure](name: String, fig: Fig) =
    Transcript.Label.create(name).onShapes(fig)


  protected def _addEdge(shape: AnyShape)(nn: Neighbor) = {
    openEdges.append(nn)
  }

  protected def runNeigborBySearch[Figure <: GeometricFigure](bySearch: BySearch[Figure]): Unit = {
    val BySearch(oct, search, sortBy, filterResults, cb, tags) = bySearch

    val res0     = oct.runSearch(search)
    val sorted   = res0.sortBy(sortBy.f)
    val filtered = filterResults(sorted)

    traceLog.trace {

      val searchAreaLabels = oct
        .searchAreas()
        .to(List)
        .map({ case (sbounds, searchArea) =>
          val name = sbounds match {
            case Oct.Bounds.Cell(dir)        => s"Cell:${dir}"
            case Oct.Bounds.CellSpan(d1, d2) => s"Span:${d1}-${d2}"
          }
          createLabel(s"Query/${name}")
            .onShapes(searchArea)
        })

      val resLabels = traceLog.shapesToLabels(filtered: _*)

      createLabel("OctSearch")
        .withProp("class", ">lazy")
        .withChildren(
          createLabel("Octothorpe")
            .withChildren(
              createLabelOn("FocalRect", oct.focalRect)
                .withProp("class", "=eager")
                .withProp("tags", tags: _*),
              createLabelOn("HorizonRect", oct.horizonRect),
              createLabel("SearchArea")
                .withChildren(searchAreaLabels: _*)
            ),
          createLabel("Found")
            .withChildren(resLabels: _*)
        )

    }
    cb(filtered)
  }
  protected def expandEdges(): Unit = {
    workingEdges = openEdges
    openEdges = mutable.ArrayBuffer()
    workingEdges.foreach(_ match {
      case bySearch: BySearch[_] =>
        runNeigborBySearch(bySearch)
      case _ =>
    })

    if (!openEdges.isEmpty) {
      expandEdges()
    }
  }

}
