package org.watrworks
package segment

import rsearch.{Octothorpe => Oct}
import geometry._
import geometry.syntax._
import utils.IndexedSeqADT._
import utils.{RelativeDirection => Dir}

trait GlyphTrees extends GlyphRuns with LineSegmentation { self =>

  import utils.GuavaHelpers._

  // TODO how many times does a run of glyphs of the same font appear *not* on the same baseline
  //    (which may indicate a font used for graph/chart data points)
  def buildGlyphTree(): Unit = {
    defineGlyphTrees()
    evalGlyphTrees()

    // val (fontName, _) = extract.FontDefs.splitScaledFontId(fontId)
    // val fontProperties = docScope.fontDefs
    //   .getFont(fontName)
    //   .map(_.reportShort().toString())
    //   .getOrElse("no font properties found")

    // println(
    //   s"font=${fontId}, name=${fontName}, baselineCount=${baselinesForFont.length}"
    // )
    // println(fontProperties)

    val report2 = pageScope.pageStats.vjumps.showBox()
    println("\n")
    println(report2.toString())
    println("\n\n==================\n")
  }

  def _addEdge(shape: AnyShape)(nn: Neighbor) =
    shape.modAttr(GlyphTreeEdges)(_.get :+ nn)

  def lookDown = Oct
    .withSearchRegions(Oct.cell(Dir.Bottom))
    .withHorizon(pageGeometry)

  def lookRight = Oct
    .withSearchRegions(Oct.cell(Dir.Right))
    .withHorizon(pageGeometry)

  def lookBottomRight = Oct
    .withSearchRegions(Oct.cell(Dir.BottomRight))
    .withHorizon(pageGeometry)

  protected def evalGlyphTrees(): Unit = {
    import Neighbor._
    for {
      bigramShape <- getLabeledRects(LB.GlyphBigram)
      edgeList    <- bigramShape.getAttr(GlyphTreeEdges).to(List)
      neighbor    <- edgeList
    } {

      neighbor match {
        case ByShape(s @ _) =>
        case BySearch(oct, search, action) =>
          val found = oct.runSearch(search)
          action(found)

        case ByIDOffset(offset @ _) =>
        case _                      =>
      }
    }
  }

  protected def defineGlyphTrees(): Unit = {
    val fontBaselines   = getLabeledLines(LB.CharRunFontBaseline)
    val sortedBaselines = sortShapesByFontOccurrence(fontBaselines)

    sortedBaselines.foreach({ case (baselinesForFont, fontId) =>
      val fontOffsets = docScope.fontDefs.getScaledFontOffsets(fontId)
      baselinesForFont.foreach(baseline => {

        val fontBaselineShape = baseline.asLineShape
        val fontBaseline      = fontBaselineShape.shape
        val offsets           = fontOffsets.forFontBoxBottom(fontBaseline.p1.y)

        val chars = fontBaselineShape.getAttr(ExtractedChars).getOrElse(Nil)

        chars.headOption.foreach(char0 => {
          val p  = char0.fontBbox.toPoint(Dir.BottomLeft)
          val ps = indexShape(p, LB.CharRunStartpoint)
          ps.setAttr(GlyphTreeEdges)(Nil)
          ps.setAttr(PrimaryFont)(fontId)

          _addEdge(ps)(
            Neighbor.BySearch(
              lookBottomRight.centeredOn(p.minBounds),
              searchForPoints(_, LB.CharRunStartpoint)
                .sortBy(r => r.shape.dist(p))
                .take(1),
              (_: Seq[AnyShape]).foreach(shape => {
                val shapeFontId = shape.getAttr(PrimaryFont).get
                val pn          = shape.asPointShape.shape
                val angleTo     = p.angleTo(pn)
                // pageScope.pageStats .addFontVJump(ngramFontId, glyphNGramTop, shapeFontId, shapeTop)
              })
            )
          )
        })
        chars.lastOption.foreach(charN => {
          val p  = charN.fontBbox.toPoint(Dir.BottomLeft)
          val ps = indexShape(p, LB.CharRunEndpoint)
          ps.setAttr(GlyphTreeEdges)(Nil)
          ps.setAttr(PrimaryFont)(fontId)

        })

        // Define the min-bounds glyph pair shapes, falling back to single-glyph
        //   min-bounds when there is just a single glyph in a font-baseline-run
        val glyphNGrams = chars
          .sliding(2)
          .to(Seq)
          .zipWithIndexADT
          .flatMap(_ match {
            case (Seq(c1, c2), pos) =>
              val pageSlice = getAscentDescentPageSlice(offsets).get

              val c1Rect  = c1.minBBox
              val c2Rect  = c2.minBBox
              val c12Rect = c1Rect union c2Rect

              val glyphRect = clipRectBetween(
                c12Rect.left,
                c12Rect.right,
                pageSlice
              ).get

              val glyphBigram = indexShape(glyphRect, LB.GlyphBigram)
              glyphBigram.setAttr(GlyphTreeEdges)(Nil)

              glyphBigram.setAttr(PrimaryFont)(fontId)

              def addEdge(nn: Neighbor) = _addEdge(glyphBigram)(nn)

              pos match {
                case SeqPos.First   => addEdge(Neighbor.ByIDOffset(-1))
                case SeqPos.Last(_) => addEdge(Neighbor.ByIDOffset(1))

                case _ =>
              }

              Seq(glyphBigram)

            case (Seq(c1), _) =>
              val pageSlice = getAscentDescentPageSlice(offsets).get

              val c1Rect = c1.minBBox

              val glyphRect = clipRectBetween(
                c1Rect.left,
                c1Rect.right,
                pageSlice
              ).get
              val glyph1Gram = indexShape(glyphRect, LB.Glyph1gram)
              glyph1Gram.setAttr(GlyphTreeEdges)(Nil)
              glyph1Gram.setAttr(PrimaryFont)(fontId)
              Seq(glyph1Gram)

            case _ =>
              Seq()
          })

        // create Next links
        glyphNGrams.zipWithIndexADT
          .foreach({ case (glyphNGram, pos) =>
            def addEdge(nn: Neighbor) = _addEdge(glyphNGram)(nn)

            // println(s"adding edges: glyph ${glyphNGram.id} @${pos} ")
            // Create 'down' links via search
            // Search down:
            //   - limit depth of search
            //   - sort by distance from center
            //   - specify what to do with results
            val ngramFontId: String @@ ScaledFontID = glyphNGram.getAttr(PrimaryFont).get
            val glyphNGramTop                       = glyphNGram.asRectShape.shape.top

            addEdge(
              Neighbor.BySearch(
                lookDown.centeredOn(glyphNGram.minBounds),
                searchForRects(_, LB.GlyphBigram)
                  .sortBy(r => r.shape.top.unwrap)
                  .take(1),
                (_: Seq[AnyShape]).foreach(shape => {
                  val shapeFontId = shape.getAttr(PrimaryFont).get
                  val shapeTop    = shape.asRectShape.shape.top
                  pageScope.pageStats
                    .addFontVJump(ngramFontId, glyphNGramTop, shapeFontId, shapeTop)
                })
              )
            )

            pos match {
              case SeqPos.First =>
                addEdge(Neighbor.ByIDOffset(-1))

              case SeqPos.Last(_) =>
                addEdge(Neighbor.ByIDOffset(1))

              case SeqPos.Sole =>
                addEdge(Neighbor.ByIDOffset(-1))
                addEdge(Neighbor.ByIDOffset(1))

              case _ =>
            }
          })

        glyphNGrams
          .sliding(2)
          .foreach({
            case Seq(gb1, gb2) =>
              _addEdge(gb1)(Neighbor.ByShape(gb2))

            case _ =>
          })

      })
    })
  }
}

// Neighborhood search types
sealed trait Neighbor

object Neighbor {
  case class ByShape(s: AnyShape)    extends Neighbor
  case class ByIDOffset(offset: Int) extends Neighbor

  // type ActionRes = (AnyShape, Rect)
  type ActionRes = AnyShape

  case class BySearch(
    oct: Oct,
    search: Rect => Seq[AnyShape],
    action: Seq[ActionRes] => Unit
  ) extends Neighbor

}
