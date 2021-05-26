package org.watrworks
package segment

import geometry.syntax._
import utils.IndexedSeqADT._
import utils.{RelativeDirection => Dir}
import utils.ExactFloats._
import org.watrworks.extract.FontBaselineOffsets

trait GlyphTrees extends GlyphRuns with LineSegmentation with GlyphGraphSearch { self =>

  import utils.GuavaHelpers._

  // TODO how many times does a run of glyphs of the same font appear *not* on the same baseline
  //    (which may indicate a font used for graph/chart data points)
  def buildGlyphTree(): Unit = {
    defineGlyphTrees()
    evalGlyphTrees()

    println("Font Backslash Angles \n")
    println(fontBackslashAngle.table.showAsList().toString())
    println("\n\n==================\n")
  }

  protected def evalGlyphTrees(): Unit = {
    expandEdges()
  }

  protected def defineGlyphTrees(): Unit = {
    val fontBaselines   = getLabeledLines(LB.CharRunFontBaseline)
    val sortedBaselines = sortShapesByFontOccurrence(fontBaselines)

    // Define 'leftmost' char-runs
    sortedBaselines.foreach({ case (baselinesForFont, baselineFontId) =>
      val fontOffsets: FontBaselineOffsets = docScope.fontDefs.getScaledFontOffsets(baselineFontId)

      baselinesForFont.foreach(baseline => {

        val fontBaselineShape = baseline.asLineShape
        val fontBaseline      = fontBaselineShape.shape

        addFindLeftmostEdge(baseline, baselineFontId)

        val chars = fontBaselineShape.getAttr(ExtractedChars).getOrElse(Nil)

        chars.headOption.foreach(char0 => {
          val p = char0.fontBbox.toPoint(Dir.BottomLeft)
          initNodeShape(p, LB.CharRunStartpoint, Some(baselineFontId))
        })

        chars.lastOption.foreach(charN => {
          val p = charN.fontBbox.toPoint(Dir.BottomRight)
          initNodeShape(p, LB.CharRunEndpoint, Some(baselineFontId))
        })

        val offsets                = fontOffsets.forFontBoxBottom(fontBaseline.p1.y)
        val ascentDescentPageSlice = getAscentDescentPageSlice(offsets).get
        // Define the min-bounds glyph pair shapes, falling back to single-glyph
        //   min-bounds when there is just a single glyph in a font-baseline-run
        val glyphNGrams = chars
          .sliding(2)
          .to(Seq)
          .flatMap(_ match {
            case Seq(c1, c2) =>
              val glyphRect = boundingRect(ascentDescentPageSlice, c1.minBBox, c2.minBBox).get

              Seq(initNodeShape(glyphRect, LB.GlyphBigram, Some(baselineFontId)))

            case Seq(c1) =>
              val glyphRect = boundingRect(ascentDescentPageSlice, c1.minBBox).get

              Seq(initNodeShape(glyphRect, LB.Glyph1gram, Some(baselineFontId)))

            case _ =>
              Seq()
          })

        glyphNGrams.zipWithIndexADT
          .foreach({ case (glyphNGram, pos) =>
            addFontVJumpEdge(glyphNGram)

            def addEdge(nn: Neighbor) = _addEdge(glyphNGram)(nn)

            pos match {
              case SeqPos.First   => addEdge(Neighbor.ByIDOffset(-1))
              case SeqPos.Last(_) => addEdge(Neighbor.ByIDOffset(1))

              case SeqPos.Sole =>
                addEdge(Neighbor.ByIDOffset(-1))
                addEdge(Neighbor.ByIDOffset(1))

              case _ =>
            }
          })

        glyphNGrams
          .sliding(2)
          .foreach({
            case Seq(gb1, gb2) => _addEdge(gb1)(Neighbor.ByShape(gb2))
            case _             =>
          })

      })
    })
  }

}
