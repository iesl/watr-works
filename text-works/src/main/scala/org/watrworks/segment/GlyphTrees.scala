package org.watrworks
package segment

import geometry.syntax._
import utils.IndexedSeqADT._

trait GlyphTrees extends GlyphRuns { self =>

  def buildGlyphTree(): Unit = {
    defineGlyphTrees()
  }

  protected def defineGlyphTrees(): Unit = {
    val startingLines = getLabeledLines(LB.CharRunFontBaseline)

    startingLines.foreach(fontBaseline => {

      val chars = getCharsForShape(fontBaseline)

      // Define the min-bounds glyph pair shapes, falling back to
      //   single-glyph min-bounds when there is just a single glyph in
      //   a font-baseline-run
      val fontGlyphNGrams = chars
        .sliding(2)
        .to(Seq)
        .zipWithIndexADT
        .flatMap(_ match {
          case (Seq(c1, c2), pos) =>
            val c1Rect  = c1.minBBox
            val c2Rect  = c2.minBBox
            val c12Rect = c1Rect union c2Rect

            val glyphBigram = indexShape(c12Rect, LB.GlyphBigram)

            // first and/or last glyph min-bounds in the group
            val bookendGlyphs = pos match {
              case SeqPos.First =>
                Seq(indexShape(c1Rect, LB.Glyph1gram))

              case SeqPos.Last(_) =>
                Seq(indexShape(c2Rect, LB.Glyph1gram))

              case SeqPos.Mid(_) =>
                Seq()

              case SeqPos.Sole =>
                Seq(indexShape(c1Rect, LB.Glyph1gram), indexShape(c2Rect, LB.Glyph1gram))
            }

            glyphBigram +: bookendGlyphs

          case (Seq(c1), _) =>
            Seq(indexShape(c1.minBBox, LB.Glyph1gram))

          case _ =>
            Seq()
        })

      // fontBaseline.linkedShapes(LB.Glyph)
      // linkage = label +  from: shape, at: point, to: shape at: point
      // glyphShape.attr(LB.Glyph)

    })
  }

}
