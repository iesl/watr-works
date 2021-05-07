package org.watrworks
package segment

import geometry.{Octothorpe => Oct}
import geometry.syntax._
import utils.IndexedSeqADT._
import org.watrworks.watrmarks.Label

trait GlyphTrees extends GlyphRuns { self =>
  val Next  = Label.auto
  val Right = Label.auto
  val Left  = Label.auto
  val Down  = Label.auto

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
      val glyphNGrams = chars
        .sliding(2)
        .to(Seq)
        .zipWithIndexADT
        .flatMap(_ match {
          case (Seq(c1, c2), pos) =>
            val c1Rect  = c1.minBBox
            val c2Rect  = c2.minBBox
            val c12Rect = c1Rect union c2Rect

            val glyphBigram = indexShape(c12Rect, LB.GlyphBigram)

            pos match {
              case SeqPos.First =>
                glyphBigram.setAttr(
                  Next / Left,
                  NextNeighbors.ByIDOffset(-1)
                )

              case SeqPos.Last(_) =>
                glyphBigram.setAttr(
                  Next / Right,
                  NextNeighbors.ByIDOffset(1)
                )

              case _ =>
            }

            Seq(glyphBigram)

          case (Seq(c1), _) =>
            Seq(indexShape(c1.minBBox, LB.Glyph1gram))

          case _ =>
            Seq()
        })
      // TODO Next attribute stores list of out-edges. Edge contains the type (by-id, right, down, search)

      // create Next links
      glyphNGrams.zipWithIndexADT
        .foreach({ case (glyphNGram, pos) =>
          // Create 'down' links via search
          glyphNGram.setAttr(
            Next / Down,
            NextNeighbors.BySearch(
              Oct.focusedOn(
                Oct.Def(List(Oct.Bounds.Cell(Dir.Bottom))),
                glyphNGram.minBounds
              ),
              LB.GlyphBigram
            )
          )

          pos match {
            case SeqPos.First =>
              glyphNGram.setAttr(
                Next / Left,
                NextNeighbors.ByIDOffset(-1)
              )

            case SeqPos.Last(_) =>
              glyphNGram.setAttr(
                Next / Right,
                NextNeighbors.ByIDOffset(1)
              )

            case SeqPos.Sole =>
              glyphNGram.setAttr(
                Next / Left,
                NextNeighbors.ByIDOffset(-1)
              )
              glyphNGram.setAttr(
                Next / Right,
                NextNeighbors.ByIDOffset(1)
              )

            case _ =>
          }

        })

      glyphNGrams
        .sliding(2)
        .foreach({ case Seq(gb1, gb2) =>
          gb1.setAttr(
            Next / Right,
            NextNeighbors.ByShape(gb2)
          )
        })

    })
  }
}

// Neighborhood search types
sealed trait NextNeighbors

object NextNeighbors {
  case class ByShape(s: AnyShape)                  extends NextNeighbors
  case class ByIDOffset(offset: Int)               extends NextNeighbors
  case class BySearch(oct: Oct, shapeLabel: Label) extends NextNeighbors
}
