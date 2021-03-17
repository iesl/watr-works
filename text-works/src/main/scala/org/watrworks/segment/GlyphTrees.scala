package org.watrworks
package segment

import geometry._
import geometry.syntax._
import extract._
// import utils.ExactFloats._
// import watrmarks._
import utils.SlicingAndDicing._
// import TypeTags._

trait GlyphTrees extends GlyphRuns { self =>

  def buildGlyphTree(): Unit = {
    println("buildGlyphTree")
    labelGlyphRuns()
  }

  protected def labelGlyphRuns(): Unit = {
    pageScope.pageItems.toSeq
      .sliding(2)
      .foreach({
        case Seq(item1: ExtractedItem.CharItem, item2: ExtractedItem.CharItem) =>
          println(s"labelGlyphRuns: char/char")
          val bothNatLang      = item1.fontProps.isNatLangFont() && item2.fontProps.isNatLangFont()
          val isSimilarPair    = item2.glyphProps.prevSimilar == item1.id
          val fontOffsets1     = docScope.fontDefs.getScaledFontOffsets(item1.scaledFontId)
          val adjustedOffsets1 = fontOffsets1.forFontBoxBottom(item1.glyphProps.fontBBox.bottom)
          if (isSimilarPair) {
            //

          } else {

            val fontOffsets2     = docScope.fontDefs.getScaledFontOffsets(item2.scaledFontId)
            val adjustedOffsets2 = fontOffsets2.forFontBoxBottom(item2.glyphProps.fontBBox.bottom)
          }

        // indexShapeAndSetItems(item.minBBox, LB.SymbolicGlyph, item)
        case Seq(item1: ExtractedItem, item2: ExtractedItem.CharItem) =>
          println(s"labelGlyphRuns: item/char")
        // case Seq(item1: ExtractedItem.CharItem, item2: ExtractedItem) =>
        case _ => // noop
      })

  }

}
