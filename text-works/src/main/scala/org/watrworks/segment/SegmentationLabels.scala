package org.watrworks
package segment

import watrmarks._

object SegmentationLabels extends DocSegLabels {

  val Cluster  = Label.auto
  val Evidence = Label.auto
  val Attr     = Label.auto
  val Order    = Label.auto
  val Rel      = Label.auto
  val Has      = Label.auto

  val Caption = Label.auto
  val Image   = Label.auto

  val Para = Label.auto

  val CharRunFontBaseline = Label.auto
  val SymbolicGlyphLine   = Label.auto


  val Glyph                   = Label.auto
  val GlyphBigram             = Label.auto
  val Glyph1gram              = Label.auto
  val NatLangGlyph            = Label.auto
  val SymbolicGlyph           = Label.auto
  val ContiguousGlyphs        = Label.auto
  val ContiguousGlyphBaseline = Label.auto
  val CapDescenderBand        = Label.auto
  val BaselineMidriseBand     = Label.auto
  val TextLineGroup           = Label.auto

  val Fonts               = Label.auto qualifiedAs Attr
  val PrimaryFont         = Label.auto qualifiedAs Attr
  val FontBaselineOffsets = Label.auto qualifiedAs Attr
  val FontIndex           = Label.auto qualifiedAs Attr
  val LabeledIntervals    = Label.auto qualifiedAs Attr

  val ExtractedItems = Label.auto

  val PageAtom = Label.auto

  val LeftAlignedCharCol  = Label.auto
  val RightAlignedCharCol = Label.auto
  val PathBounds          = Label.auto

  val LinePairTrapezoid = Label.auto
  val WeightedLabels    = Label.auto qualifiedAs Attr
  val TextGridRow       = Label.auto qualifiedAs Attr

  val UpLeftChar         = Label.auto qualifiedAs Attr
  val LowRightChar       = Label.auto qualifiedAs Attr
  val UpLeftCharCapCase  = Label.auto qualifiedAs Attr
  val LowRightCharPeriod = Label.auto qualifiedAs Attr

}