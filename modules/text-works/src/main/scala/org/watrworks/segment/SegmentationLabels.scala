package org.watrworks
package segment

import watrmarks._

object SegmentationLabels extends DocSegLabels {

  val Cluster  = Label.auto
  val Evidence = Label.auto

  val Caption = Label.auto
  val Image   = Label.auto
  val Para    = Label.auto

  val CharRunFontBaseline        = Label.auto
  val CharRunFontLeadingBaseline = Label.auto
  val CharRunStartpoint          = Label.auto
  val CharRunEndpoint            = Label.auto
  val SymbolicGlyphLine          = Label.auto

  val Glyph               = Label.auto
  val GlyphBigram         = Label.auto
  val Glyph1gram          = Label.auto
  val NatLangGlyph        = Label.auto
  val BaselineMidriseBand = Label.auto
  val MonoFontTextLattice = Label.auto
  val AscentDescentBand   = Label.auto
  val TextLineGroup       = Label.auto

  val LowerSkyline  = Label.auto

  val LeftAlignedCharCol  = Label.auto
  val RightAlignedCharCol = Label.auto
  val PathBounds          = Label.auto

  val LinePairTrapezoid = Label.auto
}
