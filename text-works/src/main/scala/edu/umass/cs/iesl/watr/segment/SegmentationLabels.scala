package edu.umass.cs.iesl.watr
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
  val Image = Label.auto

  val Para = Label.auto

  val ExtractedLineStarts     = Label.auto

  val CharRun                 = Label.auto
  val CharRunBegin            = Label.auto
  // val CharRunBaseline      = Label.auto

  val CharRunFontBaseline     = Label.auto
  val SymbolicGlyphLine       = Label.auto
  // val FontBaseline         = Label.auto

  val Glyph                   = Label.auto
  val NatLangGlyph            = Label.auto
  val SymbolicGlyph           = Label.auto
  val ContiguousGlyphs        = Label.auto
  val ContiguousGlyphBaseline = Label.auto

  val Fonts                   = Label.auto :: Attr

  val HasCharRunBaselines     = Label.auto

  val ExtractedItems          = Label.auto

  val VisualLines             = Label.auto

  val VisualLineModal         = Label.auto

  val NoteMarkers             = Label.auto
  val NoteMarker              = Label.auto

  val PageAtom                = Label.auto
  val PageAtomGrp             = Label.auto
  val PageAtomTmp             = Label.auto
  val Ordering                = Label.auto



  val LeftAlignedCharCol     = Label.auto
  val RightAlignedCharCol     = Label.auto
  val PathBounds             = Label.auto


  // val LineByHash             = Label.auto
  // val LeftAlignedColEnd      = Label.auto
  // val WhitespaceColCandidate = Label.auto
  // val WhitespaceCol          = Label.auto
  // val WhitespaceColTop       = Label.auto
  // val HPageDivider           = Label.auto
  // val WhitespaceColBottom    = Label.auto
  // val Marked                 = Label.auto
  // val LineStartHint          = Label.auto
  // val LineEndHint            = Label.auto

  // val ColLeftEvidence      = Label.auto
  // val ColRightEvidence      = Label.auto

  // // val LineEndEvidence        = Label.auto

  // val ReadingBlock           = Label.auto
  // val ReadingBlocks          = Label.auto
  // val ReadingBlockLines      = Label.auto
  // val PageLines              = Label.auto
  // val HasVisualLines         = Label.auto



  // val HLinePath              = Label.auto
  // val VLinePath              = Label.auto
  // val LinePath               = Label.auto
  // val OutlineBox             = Label.auto

  // val Canonical             = Label.auto
  // val Tmp                   = Label.auto
}
