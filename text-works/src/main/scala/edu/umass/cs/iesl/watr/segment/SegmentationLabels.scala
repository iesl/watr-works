package edu.umass.cs.iesl.watr
package segment

import watrmarks.Label


object SegmentationLabels {

  val Caption = Label.auto
  val Image = Label.auto
  val Table = Label.auto

  val DocumentPages = Label.auto
  val Para = Label.auto

  val ExtractedLine = Label.auto
  val ExtractedLineStarts    = Label.auto
  val ExtractedLineEnds      = Label.auto

  val CharRun           = Label.auto
  val CharRunBegin      = Label.auto
  val CharRunEnd        = Label.auto
  val CharRunBaseline   = Label.auto
  val CharRunBeginVLine = Label.auto
  val CharRunBeginVLineToBaseline = Label.auto
  val VisualBaseLine    = Label.auto

  val ExtractedItems    = Label.auto

  val VisualLine        = Label.auto
  val VisualLines       = Label.auto

  val VisualLineModal = Label.auto
  val Sup             = Label.auto
  val Sub             = Label.auto

  val NoteMarkers     = Label.auto
  val NoteMarker      = Label.auto

  val PageAtom        = Label.auto
  val PageAtomGrp     = Label.auto
  val PageAtomTmp     = Label.auto
  val Ordering        = Label.auto



  val LineByHash             = Label.auto
  val LeftAlignedCharCol     = Label.auto
  val LeftAlignedColEnd      = Label.auto
  val WhitespaceColCandidate = Label.auto
  val WhitespaceCol          = Label.auto
  val WhitespaceColTop       = Label.auto
  val HPageDivider           = Label.auto
  val WhitespaceColBottom    = Label.auto
  val Marked                 = Label.auto
  val LineStartHint          = Label.auto
  val LineEndHint            = Label.auto

  val ColLeftEvidence      = Label.auto
  val ColRightEvidence      = Label.auto

  // val LineEndEvidence        = Label.auto

  val ReadingBlock           = Label.auto
  val ReadingBlocks          = Label.auto
  val ReadingBlockLines      = Label.auto
  val PageLines              = Label.auto
  val HasVisualLines         = Label.auto



  val PathBounds             = Label.auto
  val HLinePath              = Label.auto
  val VLinePath              = Label.auto
  val LinePath               = Label.auto
  val OutlineBox             = Label.auto

  val Canonical             = Label.auto
  val Tmp                   = Label.auto
}
