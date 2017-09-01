package edu.umass.cs.iesl.watr
package watrmarks


trait DocSegLabels {

  // FIXME <- nulllabel????
  val NullLabel = Label("null")

  val DocumentPages = Label("document-pages")
  val DocumentPage = Label("document-page")
  val Column = Label("ds", "column")

  val SectionHeadingLine = Label("ds", "section-heading-line")
  val SectionNumber = Label("ds", "section-number")
  val SectionTitle = Label("ds", "section-title")

  val Title = Label("ds", "title")
  val Author = Label("ds", "author")
  val Authors = Label("ds", "authors")

  val Affiliation = Label("ds", "affiliation")
  val Affiliations = Label("ds", "affiliations")
  val References = Label("ds", "references")

  val AbstractHeading = Label("ds", "abstract-heading")
  val Abstract = Label("ds", "abstract")

  val TextBlock = Label("ds", "text-block")
  val TextSpan = Label("ds", "text-span")
  val ParaBegin = Label("ds", "para-begin")
  val Para = Label("Para")
  val Caption = Label("Caption")
  val Image = Label("Image")
  val Table = Label("Table")

  val VisualLine = Label("ds", "visual-line")
  val VisualLines = Label("ds", "visual-lines")

  val VisualLineModal = Label("ds", "visual-line-modal")
  val SemanticLine = Label("ds", "semantic-line")


  val Token = Label("ds", "token")

  val Tokenized = Label("ds", "tokenized")
  val Sup = Label("ds", "sup-script")
  val Sub = Label("ds", "sub-script")

  val NoteMarkers = Label("note-markers")
  val NoteMarker = Label("note-marker")

  val PageAtom = Label("atom")
  val PageAtomGrp = Label("atom-grp")
  val PageAtomTmp = Label("atom-tmp")
  val Ordering = Label("Ordering")


  // Page Segmentation Labels
  val LineByHash             = Label("LineByHash")
  val LeftAlignedCharCol     = Label("LeftAlignedCharCol")
  val WhitespaceColCandidate = Label("WhitespaceColCandidate")
  val WhitespaceCol          = Label("WhitespaceCol")
  val WhitespaceColTop       = Label("WhitespaceColTop")
  val HPageDivider           = Label("HPageDivider")
  val WhitespaceColBottom    = Label("WhitespaceColBottom")
  val Marked                 = Label("Marked")


  // Ordering for blocks/lines on page
  val ReadingBlock         = Label("ReadingBlock")
  val ReadingBlocks        = Label("ReadingBlocks")
  val ReadingBlockLines    = Label("ReadingBlockLines")
  val PageLines            = Label("PageLines")
  val HasVisualLines       = Label("HasVisualLines")

  // val BlockReadingOrder      = Label("BlockReadingOrder")
  // val ReadingOrder           = Label("ReadingOrder")

  // Drawn path lines
  val PathBounds             = Label("PathBounds")
  val HLinePath              = Label("HLinePath")
  val VLinePath              = Label("VLinePath")
  val LinePath               = Label("LinePath")
  val OutlineBox             = Label("Outline")

  val Canonical             = Label("Canonical")
  val Tmp                   = Label("Tmp")


}

object StandardLabels extends DocSegLabels {

}
