package edu.umass.cs.iesl.watr
package watrmarks


trait DocSegLabels {
  val FullPdf        = Label.auto.withNamespace("seg")
  val VisualLine     = Label.auto.withNamespace("seg")
  val Sup            = Label.auto.withNamespace("seg")
  val Sub            = Label.auto.withNamespace("seg")

  val NullLabel      = Label.auto.withNamespace("seg")
  // val NoteMarkers    = Label.auto
  // val NoteMarker     = Label.auto
  // val Authors        = Label.auto
  // val Token          = Label.auto
  // val Author         = Label.auto
  // val Abstract       = Label.auto
  // val References     = Label.auto
  // val Affiliations   = Label.auto
  // val Title          = Label.auto
}

object StandardLabels extends DocSegLabels {

}
