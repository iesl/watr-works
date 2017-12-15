package edu.umass.cs.iesl.watr
package watrmarks


trait DocSegLabels {
  val DocumentPages  = Label.auto
  val VisualLine     = Label.auto
  val Sup            = Label.auto
  val Sub            = Label.auto

  val NullLabel      = Label.auto
  val NoteMarkers    = Label.auto
  val NoteMarker     = Label.auto
  val Authors        = Label.auto
  val Token          = Label.auto
  val Author         = Label.auto
  val Abstract       = Label.auto
  val References     = Label.auto
  val Affiliations   = Label.auto
  val Title          = Label.auto
}

object StandardLabels extends DocSegLabels {

}
