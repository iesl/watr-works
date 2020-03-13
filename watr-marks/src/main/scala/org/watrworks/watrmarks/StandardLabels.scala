package edu.umass.cs.iesl.watr
package watrmarks


trait DocSegLabels {
  val FullPdf        = Label.auto.withNamespace("seg")
  val VisualLine     = Label.auto.withNamespace("seg")
  val Sup            = Label.auto.withNamespace("seg")
  val Sub            = Label.auto.withNamespace("seg")

  val NullLabel      = Label.auto.withNamespace("seg")
}

object StandardLabels extends DocSegLabels {

}
