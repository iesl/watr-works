package edu.umass.cs.iesl.watr
package textboxing


import textboxing.{TextBoxing => TB}, TB._

object TextBoxingLayouts {

  def hangingIndent(heading: String, items: Seq[Box]): Box = {
    heading.atop(indent(4,
      vjoins(items)
    ))
  }
}
