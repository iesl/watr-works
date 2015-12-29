package edu.umass.cs.iesl.watr
package watrcolors
package html

import org.scalajs.jquery.jQuery

import scalatags.stylesheet.{CascadingStyleSheet, StyleSheet, StyleSheetTags, Sheet, Selector}


object Pane {

  import scalatags.Text.all._


  def apply(paneContent: TextTag) = {
    <.div(^.`class`:="pretty-split-pane-frame")(
      paneContent
    )
  }

}
