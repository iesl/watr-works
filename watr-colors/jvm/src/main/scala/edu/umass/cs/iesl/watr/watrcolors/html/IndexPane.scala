package edu.umass.cs.iesl.watr
package watrcolors
package html

// import org.scalajs.jquery.jQuery
// import scalatags.stylesheet.{CascadingStyleSheet, StyleSheet, StyleSheetTags, Sheet, Selector}




object IndexPane {


  import scalatags.Text.all._


  def apply()  = {
    <.div(
      <.div()("Query")(
        <.input()
      ),
      <.div()("Explore")(
        "directory cursor"
      )
    )

  }

}
