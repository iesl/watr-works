package edu.umass.cs.iesl.watr
package watrcolors
package html

// import scalatags.stylesheet.{CascadingStyleSheet, StyleSheet, StyleSheetTags, Sheet, Selector}
import scalatags.stylesheet.{CascadingStyleSheet, Sheet}

object Frame {

  import scalatags.Text.all._

  val WatrStyles = Sheet[WatrStyles]

  trait WatrStyles extends CascadingStyleSheet {

    def htmlBody = cls(
      (html ~ body)(
        height := "100%",
        minHeight := "100%",
        margin := 0,
        padding := 0
      )
    )
  }

  def apply() = {
    <.html(
      ^.lang := "en",
      <.head(
        <.title("WatrColors"),
        <.meta(content := "width=device-width, initial-scale=1", name := "viewport"),
        <.meta(httpEquiv := "Content-Type", content := "text/html; charset=UTF-8"),
        <.script(`type` := "text/javascript", src := "//localhost:12345/workbench.js"),
        <.script(`type` := "text/javascript", src := "/assets/watrcolors-client-fastopt.js"),
        <.script(`type` := "text/javascript", src := "/webjars/mousetrap/1.5.3/mousetrap.min.js"),
        <.script(`type` := "text/javascript", src := "/webjars/jquery/2.2.3/jquery.min.js"),
        <.script(`type` := "text/javascript", src := "/webjars/fabric/1.6.2/dist/fabric.js"),

        <.link(rel := "stylesheet", `type` := "text/css", href := "/webjars/bootstrap/3.3.6/css/bootstrap.min.css"),

        <.style(^.`type` := "text/css", WatrStyles.styleSheetText)
      ),

      body(margin := 0, WatrStyles.htmlBody)(
        <.div(^.id := "main")("Loading..."),
        script("edu.umass.cs.iesl.watr.watrcolors.client.WatrColorClient().main()")
        // <.script(^.`type`:="text/javascript")("edu.umass.cs.iesl.watr.watrcolors.client.WatrColorClient().main()"
        //   // raw(s""" edu.umass.cs.iesl.watr.watrcolors.client.WatrColorClient().main() """)
        // )
      )
    )
  }
}
