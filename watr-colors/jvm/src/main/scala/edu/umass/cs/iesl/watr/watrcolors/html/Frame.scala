package edu.umass.cs.iesl.watr
package watrcolors
package html

import org.scalajs.jquery.jQuery

import scalatags.stylesheet.{CascadingStyleSheet, StyleSheet, StyleSheetTags, Sheet, Selector}

object Frame {

  import scalatags.Text.all._

  val WatrStyles = Sheet[WatrStyles]

  trait WatrStyles extends CascadingStyleSheet {

    def htmlBody = cls(
      (html ~ body)(
        height:= "100%",
        minHeight:= "100%",
        margin:= 0,
        padding:= 0
      )
    )

  }

  val svgPanestr = <.div(

  )


  def apply(mainContent: TextTag) = {

      <.html(^.lang:="en",
        <.head(
          <.title("WatrColors"),
          <.meta(content:="width=device-width, initial-scale=1", name:="viewport"),
          <.meta(httpEquiv:="Content-Type", content:="text/html; charset=UTF-8"),
          <.script(`type`:="text/javascript", src:="//localhost:12345/workbench.js"),
          <.script(`type`:="text/javascript", src:="/watrcolors-client-fastopt.js"),
          <.script(`type`:="text/javascript", src:="/webjars/mousetrap/1.5.3/mousetrap.min.js"),
          <.script(`type`:="text/javascript", src:="/webjars/jquery/2.1.4/jquery.min.js"),

          // script(`type`:="text/javascript", src:="/webjars/jqueryui-layout/1.4.0/jquery.layout.js"),

          script(`type`:="text/javascript",            src:="/js/split-pane/split-pane.js"),
          link(rel:="stylesheet", `type`:="text/css", href:="/js/split-pane/split-pane.css"),
          link(rel:="stylesheet", `type`:="text/css", href:="/js/split-pane/pretty-split-pane.css"),
          // script(`type`:="text/javascript", src:="/webjars/split-pane/0.5.1/split-pane.js"),
          // link(rel:="stylesheet", `type`:="text/css", href:="/webjars/split-pane/0.5.1/split-pane.css"),
          // link(rel:="stylesheet", `type`:="text/css", href:="/webjars/split-pane/0.5.1/pretty-split-pane.css"),

          link(rel:="stylesheet", `type`:="text/css", href:="/webjars/bootstrap/3.3.6/css/bootstrap.min.css"),
            // script(`type`:="text/javascript", src:="/webjars/bootstrap/3.3.6/js/bootstrap.min.js")

          <.style(^.`type`:="text/css",
            WatrStyles.styleSheetText
          ),
          script("edu.umass.cs.iesl.watr.watrcolors.WatrColorClient().main()")
        ),


        body(margin:=0, WatrStyles.htmlBody)(
          mainContent
        )
      )


  }
}
