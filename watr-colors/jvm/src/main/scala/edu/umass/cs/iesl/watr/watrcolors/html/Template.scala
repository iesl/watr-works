package edu.umass.cs.iesl.watr
package watrcolors
package html

import org.scalajs.jquery.jQuery
// import SplitPane._

// import scalatags.stylesheet.Sheet._
import scalatags.stylesheet.{CascadingStyleSheet, StyleSheet, StyleSheetTags, Sheet, Selector}

object Template {
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

      def topComponent = cls (
        bottom := "20em",
        marginBottom := "3px"
      )

      def horizontalDivider = cls (
        bottom := "20em",
        height := "3px",
        width := "100%"
      )

      def bottomComponent = cls (
        height := "20em"
      )
      def leftComponent = cls (
        width := "10em"
      )
      def verticalDivider = cls (
        left := "10em",
        height := "100%",
        width := "3px"
      )
      def rightComponent = cls (
        left := "10em",
        marginLeft := "3px"
      )

      def topComponent2 = cls (
        bottom := "50%",
        marginBottom := "3px"
      )
      def horizontalDivider2 = cls (
        bottom := "50%",
        height := "3px",
        width := "100%"
      )
      def bottomComponent2 = cls (
        height := "50%"
      )

    // def innerComponent = cls (
    //   height := "100%",
    //   width := "100%"
    // )

    // def x = cls(
    //   (html ~ body)(
    //     height:= "100%",
    //     minHeight:= "100%",
    //     margin:= 0,
    //     padding:= 0
    //   ),
    //   Selector("#top-component") (
    //     bottom := "20em",
    //     marginBottom := "3px"
    //   ),
    //   Selector("#horizontal-divider") (
    //     bottom := "20em",
    //     height := "3px"
    //   ),
    //   Selector("#bottom-component") (
    //     height := "20em"
    //   ),
    //   Selector("#left-component") (
    //     width := "10em"
    //   ),
    //   Selector("#vertical-divider") (
    //     left := "10em",
    //     width := "3px"
    //   ),
    //   Selector("#right-component") (
    //     left := "10em",
    //     marginLeft := "3px"
    //   ),

    //   Selector("#top-component2") (
    //     bottom := "50%",
    //     marginBottom := "3px"
    //   ),
    //   Selector("#horizontal-divider2") (
    //     bottom := "50%",
    //     height := "3px"
    //   ),
    //   Selector("#bottom-component2") (
    //     height := "50%"
    //   )
    // )
  }


  val txt =
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
          <.div(^.`class`:="pretty-split-pane-frame")(
            <.div(^.`class`:="split-pane fixed-bottom", ^.`id`:="split-pane-1")(
              <.div(^.`class`:="split-pane-component", ^.`id`:="top-component", WatrStyles.topComponent)(
                <.div(^.`class`:="pretty-split-pane-component-inner")(
                  <.p("Split panes can be nested. Re-size the browser window and drag the dividers to see the effect."),
                  <.p("Look at the source of this resource to learn about setting them up.")
                )
              ),
              <.div(^.`class`:="split-pane-divider", ^.`id`:="horizontal-divider", WatrStyles.horizontalDivider),
              <.div(^.`class`:="split-pane-component", ^.`id`:="bottom-component", WatrStyles.bottomComponent)(
                <.div(^.`class`:="split-pane fixed-left", ^.`id`:="split-pane-2")(
                  <.div(^.`class`:="split-pane-component", ^.`id`:="left-component", WatrStyles.leftComponent)(
                    <.div(^.`class`:="pretty-split-pane-component-inner")("xx")
                  ),
                  <.div(^.`class`:="split-pane-divider", ^.`id`:="vertical-divider", WatrStyles.verticalDivider) ,
                  <.div(^.`class`:="split-pane-component", ^.`id`:="right-component", WatrStyles.rightComponent)(
                    <.div(^.`class`:="split-pane horizontal-percent")(
                      <.div(^.`class`:="split-pane-component", ^.`id`:="top-component2", WatrStyles.topComponent2)(
                        <.div(^.`class`:="pretty-split-pane-component-inner")("yy")
                      ),
                      <.div(^.`class`:="split-pane-divider", ^.`id`:="horizontal-divider2", WatrStyles.horizontalDivider2),
                      <.div(^.`class`:="split-pane-component", ^.`id`:="bottom-component2", WatrStyles.bottomComponent2)(
                        <.div(^.`class`:="pretty-split-pane-component-inner")("zz")
                      )
                    )
                  )
                )
              )
            )
          )

          // div(id:="main", `class`:="container-fluid")(
          //   div(id:="win-0", `class`:="container-fluid")(
          //     span(id:="currfile")
          //   ),
          //   span(id:="winstack-top")
          // ),
        )

      )

}
