package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.stylesheet._
import scalatags.Text.all._

object WatrStyles extends CascadingStyleSheet {

  def htmlBody = cls(
    (html ~ body)(
      height := "100%",
      minHeight := "100%",
      margin := 0,
      padding := 0
    )
  )

  def overlayContainer = cls(
    padding:="0",
    border:="0",
    margin:="0",
    position.relative
  )


  def pageImg = cls(
    padding:="0",
    margin:="0",
    border := "1px solid #000",
    width := "100%",
    display:="block",
    zIndex:=10
  )

  def imgContainer = cls(
    position.absolute,
    left:="0",
    top:="0",
    padding:="0",
    margin:="0",
    border := "1px solid #000",
    zIndex:=0,
    backgroundColor := "ivory",
    display:="inline-block"
  )

  def fabricCanvas = cls(
    position.absolute,
    padding:="0",
    margin:="0",
    border := "0",
    left:="0",
    zIndex:=100,
    top:="0"
  )

  def infoPane = cls(
    position.absolute,
    padding:="0",
    margin:="0",
    borderTop := "2px solid #AAA",
    borderLeft := "2px solid #AAA",
    left:="0",
    top:="0"
  )
}

object Frame {


  // val WatrStyles = Sheet[WatrStyles]


  def apply() = {
    <.html(
      ^.lang := "en",
      <.head(
        <.title("WatrColors"),
        <.meta(content := "width=device-width, initial-scale=1", name := "viewport"),
        <.meta(httpEquiv := "Content-Type", content := "text/html; charset=UTF-8"),
        <.script(`type` := "text/javascript", src := "//localhost:12345/workbench.js"),
        <.script(`type` := "text/javascript", src := "/assets/watrcolors-client-fastopt.js"),
        <.script(`type` := "text/javascript", src := "/webjars/mousetrap/1.6.0/mousetrap.min.js"),
        <.script(`type` := "text/javascript", src := "/webjars/jquery/2.2.4/jquery.min.js"),
        <.script(`type` := "text/javascript", src := "/webjars/fabric/1.6.2/dist/fabric.js"),

        <.link(rel := "stylesheet", `type` := "text/css", href := "/webjars/bootstrap/3.3.7/css/bootstrap.min.css"),

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

object ShellHtml {


  // val WatrStyles = Sheet[WatrStyles]


  def apply() = {
    <.html(
      ^.lang := "en",
      <.head(
        <.title("WatrColors"),
        <.meta(content := "width=device-width, initial-scale=1", name := "viewport"),
        <.meta(httpEquiv := "Content-Type", content := "text/html; charset=UTF-8"),
        <.script(`type` := "text/javascript", src := "/assets/watrcolors-client-fastopt.js"),
        <.script(`type` := "text/javascript", src := "/webjars/mousetrap/1.6.0/mousetrap.min.js"),
        <.script(`type` := "text/javascript", src := "/webjars/jquery/2.2.4/jquery.min.js"),
        <.script(`type` := "text/javascript", src := "/webjars/fabric/1.6.2/dist/fabric.js"),

        <.link(rel := "stylesheet", `type` := "text/css", href := "/webjars/bootstrap/3.3.7/css/bootstrap.min.css"),

        <.style(^.`type` := "text/css", WatrStyles.styleSheetText)
      ),

      body(margin := 0, WatrStyles.htmlBody)(
        <.div(^.id := "main")("Loading..."),
        script("edu.umass.cs.iesl.watr.watrcolors.client.WatrTableClient().main()")
      )
    )
  }
}
