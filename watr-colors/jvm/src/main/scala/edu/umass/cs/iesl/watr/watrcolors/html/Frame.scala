package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.stylesheet._
import scalatags.Text.all._

object WatrStyles extends CascadingStyleSheet {
  initStyleSheet()


  def topBar = cls(
    position.fixed,
    overflow.hidden,
    margin          := 0,
    padding         := 0,
    top             := 0,
    left            := 0,
    width           := "100%",
    height          := "30px",
    zIndex          := 9999,
    backgroundColor := "#eebbcc"
  )


  def htmlBody = cls(
    (html ~ body)(
      height := "100%",
      minHeight := "100%",
      margin := 0,
      width := "100%"
      // paddingTop := "2em",
      // paddingBottom := "2rem"
    )
  )

  def container = cls(
    width := "100%",
    paddingTop := "30px",
    marginTop := 16
  )

  def canvasContainer = cls(
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

  def fabricCanvas = cls(
    position.absolute,
    padding:="0",
    margin:="0",
    border := "0",
    left:="0",
    zIndex:=100,
    top:="0"
  )

}

object ShellHtml {
  def htmlHead() = {
    <.head(
      <.meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
      <.meta(httpEquiv:="Content-Type", content:="text/html"),
      <.meta(httpEquiv:="X-UA-Compatible", content:="IE=edge"),
      <.meta(^.charset:="utf-8"),
      <.title("WatrColors"),
      <.script(`type` := "text/javascript", src := "/assets/watrcolors-fastopt.js"),
      <.script(`type` := "text/javascript", src := "/webjars/mousetrap/1.6.0/mousetrap.min.js"),
      <.script(`type` := "text/javascript", src := "/webjars/jquery/2.2.4/jquery.min.js"),
      <.script(`type` := "text/javascript", src := "/webjars/fabric/1.6.2/dist/fabric.js"),

      <.link(rel := "stylesheet", `type` := "text/css", href := "/webjars/bootstrap/3.3.7/css/bootstrap.min.css"),

      <.style(^.`type` := "text/css", WatrStyles.styleSheetText)
    )
  }

  def statusbar()  = {
    <.div(WatrStyles.topBar)(
      "WatrColors Status:", <.span(^.id:="status-text")
    )
  }


  def bodyContent() = {
    <.div(WatrStyles.container)(

      <.div(^.id:="canvas-container", WatrStyles.canvasContainer)(
        <.canvas(^.style:="display: block", ^.id:="canvas", ^.width:="1000", ^.height:="1000")
      ),

      <.script(`type` := "text/javascript")(
        raw("edu.umass.cs.iesl.watr.watrcolors.client.WatrColors().main()")
      )

    )

  }

  def apply() = {
    <.html(
      htmlHead(),
      <.body(WatrStyles.htmlBody)(
        statusbar(),
        bodyContent()
      )
    )
  }
}
