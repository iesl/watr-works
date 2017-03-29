package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.stylesheet._
import scalatags.Text.all._
import utils.Colors

object WatrStyles extends CascadingStyleSheet {
  val C = Colors
  initStyleSheet()


  def statusBar = cls(
    position.fixed,
    overflow.hidden,
    margin          := 0,
    padding         := 0,
    top             := 0,
    width           := "100%",
    height          := "30px",
    zIndex          := 9999,
    backgroundColor := "#"+C.LightSteelBlue1.toHex
  )

  def statusText = cls(
    position.fixed,
    overflow.hidden,
    margin          := 0,
    padding         := 0,
    top             := 0,
    width           := "100%",
    height          := "30px",
    zIndex          := 9999,
    left            := "30%",
    backgroundColor := "#"+C.LightSteelBlue3.toHex
  )

  def statusCtrls = cls(
    position.fixed,
    overflow.hidden,
    margin          := 0,
    padding         := 0,
    top             := 0,
    width           := "100%",
    height          := "30px",
    zIndex          := 9999,
    left            := 0,
    backgroundColor := "#"+C.LightSteelBlue2.toHex
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

  def mainContent = cls(
    width := "100%",
    padding:="0",
    border:="0",
    // margin:="0",
    marginTop := "30px"
  )

  def canvasContainer = cls(
    padding:="0",
    border:="0",
    margin:="0",
    position.relative
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
      <.link(rel := "stylesheet", `type` := "text/css", href := "/assets/css/main.css"),
      <.style(^.`type` := "text/css", WatrStyles.styleSheetText)
    )
  }

  // def statusbar()  = {
  //   <.div(^.id:="status-bar", WatrStyles.statusBar)(
  //     <.div(^.id:="status-controls", WatrStyles.statusCtrls),
  //     <.div(^.id:="status-text", WatrStyles.statusText)
  //   )
  // }


  // def bodyContent() = {
  //   <.div(WatrStyles.mainContent)(
  //     <.div(^.id:="canvas-container", WatrStyles.canvasContainer)(
  //       // <.canvas(^.style:="display: block", ^.id:="canvas", ^.width:="1000", ^.height:="1000", WatrStyles.fabricCanvas)
  //       <.canvas(^.id:="canvas", WatrStyles.fabricCanvas)
  //     )
  //   )
  // }

  def apply() = {
    <.html(
      htmlHead(),
      <.body(
        WatrStyles.htmlBody,
        ^.onload:="WatrColors.main();")
    )
  }
}
