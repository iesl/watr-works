package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.stylesheet._
import scalatags.Text.all._

object WatrStyles extends CascadingStyleSheet {
  initStyleSheet()


  def topBar = cls(
    position         := "fixed",
    top              := 0,
    left             := 0,
    width            := "100%",
    height           := 60,
    backgroundColor  := "black"
  )

  def colN = cls(
    paddingTop := "1rem",
    paddingBottom:= "1rem",
    backgroundColor:= "rgba(86,61,124,.15)",
    border:= "1px solid rgba(86,61,124,.2)"
  )

  def htmlBody = cls(
    (html ~ body)(
      height := "100%",
      minHeight := "100%",
      margin := 0,
      width := "100%",
      paddingTop := "2em",
      paddingBottom := "2rem"
    )
  )

  def container = cls(
    width := "100%",
    marginTop := 60,
    position.relative
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
      "WatrColors Status:"
    )
  }

  // def navbar()  = {
  //   <.nav(^.`class`:="navbar navbar-fixed-top navbar-dark bg-inverse")(
  //     <.a(^.`class`:="navbar-brand", ^.`href`:="#")("Project name"),
  //     <.ul(^.`class`:="nav navbar-nav")(
  //       <.li(^.`class`:="nav-item active")(
  //         <.a(^.`class`:="nav-link", ^.`href`:="#")("Home", <.span(^.`class`:="sr-only")("(current)"))
  //       ),
  //       <.li(^.`class`:="nav-item")(
  //         <.a(^.`class`:="nav-link", ^.`href`:="#")("About")
  //       ),
  //       <.li(^.`class`:="nav-item")(
  //         <.a(^.`class`:="nav-link", ^.`href`:="#")("Contact")
  //       )
  //     )
  //   )
  // }

  def bodyContent() = {
    <.div(
      statusbar(),
      <.div(WatrStyles.container)(

        <.div(^.id:="canvas-container", WatrStyles.canvasContainer)(
          <.canvas(^.style:="display: block", ^.id:="canvas", ^.width:="1000", ^.height:="1000")
        ),

        <.script(`type` := "text/javascript")(
          raw("edu.umass.cs.iesl.watr.watrcolors.client.WatrColors().main()")
        )

      )
    )

  }

  def apply() = {
    <.html(
      htmlHead(),
      <.body(WatrStyles.htmlBody)(
        bodyContent()
      )
    )
  }
}
