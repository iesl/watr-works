package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.stylesheet._
import scalatags.Text.all._

object WatrStyles extends CascadingStyleSheet {
//   body {
//     padding-top: 2rem;
//     padding-bottom: 2rem;
//   }

//   h3 {
//     margin-top: 2rem;
//   }

//   .row {
//     margin-bottom: 1rem;
//   }
//     .row .row {
//     margin-top: 1rem;
//     margin-bottom: 0;
//   }
//     [class*="col-"] {
//     padding-top: 1rem;
//     padding-bottom: 1rem;
//     background-color: rgba(86,61,124,.15);
//     border: 1px solid rgba(86,61,124,.2);
//   }

//   hr {
//     margin-top: 2rem;
//     margin-bottom: 2rem;
// }

  def container = cls(
    width := "100%"
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
      // <.script(`type`:="text/javascript", src:="/workbench.js")
    )
  }

  def navbar()  = {
    <.nav(^.`class`:="navbar navbar-fixed-top navbar-dark bg-inverse")(
      <.a(^.`class`:="navbar-brand", ^.`href`:="#")("Project name"),
      <.ul(^.`class`:="nav navbar-nav")(
        <.li(^.`class`:="nav-item active")(
          <.a(^.`class`:="nav-link", ^.`href`:="#")("Home", <.span(^.`class`:="sr-only")("(current)"))
        ),
        <.li(^.`class`:="nav-item")(
          <.a(^.`class`:="nav-link", ^.`href`:="#")("About")
        ),
        <.li(^.`class`:="nav-item")(
          <.a(^.`class`:="nav-link", ^.`href`:="#")("Contact")
        )
      )
    )
  }

  def bodyContent() = {
    <.div(^.`class`:="container", WatrStyles.container)(
      <.h3("Two columns"),
      <.p("Get two columns starting at desktops and scaling to large desktops)."),
      <.div(^.`class`:="row")(
        <.div(WatrStyles.colN, ^.`class`:="col-md-8")(".col-md-8"),
        <.div(WatrStyles.colN, ^.`class`:="col-md-4")(".col-md-4")
      ),

      <.h3("Full width, single column"),
      <.p(^.`class`:="text-warning")(
        "No grid classes are necessary for full-width elements."
      ),
      <.div(^.id := "main")("Loading..."),
      <.script(`type` := "text/javascript")(
        raw("edu.umass.cs.iesl.watr.watrcolors.client.WatrTableClient().main()")
      )
    )

  }

  def apply() = {
    <.html(
      htmlHead(),
      <.body(WatrStyles.htmlBody)(
        navbar(),
        bodyContent()
      )
    )
  }

  def apply2() = {
    <.html(^.lang := "en")(
      <.head(
        ^.lang := "en",
        <.title("WatrColors"),
        <.meta(content := "width=device-width, initial-scale=1", name := "viewport"),
        <.meta(httpEquiv := "Content-Type", content := "text/html; charset=UTF-8"),
        <.script(`type` := "text/javascript", src := "/assets/watrcolors-fastopt.js"),
        <.script(`type` := "text/javascript", src := "/webjars/mousetrap/1.6.0/mousetrap.min.js"),
        <.script(`type` := "text/javascript", src := "/webjars/jquery/2.2.4/jquery.min.js"),
        <.script(`type` := "text/javascript", src := "/webjars/fabric/1.6.2/dist/fabric.js"),

        <.link(rel := "stylesheet", `type` := "text/css", href := "/webjars/bootstrap/3.3.7/css/bootstrap.min.css"),

        <.style(^.`type` := "text/css", WatrStyles.styleSheetText)
      ),

      body(margin := 0, WatrStyles.htmlBody)(
        <.div(^.id := "main")("Loading..."),
        <.script(`type` := "text/javascript")(
          raw("edu.umass.cs.iesl.watr.watrcolors.client.WatrTableClient().main()")
        )
      )
    )
  }
}
