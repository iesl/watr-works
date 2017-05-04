package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.stylesheet._
import scalatags.Text.all._
import utils.Colors

object WatrStyles extends CascadingStyleSheet {
  val C = Colors
  initStyleSheet()

  def htmlBody = cls(
    (html ~ body)(
      height := "100%",
      minHeight := "100%",
      margin := 0,
      width := "100%"
    )
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
      <.script(`type` := "text/javascript", src := "/webjars/mousetrap/1.6.0/mousetrap.min.js"),
      <.script(`type` := "text/javascript", src := "/webjars/jquery/2.2.4/jquery.min.js"),
      <.script(`type` := "text/javascript", src := "/webjars/fabric/1.6.2/dist/fabric.js"),

      <.link(rel := "stylesheet", `type` := "text/css", href := "/webjars/bootstrap/3.3.7/css/bootstrap.min.css"),
      <.link(rel := "stylesheet", `type` := "text/css", href := "/assets/css/main.css"),
      <.link(rel := "stylesheet", `type` := "text/css", href := "/assets/css/simple-sidebar.css"),

      <.script(`type` := "text/javascript", src := "/assets/watrcolors-fastopt.js"),

      <.style(^.`type` := "text/css", WatrStyles.styleSheetText)
    )
  }

  def apply(pageName: String) = {
    <.html(
      htmlHead(),
      <.body(WatrStyles.htmlBody, ^.onload:=s"${pageName}.display();")
    )
  }


  def register() = {
    <.html(
      htmlHead(),
      <.body(WatrStyles.htmlBody, ^.onload:=s"Registration.display();")
    )

  }
}
