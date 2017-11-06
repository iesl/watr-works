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
    )
  }

  def apply(pageName: String, user: Option[String]) = {
    <.html(
      htmlHead(),
      <.body(
        <.div(id := "menu")
      ),
      <.script(`type` := "text/javascript", src := "/dist/menu.bundle.js")
    )
  }

}
