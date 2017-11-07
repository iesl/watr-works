package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.Text.all._

object Frame {
  def htmlHead() = {
    <.head(
      <.meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
      <.meta(httpEquiv:="Content-Type", content:="text/html"),
      <.meta(httpEquiv:="X-UA-Compatible", content:="IE=edge"),
      <.meta(^.charset:="utf-8"),
      <.title("WatrColors"),
    )
  }

  def apply(bundleName: String) = {
    <.html(
      htmlHead(),
      <.body(
        <.script(`type` := "text/javascript", src := s"/dist/${bundleName}.bundle.js")
      ),
    )
  }

}
