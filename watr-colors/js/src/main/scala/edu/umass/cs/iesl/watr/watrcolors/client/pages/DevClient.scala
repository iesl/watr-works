package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages

import parts._

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport

import scaladget.stylesheet.{all => sty}

import scalatags.JsDom.all._
import sty._

import watrmarks.{StandardLabels => LB}
// import rx._
import org.scalajs.dom.raw._

// import scaladget.tools.JsRxTags._

@JSExportTopLevel("DevClient")
object DevClient extends BaseClientDefs {
  import BootstrapBits._
  //

  def navEntry(): HTMLElement = {
    <.li(
      <.span(
        <.div("content")
      )
    ).render
  }

  @JSExport
  def display(): Unit = {
    println("hello from scalajs")

    withBootstrapNative {

      val pageName = navItem(span("Demo").render)

      val selectorControls = SharedLayout.zoneSelectorControls(
        new ZoneSelectionRx(),
        List(
          LB.Title,
          LB.Authors,
          LB.Abstract,
          LB.Affiliation,
          LB.References
        ))

      val nav = div()
      // SharedLayout.initNavbar(List(
      //   pageName,
      //   NavSpan(selectorControls)
      // ))

      val bodyContent = div(p("Main Body Content"))

      val sidebarContent =
        ul(`class`:="sidebar-nav")()

      SharedLayout.pageSetup(nav, bodyContent, sidebarContent).render
    }

  }
}
