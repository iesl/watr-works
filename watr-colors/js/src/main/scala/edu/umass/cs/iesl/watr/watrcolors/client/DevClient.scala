package edu.umass.cs.iesl.watr
package watrcolors
package client


import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport
import scaladget.api._

import scaladget.stylesheet.{all => sty}
import scaladget.api.{BootstrapTags => bs}

import scalatags.JsDom.all._
// import scalatags.JsDom.{
//   TypedTag
// }
import bs._
import sty._

// import rx._
// import rx.ops._
// import scaladget.tools.JsRxTags._

@JSExportTopLevel("DevClient")
object DevClient extends SharedClientDefs {
  //

  def selectionConstraintChoice(): SelectableButtons = {
    val radios: SelectableButtons = bs.radios()(
      bs.selectableButton("Line", true, onclick = () =>{}),
      bs.selectableButton("Char", onclick = () =>{}),
      bs.selectableButton("None", onclick = () => {})
    )
    radios
  }

  case class MyElement(name: String)
  // val selected: Var[MyElement] = Var(elements(1))

  def activeLabelChoice(): Selector.Options[MyElement] = {
    // Define a toy case class containing at least a name attribute

    // Define the option sequence
    val first = MyElement("Title")
    val second = MyElement("Authors")
    val third = MyElement("Abstract")
    val elements = Seq(first, second, third)
    lazy val optionDropDown: Selector.Options[MyElement] =
      elements.options(
        1,
        btn_success,
        (m: MyElement) => m.name
        // () => selected() = optionDropDown.content.now.get,
        // decorations = Map(first -> glyph_fire, second -> glyph_settings, third -> glyph_flash)
      )
    optionDropDown

  }

  @JSExport
  def display(): Unit = {
    println("hello from scalajs")

    bs.withBootstrapNative {

      val pageName = navItem(span("Demo").render)

      val buttonCluster = navItem(
        buttonGroup()(
          bs.button("Delete", btn_small, ()=> {println("del")}),
          bs.button("Merge", btn_small, ()=> {println("merge")})
        ).render
      )

      val nav = PageLayout.initNavbar(List(
        pageName,
        navItem(span(selectionConstraintChoice().render).render),
        navItem(span(activeLabelChoice().selector).render),
        navItem(span(buttonCluster.render).render)
      ))

      val basdf = buttonGroup()(
        bs.button("Delete", btn_small, ()=> {println("del")}),
        bs.button("Merge", btn_small, ()=> {println("merge")})
      )

      // val nav = PageLayout.sampleNavbar()
      val asdf = div(
        span("Select snap-to:"),
        // span(selectionConstraintChoice()),
        span(activeLabelChoice().selector),
        span(basdf)
      )


      val bodyContent = div(p("Body Content"))
      val sidebarContent =
        ul(`class`:="sidebar-nav")(
          // li(^.`class`:="sidebar-brand")(
          //   a(^.`href`:="#")("Start Bootstrap")
          // ),
          // li(asdf),
          // li(a(^.`href`:="#")("Contact"))
        )

      PageLayout.pageSetup(nav, bodyContent, sidebarContent).render
    }

  }
}
