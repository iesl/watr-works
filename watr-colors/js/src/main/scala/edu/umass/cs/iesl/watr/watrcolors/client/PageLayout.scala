package edu.umass.cs.iesl.watr
package watrcolors
package client

import org.scalajs.dom.raw._

import scaladget.stylesheet.{all => sty}
import scaladget.api.{BootstrapTags => bs}
import scaladget.api.{SelectableButton, SelectableButtons}
import scalatags.JsDom.all._
import scalatags.JsDom.{
  TypedTag
}
import bs._
// import sty._


import rx._

// import scaladget.tools.JsRxTags._
case class MenuAction(
  name: String,
  action: () => Unit
)


object PageLayout extends SharedClientDefs {

  import pageStyles._

  def initNavbar(
    navItems: Seq[NavItem[HTMLSpanElement]] = Seq()
  )(implicit rxOwnr: Ctx.Owner): HtmlTag = {
    val logo = navItem(
      span("WatrColors").render
    )
    val navs = logo :: navItems.toList

    bs.navBar(
      navbarStyle,
      navs:_*
    )
  }

  def pageSetup(
    nav: TypedTag[HTMLElement],
    bodyContent: TypedTag[HTMLElement],
    sidebarContent: TypedTag[HTMLElement]
  )(implicit co: Ctx.Owner) = {
    div(
      nav,
      sidebarLayout(bodyContent, sidebarContent)
    )
  }

  def sidebarLayout(
    bodyContent: TypedTag[HTMLElement],
    sidebarContent: TypedTag[HTMLElement]
  )(implicit co: Ctx.Owner): TypedTag[HTMLElement] = {

    div(^.`id`:="wrapper", `class`:="toggled")(
      div(`id`:="sidebar-wrapper", pageStyles.sidebarStyle)(
        sidebarContent
      ),

      div(`id`:="page-content-wrapper")(
        div(^.`class`:="container-fluid")(
          div(^.`class`:="row")(
            div(^.`class`:="col-lg-12")(
              bodyContent
            )
          )
        )
      )
    )

  }



  def selectionConstraintChoice(): SelectableButtons = {
    val radios: SelectableButtons = bs.radios()(
      bs.selectableButton("Line", true, onclick = () =>{}),
      bs.selectableButton("Char", onclick = () =>{}),
      bs.selectableButton("None", onclick = () => {})
    )
    radios
  }
}


