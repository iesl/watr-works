package edu.umass.cs.iesl.watr
package watrcolors
package client

import org.scalajs.dom.raw._

import scaladget.stylesheet.{all => sty}
import scaladget.api.{BootstrapTags => bs}
import scalatags.JsDom.all._
import scalatags.JsDom.{
  TypedTag
}
import bs._
import sty._


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
    bod: TypedTag[HTMLElement]
  )(implicit co: Ctx.Owner) = {
    div(
      nav,
      bod(mainContentStyle),
      div(footerStyle)
    )
  }


  def sampleNavbar(): TypedTag[HTMLElement] = {
    import scaladget.api.Selector.Options


    lazy val importModel = MenuAction("Import your model", () => {
      // modelWizardPanel.dialog.show
    })

    lazy val elements = Seq(importModel)

    lazy val menuActions: Options[MenuAction] = elements.options(
      key = btn_danger,
      naming = (m: MenuAction) ⇒ m.name,
      onclose = () => menuActions.content.now.foreach {
        _.action()
      },
      fixedTitle = Some("New project")
    )
    val itemStyle = lineHeight := "35px"

    val execItem           = navItem(div(glyph_flash, itemStyle).tooltip("Executions"),     () => {})
    val authenticationItem = navItem(div(glyph_lock, itemStyle).tooltip("Authentications"), () => {})
    val pluginItem         = navItem(div(glyph_plug, itemStyle).tooltip("Plugins"),         () => {})
    val envItem            = navItem(div(glyph_exclamation, itemStyle).render,              () => {})
    val docItem            = navItem(div(glyph_file, itemStyle).tooltip("Documentation"),   () => {})
    // navItem(div(glyph_chevron_left, fileChevronStyle).render, todo = () ⇒ {}),

    val navBar = div(
      // Rx {
      bs.navBar(
        absoluteFullWidth +++
          sty.nav +++
          navbar_pills +++
          navbar_inverse +++
          (fontSize := 20) +++
          navbar_staticTop,
        navItem(menuActions.selector),
        execItem,
        docItem
      )
    )
    navBar
  }
}
