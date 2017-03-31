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



object PageLayout {

  import pageStyles._


  def initNavbar(navItems: Seq[NavItem[HTMLSpanElement]] = Seq()) = {
    val logo = navItem(
      span("WatrColors").render
    )
    val navs = logo :: navItems.toList

    // +++ navbar_inverse
    val navStyle = (
      navbar_staticTop ++ navbarStyle
    )

    bs.navBar(
      navStyle,
      navs:_*
    )
  }

  def pageSetup(
    nav: TypedTag[HTMLElement],
    bod: TypedTag[HTMLElement]
  ) = {
    div(
      nav,
      div(bod),
      div(footerStyle)(
        "<<WatrWorks>>"
      )
    )
  }

  case class MenuAction(name: String, action: () ⇒ Unit)

  def sampleNavbar(): TypedTag[HTMLElement] = {
    import scaladget.api.Selector.Options

    lazy val fileChevronStyle: ModifierSeq = Seq(
      lineHeight := "10px",
      top := 10,
      left := -30,
      sty.paddingRight(20)
    )

    lazy val mainNav0: ModifierSeq = Seq(
      sty.paddingLeft(0),
      borderColor := "yellow",
      zIndex := 10
    )

    lazy val mainNav370: ModifierSeq = Seq(
      sty.paddingLeft(370),
      borderColor := "yellow",
      zIndex := 10
    )

    lazy val importModel = MenuAction("Import your model", () ⇒ {
      // modelWizardPanel.dialog.show
    })

    lazy val elements = Seq(importModel)

    lazy val menuActions: Options[MenuAction] = elements.options(
      key = btn_danger,
      naming = (m: MenuAction) ⇒ m.name,
      onclose = () ⇒ menuActions.content.now.foreach {
        _.action()
      },
      fixedTitle = Some("New project")
    )
    val itemStyle = lineHeight := "35px"

    val execItem           = navItem(div(glyph_flash, itemStyle).tooltip("Executions"), () ⇒ {})
    val authenticationItem = navItem(div(glyph_lock, itemStyle).tooltip("Authentications"), () ⇒ {})
    val pluginItem         = navItem(div(glyph_plug, itemStyle).tooltip("Plugins"), () ⇒ {})
    val envItem            = navItem(div(glyph_exclamation, itemStyle).render, () ⇒ {})
    val docItem            = navItem(div(glyph_file, itemStyle).tooltip("Documentation"), () ⇒ {})
    // navItem(div(glyph_chevron_left, fileChevronStyle).render, todo = () ⇒ {}),

    val navBar = div(
      // Rx {
      bs.navBar(
        absoluteFullWidth +++
          sty.nav +++
          navbar_pills +++
          navbar_inverse +++
          (fontSize := 20) +++
          navbar_staticTop +++ { mainNav0 },
        navItem(menuActions.selector),
        execItem,
        docItem
      )
    )
    navBar
  }
}
