package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages

import parts._

import org.scalajs.dom.raw._

import scaladget.stylesheet.{
  all => sty
}


import scaladget.api.{
  SelectableButtons
}
import scalatags.JsDom.all._
import scalatags.JsDom.tags
import scalatags.JsDom.{
  TypedTag
}

import labeling._
import rx._

import scaladget.tools.JsRxTags.{ctx => _, _}

object SharedLayout extends BasicClientDefs {
  import BootstrapBits._

  // import pageStyles._

  // User/passwd input forms

  val usernameInput = input("")(
    placeholder := "user@email.edu",
    `type` := "username",
    width := "300px",
    sty.marginBottom(15),
    name := "username",
    autofocus := true
  ).render

  def setPasswordForm(): HtmlTag =
    tags.form(
      action := "/user/login",
      method := "post",
      usernameInput
    )


  def loginPanel(implicit co: Ctx.Owner): HtmlTag =  {
    setPasswordForm()
  }


  def initNavbarLeft(
    navItems: Seq[NavEntry[_ <: HTMLElement]] = Seq()
  )(implicit co: Ctx.Owner): HtmlTag = {
    val logo = navInfo(
      div("navbar-header".clazz,
        a("navbar-brand".clazz,
          href:="/",
          span(pageStyles.logo, "WatrColors").render
        ).render
      ).render
    )

    val navs = logo :: navItems.toList

    navBar(
      pageStyles.navbarStyle,
      navs:_*
    )
  }

  def initNavbarRight(
    navItems: Seq[NavEntry[_ <: HTMLElement]] = Seq(),
    navItemsRight: Seq[NavEntry[_ <: HTMLElement]] = Seq()
  )(implicit co: Ctx.Owner): HtmlTag = {
    val logo = navInfo(
      a("navbar-brand".clazz,
        href:="/",
        span(pageStyles.logo, "WatrColors").render
      ).render
    )

    val navsL = logo :: navItems.toList
    val navsR = navItemsRight.toList

    navBarLeftRight(
      pageStyles.navbarStyle,
      navsL, navsR
    )
  }

  def pageSetup(
    userName: Option[String],
    bodyContent: TypedTag[HTMLElement],
    navLeft: Option[TypedTag[HTMLElement]] = None,
    navRight: Option[TypedTag[HTMLElement]] = None,
    sidebarContent: TypedTag[HTMLElement] = ul(`class`:="sidebar-nav")
  )(implicit co: Ctx.Owner) = {
    val userNav = userName.map(n =>
      stringNavItem(n, () => {}, activeDefault=false)
    ).toList

    val nb = initNavbarRight(
      Seq(), userNav
    )

    div(
      nb,
      sidebarLayout(bodyContent, sidebarContent)
    )
  }

  def sidebarLayout(
    bodyContent: TypedTag[HTMLElement],
    sidebarContent: TypedTag[HTMLElement]
  )(implicit co: Ctx.Owner): TypedTag[HTMLElement] = {

    div("wrapper".id, "toggled".clazz)(
      div("sidebar-wrapper".id, pageStyles.sidebarStyle)(
        sidebarContent
      ),

      div("page-content-wrapper".id)(
        div("container-fluid".clazz)(
          div("row".clazz)(
            div("col-lg-12".clazz)(
              bodyContent
            )
          )
        )
      )
    )

  }


  def zoneSelectorControls(
    clientStateRx: ClientStateRx,
    labelSelector: RxModifier
  )(implicit ctx: Ctx.Owner): RxHtmlTag = Rx {

    val delClrMrgButtons = buttonGroup()(
      button("Merge", sty.btn_small,  () => {clientStateRx.doMergeZones() = true}),
      button("Delete", sty.btn_danger, () => {clientStateRx.doDeleteZone() = true})
    )

    val initSelectConstraint =
      List[(String, Constraint, Boolean)](
        ("Line", ByLine, true),
        ("Char", ByChar, false),
        ("Region", ByRegion, false)
      ).map{ case (s, c, b) =>
          selectableButton(s,
            defaultActive = b,
            modifierSeq = sty.btn_small,
            onclick = () => { clientStateRx.uiState_constraint() = c }
          )
      }

    val selectConstraint: SelectableButtons = radios()(
      initSelectConstraint:_*
    )


    div("selection-controls".id,
      selectConstraint.render,
      labelSelector,
      delClrMrgButtons.render
    )

  }


}
