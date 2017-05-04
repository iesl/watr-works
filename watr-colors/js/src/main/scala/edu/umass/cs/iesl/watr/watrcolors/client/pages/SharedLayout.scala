package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages

import parts._

import org.scalajs.dom.raw._

import scaladget.stylesheet.{
  all => sty
}

// import sty.{ctx => _, _}

import scaladget.api.{
  SelectableButtons
}
import scalatags.JsDom.all._
import scalatags.JsDom.{
  TypedTag
}

import labeling._
import rx._

import scaladget.tools.JsRxTags.{ctx => _, _}
import scalatags.JsDom.tags

object SharedLayout extends BaseClientDefs {
  import BootstrapBits._

  import pageStyles._

  // User/passwd input forms

  val usernameInput = input("")(
    placeholder := "username",
    `type` := "username",
    width := "300px",
    sty.marginBottom(15),
    name := "username",
    autofocus := true
  ).render

  def setPasswordForm(): HtmlTag =
    tags.form(
      action := "/login",
      method := "post",
      usernameInput
    )


  def loginPanel(implicit co: Ctx.Owner): HtmlTag =  {

    setPasswordForm()

  }

  def initNavbar(
    navItems: Seq[NavEntry[_ <: HTMLElement]] = Seq()
  )(implicit co: Ctx.Owner): HtmlTag = {
    val logo = navItem(
      span(pageStyles.logo, "WatrColors").render
    )

    val login = navItem(loginPanel(co).render)
    val navs = logo :: login :: navItems.toList

      navBar(
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
