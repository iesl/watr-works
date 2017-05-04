package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages


import parts._
import wiring._

import scala.async.Async
import scala.concurrent.Future

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom
import org.scalajs.dom.ext._


import labeling._
import watrmarks._
import native.mousetrap._
import native.fabric

// import watrmarks.{StandardLabels => LB}

import scaladget.stylesheet.{all => sty}
import sty.{ctx => _, _}
import scalatags.JsDom.all._

import rx._
import scaladget.api._
import scaladget.tools.JsRxTags.{ctx => _, _}

import TypeTags._

import scala.collection.mutable
import BootstrapBits._

@JSExportTopLevel("Registration")
object Registration extends  BaseClientDefs {



  @JSExport
  def display(): Unit = {
    implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

    val navContent =  SharedLayout.initNavbar(List())

    val loginPanel = SharedLayout.loginPanel

    val bodyContent =
      div("container-fluid".clazz)(
        div("row".clazz, pageStyles.controlClusterStyle)(
          div("col-lg-12".clazz)(
            loginPanel
          )
        )
      )

    val sidebarContent = ul(`class`:="sidebar-nav")

    withBootstrapNative {
      SharedLayout.pageSetup(navContent, bodyContent, sidebarContent).render
    }

  }
}
