package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages


import parts._
import scala.scalajs.js.annotation._
import scalatags.JsDom.all._

import rx._
import BootstrapBits._

@JSExportTopLevel("Registration")
object Registration extends  BaseClientDefs {



  @JSExport
  def display(): Unit = {
    implicit val ctx: Ctx.Owner = Ctx.Owner.safe()


    val loginPanel = SharedLayout.loginPanel

    val bodyContent =
      div("container-fluid".clazz)(
        div("row".clazz, pageStyles.controlClusterStyle)(
          div("col-lg-12".clazz)(
            loginPanel
          )
        )
      )


    withBootstrapNative {
      SharedLayout.pageSetup(None, bodyContent).render
    }

  }
}
