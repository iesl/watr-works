package edu.umass.cs.iesl.watr
package watrcolors
package client


import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.mousetrap._

import scala.concurrent.Future
import scala.async.Async.{async, await}

@JSExport
class WatrShellClient extends ClientView {
  val server = ServerWire("shell")[WatrShellApi]


  override val initKeys = Keybindings(List(
  ))

  def createView(): Unit = {
    server.startShell().call().foreach { _ =>
      println("server shell started")
    }
  }
}
