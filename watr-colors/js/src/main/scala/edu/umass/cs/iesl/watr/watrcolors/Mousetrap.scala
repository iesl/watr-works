package edu.umass.cs.iesl.watr
package watrcolors

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import js.annotation._


@js.native
object Mousetrap extends js.Object {
  def bind(keys : js.Array[String], fn : js.Function1[MousetrapEvent, Boolean]) : js.Any = scala.scalajs.js.native
  def bind(key : String, fn : js.Function1[MousetrapEvent, Boolean]) : js.Any = scala.scalajs.js.native
}

@js.native
trait MousetrapEvent extends js.Object
