package edu.umass.cs.iesl.watr
package native
package mousetrap

import scala.scalajs.js
import js.annotation.JSGlobal

@js.native @JSGlobal
object Mousetrap extends js.Object {
  def bind(keys: js.Array[String], fn: js.Function1[MousetrapEvent, Boolean]) : js.Any = js.native
  def bind(key: String, fn: js.Function1[MousetrapEvent, Boolean]) : js.Any = js.native
  def bind(key: String, fn: js.Function2[MousetrapEvent, String, Boolean]) : js.Any = js.native
  def bind(key: String, fn: js.Function1[MousetrapEvent, Boolean], mod: String = "keypress") : js.Any = js.native
  def unbind(key : String) : js.Any = js.native
  def reset() : js.Any = js.native
}


@js.native
trait MousetrapEvent extends js.Object {
  def `type`: String = js.native
}



