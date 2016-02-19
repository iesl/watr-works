package edu.umass.cs.iesl.watr
package watrcolors

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

@js.native
object Mousetrap extends js.Object {
  def bind(keys: js.Array[String], fn: js.Function1[MousetrapEvent, Boolean]) : js.Any = js.native
  def bind(key: String, fn: js.Function1[MousetrapEvent, Boolean]) : js.Any = js.native
  def bind(key: String, fn: js.Function1[MousetrapEvent, Boolean], mod: String = "keypress") : js.Any = js.native
  def unbind(key : String) : js.Any = js.native
  def reset() : js.Any = js.native
}


@js.native
trait MousetrapEvent extends js.Object


// import org.scalajs.jquery.jQuery
import org.scalajs.jquery.JQuery


@js.native
trait SplitPane extends JQuery {
  def splitPane(): Unit = js.native
  def splitPane(op: String, v: Int): Unit = js.native
}

object SplitPane {
  implicit def jq2splitpane(jq: JQuery): SplitPane =
    jq.asInstanceOf[SplitPane]
}


