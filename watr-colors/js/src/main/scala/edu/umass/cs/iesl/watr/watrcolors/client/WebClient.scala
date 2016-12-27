package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.js.annotation.JSExport

import native.mousetrap._

/**

  */

case class Keybindings(
  bindings: List[(String, (MousetrapEvent) => Boolean)]
)

trait ClientView extends FabricCanvasOperations {

  def setKeybindings(kb: Keybindings) = {
    Mousetrap.reset()
    kb.bindings.foreach {
      case (str, fn) =>
        Mousetrap.bind(str, fn, "keypress")
    }
  }

  def applyHtmlUpdates(updates: List[HtmlUpdate]): Unit = {
    updates.foreach { u =>
      u match {
        case HtmlAppend(css, content) => jQuery(css).append(content)
        case HtmlPrepend(css, content) => jQuery(css).prepend(content)
        case HtmlReplace(css, content) => jQuery(css).replaceWith(content)
        case HtmlReplaceInner(css, content) => jQuery(css).html(content)
        case HtmlRemove(css) => jQuery(css).remove()
      }
    }
  }

  def initKeys: Keybindings

  def createView(): Unit

}
