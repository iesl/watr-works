package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery

import native.mousetrap._



case class Keybindings(
  bindings: List[(String, (MousetrapEvent) => Boolean)]
)


trait ClientView {

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

@JSExport
object WatrColorClient {

  var currentView: ClientView = null

  def switchViews(v: ClientView): Unit = {
    currentView = v
    currentView.createView()
    currentView.setKeybindings(currentView.initKeys)
  }

  // TODO Can't figure out why this main() is getting called twice, so putting this guard here..
  var started = false

  @JSExport
  def main(): Unit = {
    if (!started) {
      started = true

      println("WatrColors Client started")
      switchViews(new CorpusExplorerClient())

    }
  }

}
