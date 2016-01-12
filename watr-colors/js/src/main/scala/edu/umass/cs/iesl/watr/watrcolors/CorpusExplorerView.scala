package edu.umass.cs.iesl
package watr
package watrcolors

import autowire._
import org.scalajs.jquery.jQuery
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scala.scalajs.js.annotation.JSExport
import upickle.default._

sealed trait HtmlUpdate

case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
case class HtmlAppend(css: String, content: String) extends HtmlUpdate
case class HtmlReplace(css: String, content: String) extends HtmlUpdate
case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
case class HtmlRemove(css: String) extends HtmlUpdate



@JSExport
class CorpusExplorerView() extends ClientView {

  // val server = Client[CorpusExplorerApi]
  val server = ServerWire("explorer")[CorpusExplorerApi]

  def createView(): Unit = {
    server.createView().call().foreach(applyHtmlUpdates(_))
  }

  def navNext(e: MousetrapEvent): Boolean = {
    server.navNext().call().foreach(applyHtmlUpdates(_))
    true
  }

  def navNext(): Boolean = {
    server.navNext().call().foreach(applyHtmlUpdates(_))
    true
  }

  def navPrev(): Boolean = {
    server.navPrev().call() foreach (applyHtmlUpdates(_))
    true
  }

  def openCurrent(): Boolean = {
    // close this view
    //
    server.openCurrent().call() foreach (applyHtmlUpdates(_))
    WatrColorClient.switchViews(new SvgOverview())
    true
  }

  val initKeys = Keybindings(List(
    "j" -> ((e: MousetrapEvent) => navNext),
    "k" -> ((e: MousetrapEvent) => navPrev),
    "enter" -> ((e: MousetrapEvent) => openCurrent)
  ))

  def init(): Unit = {
    println("Corpus Explorer started")
  }

}

@JSExport
class SvgOverview() extends ClientView {
  // val server = Client[SvgOverviewApi]
  val server = ServerWire("svg")[SvgOverviewApi]

  val initKeys = Keybindings(List())

  def createView(): Unit = {
    server.createView().call().foreach(applyHtmlUpdates(_))
  }
}
