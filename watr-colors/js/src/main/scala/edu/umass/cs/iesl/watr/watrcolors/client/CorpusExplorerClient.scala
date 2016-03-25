package edu.umass.cs.iesl.watr
package watrcolors
package client


// import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.mousetrap._


@JSExport
class CorpusExplorerClient(entryDescriptor: Option[String]) extends ClientView {

  val server = ServerWire("explorer")[CorpusExplorerApi]
  import domtags._
  import scala.collection.mutable

  var entriesPrevs = mutable.ArrayBuffer[String]()
  var entriesNexts = mutable.ArrayBuffer[String]()
  var entriesCurr: Option[String] = entryDescriptor


  def createView(): Unit = {
    entriesCurr.map{ curr =>
      server.createView().call()
    }

    def init()  = {
      <.div(
        <.ul("corpus-entries".id)
      )
    }
    server.createView().call().foreach{ update =>
      applyHtmlUpdates(update)
    }
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

  def openFocus(): Boolean = {
    server.getFileInFocus().call().foreach { currfile =>
      WatrColorClient.switchViews(new SvgOverview(currfile))
    }
    server.openFocus().call() foreach (applyHtmlUpdates(_))

    true
  }

  override val initKeys = Keybindings(List(
    "j" -> ((e: MousetrapEvent) => navNext),
    "k" -> ((e: MousetrapEvent) => navPrev),
    "x" -> ((e: MousetrapEvent) => openFocus)
  ))

}
