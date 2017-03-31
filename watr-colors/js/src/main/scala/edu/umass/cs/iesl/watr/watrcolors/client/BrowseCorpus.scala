package edu.umass.cs.iesl.watr
package watrcolors
package client

import scaladget.stylesheet.{all => sheet}
import sheet._
// import scalatags.JsDom.{ styles  }

import scaladget.api.{BootstrapTags => bs}
import scalatags.JsDom.all._
import bs._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.concurrent.Future

import TypeTagPicklers._

import autowire._
import upickle.{default => UPickle}
import UPickle._

import rx._
import scaladget.tools.JsRxTags._

@JSExportTopLevel("BrowseCorpus")
object BrowseCorpus extends LabelerRendering {

  val buttonStyle: ModifierSeq = Seq(
    sheet.marginAll(right = 5, top = 5)
  )

  // List all Labelers
  // List all Documents
  val docList: Var[Seq[DocumentEntry]] = Var(List())
  var currDocStart = Var(0)
  var docCount = Var(0)

  def prevPage() = {
    currDocStart() = currDocStart.now - 20
    currDocStart() = math.max(0, currDocStart.now)

    server.listDocuments(20, currDocStart.now).foreach { docs =>
      docList() = docs
    }
  }
  def nextPage() = {
    server.listDocuments(20, currDocStart.now).foreach { docs =>
      docList() = docs
    }
    currDocStart() = currDocStart.now + 20
  }

  // val navButtonStyle = buttonStyle +++ btn_small
  // bs.button(glyph_chevron_left, navButtonStyle, () => prevPage()),
  // bs.button(glyph_chevron_right, navButtonStyle, () => nextPage())
  // glyph_chevron_left
  val leftGlyph = glyph_chevron_left +++ glyph_chevron_left +++ glyph_chevron_left
  val rightGlyph = glyph_chevron_right +++ glyph_chevron_right +++ glyph_chevron_right

  val docNav = Rx {
    span(
      span(s"Displaying ${currDocStart()}-${currDocStart()+20} of ${docCount()} "),
      span(btnGroup,
        bs.glyphSpan(leftGlyph, () => prevPage()),
        bs.glyphSpan(rightGlyph, () => nextPage())
      )
    )
  }

  val docListDisplay = Rx {
    for {entry <- docList()} yield {
      li(
        span(
          a(entry.urlStr, cursor := "pointer",
            href := entry.urlStr, target:="_blank"
          ) (
            entry.id.toString()
          )
        )
      )
    }
  }


  @JSExport
  def display(): Unit = {
    server.documentCount()
      .foreach { c => docCount() = c }

    bs.withBootstrapNative {
      val pageName = navItem(span("/Browse").render)
      // val nav = PageLayout.initNavbar(List(pageName))
      val nav = PageLayout.sampleNavbar()

      val bod = div("Documents")(
        div(
          docNav,
          ul(docListDisplay)
        )
      )

      PageLayout.pageSetup(nav, bod).render
    }
  }

  object server extends BrowseCorpusApi {
    val Client = new WebsideClient("browse")
    val api = Client[BrowseCorpusApi]

    def listLabelers(n: Int, skip: Int): Future[Seq[LabelerEntry]] = {
      api.listLabelers(n, skip).call()
    }

    def listDocuments(n: Int, skip: Int): Future[Seq[DocumentEntry]] = {
      println(s"BrowseCorpus: listDocuments()")
      api.listDocuments(n, skip).call()
    }

    def documentCount(): Future[Int] = {
      api.documentCount().call()
    }

  }


}
