package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.collection.mutable
import scaladget.stylesheet.{all => sty}
import sty._

import org.scalajs.dom.raw.{
  HTMLElement
}
import scaladget.api.{BootstrapTags => bs}
import scalatags.JsDom.all._
import scalatags.JsDom.{
  TypedTag
}
import org.scalajs.dom
import bs._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.concurrent.Future

import TypeTagPicklers._

import autowire._
import upickle.{default => UPickle}
import UPickle._

import rx._
  // import rx.ops._
import scaladget.tools.JsRxTags._

trait SharedClientDefs extends LabelerRendering {
  type HtmlTag = TypedTag[HTMLElement]
  type RxHtmlTag = Rx.Dynamic[TypedTag[HTMLElement]]
  type RxHtmlTags = Rx.Dynamic[Seq[TypedTag[HTMLElement]]]

  def queryParams(): Map[String, String] = {
    val params = mutable.HashMap[String, String]()

    val href = dom.window.location.href
    if (href.contains("?")) {
      val qs = href.split("\\?", 2)(1)
      for {
        qparam <- qs.split('&')
      } {
        qparam.split('=') match {
          case Array(k) =>  params(k) = ""
          case Array(k, v) => params(k) = v
          case _ =>
        }
      }
    }
    params.toMap
  }

  def param(k: String):Option[String] = {
    val params = queryParams()
    if (params.contains(k)){
      Option(params(k))
    } else None
  }

}

@JSExportTopLevel("BrowseCorpus")
object BrowseCorpus extends SharedClientDefs {


  val buttonStyle: ModifierSeq = Seq(
    sty.marginAll(right = 5, top = 5)
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

  val leftGlyph = glyph_chevron_left
  val rightGlyph = glyph_chevron_right


  def docPagination(implicit c : Ctx.Owner): RxHtmlTag = Rx {
    span(
      span(s"Displaying ${currDocStart()}-${currDocStart()+20} of ${docCount()} "),
      span(btnGroup,
        bs.glyphSpan(leftGlyph, () => prevPage()),
        bs.glyphSpan(rightGlyph, () => nextPage())
      )
    )
  }


  def documentLister(implicit c: Ctx.Owner): RxHtmlTags = Rx {

    for {entry <- docList()} yield {
      val docId = entry.urlStr
      val t = s"/label?doc=${docId}&lt=zzz"
      li(
        span(
          a(
            entry.urlStr,
            cursor := "pointer",
            href := t
          ) (
            entry.id.toString()
          )
        )
      )
    }
  }



  @JSExport
  def display(): Unit = {

    implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

    server.documentCount()
      .foreach { c => docCount() = c }


    bs.withBootstrapNative {

      val pageName = navItem(span("Browse").render)
      // val nav = PageLayout.initNavbar(List(pageName))
      val nav = PageLayout.initNavbar(List())

      val main =
        div(
          sty.marginLeft(5),
          h2("Documents"),
          div(
            sty.marginLeft(5),
            docPagination
          ),
          div(
            sty.marginLeft(10),
            ul(documentLister)
          )
        )

      PageLayout.pageSetup(nav, main).render
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
