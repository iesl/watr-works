package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages

import parts._
import wiring._

import scaladget.stylesheet.{all => sty}
import sty.{ctx => _, _}
import scalatags.JsDom.all._
import rx._
import scaladget.api._
import scaladget.tools.JsRxTags.{ctx => _, _}

import scalatags.JsDom.all._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.concurrent.Future
import watrmarks.{StandardLabels => LB}



@JSExportTopLevel("BrowseCorpus")
object BrowseCorpus extends BasicClientDefs {
  import BootstrapBits._


  val buttonStyle: ModifierSeq = Seq(
    sty.marginAll(right = 5, top = 5)
  )

  val pageLength = 40

  val docList: Var[Seq[DocumentEntry]] = Var(List())
  val currDocStart = Var(0)
  val docCount = Var(0)

  val allLabelsRx: Var[List[watrmarks.Label]] = Var(List(
    LB.Title        ,
    LB.Authors      ,
    LB.Abstract     ,
    LB.Affiliations ,
    LB.References
  ))

  val labelFiltersRx: Var[List[watrmarks.Label]] = Var(List())


  def constrain(min: Int, n: Int, max: Int): Int = {
    math.min(math.max(min, n), max)
  }

  def constrainStart(start: Int): Int = {
    constrain(0, start,
      constrain(
        0, docCount.now-pageLength, docCount.now)
    )
  }

  def prevPage()(implicit c : Ctx.Owner) = Rx {
    val newStart = constrainStart(currDocStart.now-pageLength)
    currDocStart() = newStart
  }

  def nextPage()(implicit c : Ctx.Owner)= Rx {
    val newStart = constrainStart(currDocStart.now+pageLength)
    currDocStart() = newStart
  }

  val leftGlyph = glyph_chevron_left
  val rightGlyph = glyph_chevron_right

  def docPagination()(implicit c : Ctx.Owner): RxHtmlTag = Rx {
    val currStart = currDocStart()
    val currDocCount = docCount()
    span(
      span(s"Displaying ${currStart}-${currStart+pageLength} of ${currDocCount} "),
      span(btnGroup,
        glyphSpan(leftGlyph, () => prevPage()),
        glyphSpan(rightGlyph, () => nextPage())
      )
    )
  }

  def documentLister(implicit c: Ctx.Owner): RxHtmlTags = Rx {

    for {entry <- docList()} yield {
      val docId = entry.urlStr
      val t = s"/label?doc=${docId}&lt=zzz"
      val zoneInfo = entry.zoneCounts.map{ case (label, count) =>
        li(pageStyles.inlineULI:_*)(b(label.key), ":", <.nbsp, i(count.toString))
      }

      li(
        span(
          a(
            entry.urlStr,
            cursor := "pointer",
            href := t
          ) (entry.id.toString()),
          ul(pageStyles.inlineUList:_*)(zoneInfo)
        )
      )
    }
  }



  def setupLabelFilters()(implicit co: Ctx.Owner): RxModifier = Rx {
    val allLabels = allLabelsRx()
    val labelFilters = labelFiltersRx()

    val selectActiveLabel: SelectableButtons = radiosMultiSelect("label-filter".clazz)(
      (allLabels.zipWithIndex.map{ case (lbl, i) =>
        val labelSelected = labelFilters.contains(lbl)
        selectableButton(
          lbl.key,
          labelSelected,
          modifierSeq = (sty.btn_small
            +++ (^.name  :="label-filter")
            +++ (^.id    :=s"${lbl.key}-filter")
            +++ (^.value :=s"${lbl.key}-filter")
          ),
          onclick = () => {
            if(labelSelected) {
              labelFiltersRx() = labelFilters.filterNot(_==lbl)
            } else {
              labelFiltersRx() = lbl :: labelFilters
            }
          }
        )
      }):_*
    )
    selectActiveLabel.render
  }


  @JSExport
  def display(userName: String): Unit = {

    implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

    val labelFilters = setupLabelFilters()

    val bodyContent =
      div("container-fluid".clazz)(
        div("row".clazz, pageStyles.controlClusterStyle)(
          div("col-lg-12".clazz)(
            docPagination,
            <.nbsp, <.nbsp, <.nbsp,
            <.span("Label Filters:"), <.nbsp,
            labelFilters
          )
        ),
        div("row".clazz)(
          div("col-lg-12".clazz)(
            ul(documentLister)
          )
        )
      )

    withBootstrapNative {
      SharedLayout.pageSetup(Option(userName), bodyContent).render
    }


    labelFiltersRx.trigger {
      server.documentCount(labelFiltersRx.now)
        .foreach { c =>
          docCount() = c
          server.listDocuments(pageLength, currDocStart.now, labelFiltersRx.now).foreach { docs =>
            println(s"listing docs ${currDocStart.now}, ${labelFiltersRx.now}")
            docList() = docs
            currDocStart() = 0
          }
        }
    }

    currDocStart.triggerLater {
      server.listDocuments(pageLength, currDocStart.now, labelFiltersRx.now).foreach { docs =>
        println(s"listing docs ${currDocStart.now}, ${labelFiltersRx.now}")
        docList() = docs
      }
    }

    server.documentCount(labelFiltersRx.now)
      .foreach { c => docCount() = c }

  }

  object server extends BrowseCorpusApi {
    import autowire._
    import UPicklers._
    import watrmarks.Label

    val Client = new WebsideClient("browse")
    val api = Client[BrowseCorpusApi]

    def listDocuments(n: Int, skip: Int, labelFilters: Seq[Label]): Future[Seq[DocumentEntry]] = {
      api.listDocuments(n, skip, labelFilters).call()
    }

    def documentCount(labelFilters: Seq[Label]): Future[Int] = {
      api.documentCount(labelFilters).call()
    }

  }


}
