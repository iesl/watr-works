package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages


import parts._

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport

import scaladget.stylesheet.{all => sty}

import scalatags.JsDom.all._
import sty.{ctx => _, _}

import watrmarks.{StandardLabels => LB}
import rx._
// import org.scalajs.dom.raw._
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import utils.Colors
import scaladget.api.{
  SelectableButtons
}
import scaladget.tools.JsRxTags.{ctx => _, _}
import textboxing.{TextBoxing => TB}, TB._
import labeling._
import TypeTags._

@JSExportTopLevel("DevClient")
object DevClient extends BaseClientDefs {
  import BootstrapBits._



  @JSExport
  def display(): Unit = {
    implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

    println("hello from scalajs")

    withBootstrapNative {

      val pageName = navItem(span("Demo").render)
      val setupLabelChooser: RxModifier = Rx {
        val selectActiveLabel: SelectableButtons = radios()(
          (List(
            (LB.Title, Colors.Lavender),
            (LB.Authors, Colors.Orange),
            (LB.Abstract, Colors.Firebrick),
            (LB.Affiliation, Colors.SkyBlue),
            (LB.References, Colors.Green3)
          ).zipWithIndex.map{ case ((lbl, clr), i) =>
            selectableButton(
              lbl.fqn,
              (i==0),
              modifierSeq = List(backgroundColor:=clr.cssHash()),
              onclick = () => {println(s"chose ${lbl}")}
            )
          }):_*
        )
        selectActiveLabel.render
      }

      val state = UIState(
        ByLine,
        None,
        Seq(),
        DocumentLabelerIdentifier(DocumentID("doc#XYZ"), "my-lt", Pagination(0, PageNum(3), None))
      )
      val selectorControls = SharedLayout.zoneSelectorControls(
        new ClientStateRx(state, (_) => Future{()}),
        setupLabelChooser
      )

      val nav = SharedLayout.initNavbar(List(
        pageName
      ))


      def sampleText = vjoin(center1)(
        tbox("Lorem ipsum dolor sit amet"),
        tbox("Anyconsectetur adipisicing elit, sed do eiusmod tempor"),
        tbox("aliqua. Ut enim ad minim veniam, "),
        tbox("incididunt ut labore et dolore magna "),
        tbox("aliqua. Ut enim ad minim veniam, ")
      )

      val t0 = hcat(repeat(sampleText).take(5))

      val t1 = vjoins(TB.left)(
        repeat(t0).take(20)
          .zipWithIndex.map({case (b, i) =>
            indent(i)(b)
          })
      )

      val bodyContent =
        div("container-fluid".clazz)(
          div("row".clazz, pageStyles.controlClusterStyle)(
            div("col-lg-12".clazz)(
              selectorControls
            )
          ),
          div("row".clazz, pageStyles.labelerBodyStyle)(
            div("col-lg-12".clazz)(
              pre(t1.toString)
            )
          )
        )

      val sidebarContent =
        ul(`class`:="sidebar-nav")()

      SharedLayout.pageSetup(nav, bodyContent, sidebarContent).render
    }

  }
}
