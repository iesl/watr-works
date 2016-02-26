package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.stylesheet.{CascadingStyleSheet, StyleSheet, StyleSheetTags, Sheet, Selector}
import scalatags.Text.all._

object util {
  import org.scalajs.dom
  import scalajs.js
  import scalajs.js.annotation.JSExport
  import rx._
  import scalatags.JsDom.all._
  import dom.html

  // implicit def rxFrag[T <% Frag](r: Rx[T])(implicit ev: (T) => Fra): Frag = {

  implicit def rxFrag[T](r: Rx[T])(implicit ev: (T) => Frag): Frag = {
    def rSafe: dom.Node = span(r()).render
    var last = rSafe
    r.triggerLater {
      val newLast = rSafe
      js.Dynamic.global.last = last
      last.parentNode.replaceChild(newLast, last)
      last = newLast
    }

    last
  }
}



class SvgOverviewView(
  config: PdfCorpusConfig
) {

  val svgRepoPath = config.rootDirectory

  val SvgStyles = Sheet[SvgStyles]

  trait SvgStyles extends CascadingStyleSheet {

    def overlayContainer = cls(
      position.relative
    )

    def bboxInfo = cls(
      float.right
    )

    def svgImage = cls(
      position.absolute,
      left:="0",
      top:="0",
      border := "1ps solid black",
      backgroundColor := "ivory"
    )

    def fabricCanvas = cls(
      position.absolute,
      left:="0",
      top:="0"
    )
    def testCanvas = cls(
      width:="200",
      height:="300"
    )

  }

  def containerFluid = `class` := "container-fluid"
  def row = `class` := "row"
  def col(n: Int) = `class` := s"col-md-$n"



  // split pane, svg on left, bbox hover info on right..
  def init(filename: String)  = {

    div(containerFluid)(
      <.style(^.`type`:="text/css", SvgStyles.styleSheetText),
      <.div(row)(
        <.div(col(6))(
          <.div(^.id:="overlay-container", ^.style:="max-height:100%;overflow:auto;border:1px solid red;", SvgStyles.overlayContainer)(
            <.div(^.id:="svg-container", SvgStyles.svgImage),
            <.canvas(^.id:="fabric-canvas", SvgStyles.fabricCanvas),
            <.script(^.`type`:="text/javascript")(
              raw(s"""|var documentIconURI = "images/pdf_50x50.png";
                      |var fileRepositoryURI = "${svgRepoPath}";
                      |var fileName = "$filename";
                      |""".stripMargin)),
            <.script(^.`type`:="text/javascript", ^.src:="/assets/js/edit-document.js" )
          )
        ),

        <.div(col(6))(
          // overall dimension, page dimensions:

        )
      )
    )
  }
}


