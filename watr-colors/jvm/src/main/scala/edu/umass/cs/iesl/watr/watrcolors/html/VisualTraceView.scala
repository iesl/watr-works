package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.stylesheet.{CascadingStyleSheet, Sheet}
import scalatags.Text.all._



class VisualTraceView() {

  val SvgStyles = Sheet[SvgStyles]

  trait SvgStyles extends CascadingStyleSheet {

    def overlayContainer = cls(
      padding:="0",
      border:="0",
      margin:="0",
      position.relative
    )


    def svgImage = cls(
      position.absolute,
      left:="0",
      top:="0",
      padding:="0",
      margin:="0",
      border := "0",
      backgroundColor := "ivory"
    )

    def fabricCanvas = cls(
      position.absolute,
      padding:="0",
      margin:="0",
      border := "0",
      left:="0",
      top:="0"
    )

    def infoPane = cls(
      position.absolute,
      padding:="0",
      margin:="0",
      border := "0",
      left:="0",
      top:="0"
    )
  }

  def containerFluid = `class` := "container-fluid"
  def row = `class` := "row"
  def col(n: Int) = `class` := s"col-md-$n"



  // split pane, svg on left, bbox hover info on right..
  def init()  = {

    div(containerFluid)(
      <.style(^.`type`:="text/css", SvgStyles.styleSheetText),
      <.div(row)(
        <.span("Visual Tracing Page")
      ),
      <.div(row)(
        <.div(col(6))(
          <.div(^.id:="overlay-container", SvgStyles.overlayContainer)(
            <.div(^.id:="svg-container", SvgStyles.svgImage),
            <.canvas(^.id:="fabric-canvas", SvgStyles.fabricCanvas),
            <.script(^.`type`:="text/javascript")(
              raw(s"""|var canvasEle = document.getElementById("fabric-canvas");
                      |var canvas = new fabric.Canvas('fabric-canvas');
                      |canvasEle.fabric = canvas;
                      |canvas.uniScaleTransform = true;
                      |fabric.Object.prototype.transparentCorners = false;
                      |""".stripMargin))
          )
        ),

        <.div(col(6))(
          <.div(SvgStyles.infoPane)(
            <.span("Info Area"),
            <.span(^.id:="rxinfo"),
            <.span(^.id:="selection-info")
          )
        )
      )
    )
  }
}
