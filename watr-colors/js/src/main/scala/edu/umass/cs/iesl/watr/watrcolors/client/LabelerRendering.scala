package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.js

import textreflow._
import textreflow.data._
import geometry._
import geometry.syntax._

import native.fabric
import native.fabric._

import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.std.scalaFuture._
import scala.collection.mutable

import scala.concurrent.{ Future, Promise }

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import org.scalajs.dom
// import dom.html
import scalatags.JsDom.all._

import labeling._
import LabelWidgetF._
import watrmarks.{StandardLabels => LB}
import textboxing.{TextBoxing => TB}
import utils.Color


trait LabelerRendering extends MouseGestures {
  import TextReflowF._

  override lazy val fabricCanvas =  {
    initFabric("canvas")
    getFabric("canvas")
  }


  def fabricObjectLTBounds(fobj: FabricObject): LTBounds = {
    val t = fobj.top
    val l = fobj.left
    val w = fobj.width
    val h = fobj.height

    LTBounds(l.doubleValue(), t.doubleValue(), w.doubleValue(), h.doubleValue())
  }


  def makeImageForPageRegion(pageRegion: TargetRegion, absPos: LTBounds): Future[FabricObject] = {

    val promise = Promise[FabricObject]()

    val callback: js.Function1[Image, Unit] =
      (img:Image) => {
        img.top = absPos.top
        img.left = absPos.left
        img.width = absPos.width
        img.height = absPos.height
        img.borderColor = ""
        img.backgroundColor = ""
        img.opacity = 1.0f
        img.stroke      = null
        img.strokeWidth = 0
        img.fill        = "rgb(0, 0, 0)"
        noControls(img)
        println(img)

        promise.success(img)
        ()
      }

    val pageRegionId = pageRegion.id

    Image.fromURL(s"/img/region/${pageRegionId}", callback)
    promise.future

  }


  def createButtonWidget(text: String, bbox: LTBounds): FabricObject = {
    val tw = createTextWidget(text, bbox)
    val bg = createShape(bbox, "", "blue", 0.1f)
    val g = fabric.Group(Seq(tw, bg))
    noControls(g)
    g
  }

  def createTextWidget(text: String, bbox: LTBounds): FabricObject = {
    val ftext = fabric.Text(text)
    ftext.setFontSize(14)
    ftext.top     = bbox.top
    ftext.left    = bbox.left
    val scaleX = bbox.width  / ftext.width.doubleValue
    val scaleY = bbox.height / ftext.height.doubleValue
    ftext.setScaleX(scaleX)
    ftext.setScaleY(scaleY)
    noControls(ftext)

    ftext
  }
  def createTextboxWidget(textbox: TB.Box, bbox: LTBounds): FabricObject = {
    val text = textbox.toString
    createTextWidget(text, bbox)
  }

  def createTextReflowWidget(textReflow: TextReflow, bbox: LTBounds): FabricObject = {
    val text = textReflow.toText()
    val ftext = fabric.Text(text)
    ftext.setFontSize(16)
    ftext.top     = bbox.top
    ftext.left    = bbox.left
    // val scaleX = ftext.width.doubleValue/bbox.width
    // val scaleY = ftext.height.doubleValue/bbox.height
    val scaleX = bbox.width  / ftext.width.doubleValue
    val scaleY = bbox.height / ftext.height.doubleValue
    ftext.setScaleX(scaleX)
    ftext.setScaleY(scaleY)

    ftext.opacity = 1.0f
    noControls(ftext)

    ftext
  }


  def createLabelerControls(options: LabelOptions): Tag  = {

    val labelOptions = select(
      options.labels.map(l => option(l.fqn))
    ).render

    labelOptions.onselect = (event: dom.Event) => {

    }

    val selectionGranularity = select(
      option("line"),
      option("char")
    ).render



    div(
      labelOptions,
      selectionGranularity
    )
  }

  def renderLabelWidget(positions: Seq[WidgetPositioning]): (LTBounds, Future[List[FabricObject]]) = {

    val objStack = mutable.ArrayBuffer[Future[FabricObject]]()

    def visit(p: WidgetPositioning): Unit = {
      val WidgetPositioning(fa, wbbox, transVec, scaling, id)  = p

      fa match {
        case RegionOverlay(under, overs) =>
          objStack += makeImageForPageRegion(under, wbbox)

        case LabeledTarget(target, label, score)   =>
          label.foreach({ l =>
            val bgColor = l match {
              case LB.Title    => "red"
              case LB.Authors  => "blue"
              case LB.Abstract => "yellow"
              case _ => "green"
            }
            val normalScore = score.getOrElse(0d)
            val opacity = normalScore.toFloat * 0.2f

            objStack += Future { createShape(wbbox, "", bgColor, opacity) }
          })

        case  Reflow(tr) =>
          val widget = createTextReflowWidget(tr, wbbox)
          widget.opacity = 1.0f
          noControls(widget)
          objStack += Future { widget }

        case TextBox(tb) =>
          objStack += Future { createTextboxWidget(tb, wbbox) }

        case Row(as)                     =>
        case Col(as)                     =>
        case Figure(fig)              =>

          val g = createShape(wbbox, "blue", "yellow", 0.1f)
          // val figs = fgroup.figures.map { fig =>
          //   createShape(wbbox, "blue", "yellow", 0.1f)
          // }
          // val g = fabric.Group(figs)
          noControls(g)
          objStack += Future { g }

        case Pad(a, padding, maybeColor) =>

          val color = maybeColor.getOrElse(Color.White).toRGB
          val rgba = s"rgba(${color.red}, ${color.green}, ${color.blue}, ${color.alpha})"

          val leftGutter = wbbox.copy(
            width=padding.left
          )

          val rightGutter = wbbox.copy(
            left=wbbox.right-padding.right,
            width=padding.right
          )

          val topGutter = wbbox.copy(
            left=wbbox.left+padding.left,
            width=wbbox.width-(padding.right+padding.left),
            height=padding.top
          )

          val bottomGutter = wbbox.copy(
            left=topGutter.left,
            top=wbbox.bottom-padding.bottom,
            width=topGutter.width,
            height=padding.bottom
          )

          val g = fabric.Group(Seq(
            createShape(leftGutter,   "", rgba, 1f),
            createShape(rightGutter,  "", rgba, 1f),
            createShape(topGutter,    "", rgba, 1f),
            createShape(bottomGutter, "", rgba, 1f)
          ))
          noControls(g)

          objStack += Future { g }

        // case Identified =>
        // case Panel(_, _) =>
        case _ =>
      }

    }

    positions.foreach(visit)


    val fobjs = objStack.toList
      .sequenceU
      .map({ff =>
        ff.map(noControls(_))
        ff
      })

    val totalBounds = positions.head.widgetBounds
    (totalBounds, fobjs)

  }
}
