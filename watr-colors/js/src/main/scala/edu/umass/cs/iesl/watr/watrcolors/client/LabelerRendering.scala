package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.js

import textreflow._
import geometry._
import GeometryImplicits._

import PageComponentImplicits._

import native.fabric
import native.fabric._

import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.std.scalaFuture._
import scala.collection.mutable

import scala.concurrent.{ Future, Promise }

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import labeling._
import LabelWidgetF._
import watrmarks.{StandardLabels => LB}
import textboxing.{TextBoxing => TB}

case class LwRenderingAttrs(
  fobj: FabricObject,
  regions: List[(TargetRegion, LTBounds)]
)

trait LabelerRendering extends PlainTextCorpus with FabricCanvasOperations {
  import TextReflowF._
  import matryoshka._

  override lazy val fabricCanvas =  {
    initFabric("canvas")
    getFabric("canvas")
  }



  def displayBasicCanvasShapes(): Unit = {
    fabricCanvas.add(createShape(Point(240, 240), "black", "blue", 0.5f))
    fabricCanvas.add(createShape(LBBounds(240, 240, 100, 200), "red", "black", 0.5f))
    fabricCanvas.add(createShape(LTBounds(240, 240, 100, 200), "black", "yellow", 0.5f))
    fabricCanvas.add(createShape(Line(Point(240, 40), Point(340, 440)), "black", "green", 0.5f))
  }

  def addBorder(width: Double, renderAttr: LwRenderingAttrs): LwRenderingAttrs = {
    val tbBbox = LTBounds(0, 0,
      fabricObjectLTBounds(renderAttr.fobj).width,
      width
    )
    val top    =  createShape(tbBbox, "black", "black", 0.5f)
    val bottom =  createShape(tbBbox, "blue", "blue", 0.5f)

    val tbGroup = vjoinAttrs(List(
      LwRenderingAttrs(top, List()),
      renderAttr,
      LwRenderingAttrs(bottom, List())
    ))

    val lrBbox = LTBounds(0, 0,
      width,
      fabricObjectLTBounds(tbGroup.fobj).height + width*2
    )
    val left   = createShape(lrBbox, "red", "red", 0.5f)
    val right  = createShape(lrBbox, "red", "red", 0.5f)


    hjoinAttrs(List(
      LwRenderingAttrs(left, List()),
      tbGroup,
      LwRenderingAttrs(right, List())
    ))
  }

  def fabricObjectLTBounds(fobj: FabricObject): LTBounds = {
    val t = fobj.top
    val l = fobj.left
    val w = fobj.width
    val h = fobj.height

    LTBounds(l.doubleValue(), t.doubleValue(),
      w.doubleValue(), h.doubleValue())
  }


  def makeImageForTargetRegion(tr: TargetRegion, absPos: LTBounds): Future[FabricObject] = {
    val targetRegionURI = tr.uriString

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
        println("makeImageForTargetRegion")
        println(img)

        promise.success(img)
        ()
      }

    Image.fromURL(s"/img/${targetRegionURI}", callback)
    promise.future

  }

  // def makePlaceholderImgs(trs: Seq[TargetRegion]): Seq[FabricObject] = {
  //   val objs = trs.zipWithIndex.map({case (tr, i) =>
  //     val bbox = tr.bbox.copy(
  //       left=20, top=((i+1)*20).toDouble
  //     )

  //     val shape = createShape (bbox , "black", "yellow", 0.5f)

  //     val targetRegionURI = tr.uriString
  //     val scb = (img:Image) => {
  //       img.top = bbox.top
  //       img.left = bbox.left
  //       img.width = bbox.width
  //       img.height = bbox.height

  //       fabricCanvas.add(img)
  //       fabricCanvas.renderAll()
  //       ()
  //     }
  //     val jscb: js.Function1[Image, Unit] = scb

  //     val img = Image.fromURL(s"/img/${targetRegionURI}", jscb)

  //     shape
  //   })
  //   objs
  // }


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

  def hjoinAttrs(attrs: List[LwRenderingAttrs]): LwRenderingAttrs = {
    var currLeft: Int = 0
    val objsAndBounds = attrs.map({attr =>
      val shiftedBounds = attr.regions
        .map({case (tr, bbox) =>
          (tr, bbox.moveTo(bbox.left+currLeft, y=bbox.top))
        })

      attr.fobj.setLeft(currLeft)

      currLeft = (currLeft + attr.fobj.width.intValue())
      (attr.fobj, shiftedBounds)
    })

    val fobjs = objsAndBounds.map(_._1)
    val bounds = objsAndBounds.flatMap(_._2)

    val g = fabric.Group(fobjs)
    noControls(g)

    LwRenderingAttrs(
      g, bounds
    )
  }
  def vjoinAttrs(attrs: List[LwRenderingAttrs]): LwRenderingAttrs = {
    var currTop: Int = 0
    val objsAndBounds = attrs.map({attr =>
      val shiftedBounds = attr.regions
        .map({case (tr, bbox) =>
          (tr, bbox.moveTo(bbox.left, y=bbox.top+currTop))
        })

      attr.fobj.setTop(currTop)

      currTop = (currTop + attr.fobj.height.intValue())
      (attr.fobj, shiftedBounds)
    })

    val fobjs = objsAndBounds.map(_._1)
    val bounds = objsAndBounds.flatMap(_._2)

    val g = fabric.Group(fobjs)
    noControls(g)

    LwRenderingAttrs(
      g, bounds
    )
  }


  def renderLabelWidget(positions: List[PosAttr]): (LTBounds, Future[List[FabricObject]]) = {

    val objStack = mutable.ArrayBuffer[Future[FabricObject]]()

    def visit(p: PosAttr): Unit = {
      val PosAttr(fa, wbbox, id, selfOffset, childOffsets)  = p

      fa match {
        case TargetOverlay(under, overs) =>
          objStack += makeImageForTargetRegion(under, wbbox)
          // objStack += Future { createShape(wbbox, "green", "", 0.2f) }

        case LabeledTarget(target, label, score)   =>
          label.foreach({ l =>
            val bgColor = l match {
              case LB.Title    => "red"
              case LB.Authors  => "blue"
              case LB.Abstract => "yellow"
              case _ => ""
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

        case MouseOverlay(bkplane)       =>

        case Panel(content)              =>
          objStack += Future { createShape(wbbox, "black", "", 0.1f) }

        case Pad(a, padding) =>
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
            createShape(leftGutter, "", "red", 0.2f),
            createShape(rightGutter, "", "red", 0.2f),
            createShape(topGutter, "", "red", 0.2f),
            createShape(bottomGutter, "", "red", 0.2f)
          ))
          noControls(g)

          // objStack += Future { g }

        case Button(action) =>
          objStack += Future { createButtonWidget(action, wbbox) }

        case Row(as)                     =>
          // objStack += Future { createShape(wbbox, "black", "", 0.0f) }

        case Col(as)                     =>
          // objStack += Future { createShape(wbbox, "blue", "", 0.0f) }

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
