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

import scala.concurrent.{ Future, Promise }

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import display._
import LabelWidgetF._
import watrmarks.{StandardLabels => LB}
import textboxing.{TextBoxing => TB}

case class LwRenderingAttrs(
  fobj: FabricObject,
  regions: List[(TargetRegion, LTBounds)]
)

trait LabelerRendering extends PlainTextReflow with FabricCanvasOperations {
  import TextReflowF._
  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._

  override lazy val fabricCanvas =  {
    initFabric("canvas")
    getFabric("canvas")
  }


  //   tr.cata(attributePara(render))
  //     .toPair._1
  // }

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
        img.opacity = 1.0f
        noControls(img)
        promise.success(img)
        ()
      }

    Image.fromURL(s"/img/${targetRegionURI}", callback)
    promise.future

  }

  def makePlaceholderImgs(trs: Seq[TargetRegion]): Seq[FabricObject] = {
    val objs = trs.zipWithIndex.map({case (tr, i) =>
      val bbox = tr.bbox.copy(
        left=20, top=((i+1)*20).toDouble
      )

      val shape = createShape (bbox , "black", "yellow", 0.5f)

      val targetRegionURI = tr.uriString
      val scb = (img:Image) => {
        img.top = bbox.top
        img.left = bbox.left
        img.width = bbox.width
        img.height = bbox.height

        fabricCanvas.add(img)
        fabricCanvas.renderAll()
        ()
      }
      val jscb: js.Function1[Image, Unit] = scb

      val img = Image.fromURL(s"/img/${targetRegionURI}", jscb)

      shape
    })
    objs
  }


  def createTextboxWidget(textbox: TB.Box, bbox: LTBounds): FabricObject = {
    val text = textbox.toString
    val ftext = fabric.Text(text)
    ftext.setFontSize(14)
    ftext.top     = bbox.top
    ftext.left    = bbox.left
    val scaleX = bbox.width  / ftext.width.doubleValue
    val scaleY = bbox.height / ftext.height.doubleValue
    ftext.setScaleX(scaleX)
    ftext.setScaleY(scaleY)

    ftext
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

  def createAnnotWidget(textReflow: TextReflow): fabric.Group = {
    val text = textReflow.toText()
    val vlineIds = extractVisualLineTargetRegions(textReflow)
    val ftext = fabric.Text(text)
    ftext.setFontSize(15)

    val placeholders = makePlaceholderImgs(vlineIds)
    val widgetGroup = fabric.Group(
      ftext +: placeholders
    )

    widgetGroup
  }

  // def hjoin(fobjs: Seq[FabricObject]): FabricObject = {
  //   var currLeft: Int = 0
  //   fobjs.foreach { fobj =>
  //     fobj.setLeft(currLeft)
  //     currLeft = (currLeft + fobj.width.intValue())
  //   }
  //   val g = fabric.Group(fobjs)
  //   noControls(g)
  //   g
  // }


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

  // def vjoinAttrs(attrs: List[LwRenderingAttrs]): LwRenderingAttrs = {
  //   var currTop: Int = 0
  //   val objsAndBounds = attrs.map({attr =>
  //     val shiftedBounds = attr.regions
  //       .map({case (tr, bbox) =>
  //         (tr, bbox.moveTo(bbox.left, y=bbox.top+currTop))
  //       })
  //     val futureFobj = attr.fobj.map { fobj =>
  //       fobj.setTop(currTop)
  //       currTop = (currTop + fobj.height.intValue())
  //       fobj
  //     }
  //     (futureFobj, shiftedBounds)
  //   })

  //   val fobjs = objsAndBounds.map(_._1)
  //   val bounds = objsAndBounds.flatMap(_._2)
  //   val fgroup = fobjs.sequenceU
  //     .map({objs =>
  //       val g = fabric.Group(objs)
  //       noControls(g)
  //       g
  //     })

  //   LwRenderingAttrs(
  //     fgroup, bounds
  //   )
  // }

  // def vjoin(fobjs: Seq[FabricObject]): FabricObject = {
  //   var currTop: Int = 0
  //   fobjs.foreach { fobj =>
  //     fobj.setTop(currTop)
  //     currTop = (currTop + fobj.height.intValue())
  //   }
  //   val g = fabric.Group(fobjs)

  //   noControls(g)
  //   g
  // }

  // def vcatWidgets(trs: Seq[TextReflow]): Unit = {
  //   var currTop: Int = 0
  //   trs.foreach { tr =>
  //     val widget = createAnnotWidget(tr)
  //     widget.setTop(currTop)
  //     currTop = (currTop + widget.height.intValue())
  //     fabricCanvas.add(widget)
  //   }
  // }


  import scalaz.std.list._
  import scalaz.syntax.traverse._
  import scalaz.std.scalaFuture._
  import scala.collection.mutable

  def renderLabelWidget(lwidget: LabelWidget): (LTBounds, Future[List[FabricObject]]) = {

    val objStack = mutable.ArrayBuffer[Future[FabricObject]]()

    def visit(lwidget: LabelWidget): Unit = lwidget.project match {
      case p @ Positioned( tf @ Fix(fa), pvec, wbbox, tbbox, id)  =>
        fa match {
          case TargetOverlay(under, overs) =>
            objStack += makeImageForTargetRegion(under, wbbox)

          case LabeledTarget(target, label, score)   =>
            val bgColor = label match {
              case Some(LB.Title)    => "red"
              case Some(LB.Authors)  => "blue"
              case Some(LB.Abstract) => "yellow"
              case _ => ""
            }
            val normalScore = score.getOrElse(0d)
            val opacity = normalScore.toFloat * 0.2f

            objStack += Future { createShape(wbbox, "", bgColor, opacity) }

          case  Reflow(tr) =>
            val widget = createTextReflowWidget(tr, wbbox)
            widget.opacity = 1.0f
            noControls(widget)
            objStack += Future { widget }

          case TextBox(tb) =>
            objStack += Future { createTextboxWidget(tb, wbbox) }

          case  MouseOverlay(bkplane)       =>
          case  Panel(content)              =>

          case  Row(as)                     =>
            objStack += Future { createShape(wbbox, "black", "", 0.0f) }

          case  Col(as)                     =>
            objStack += Future { createShape(wbbox, "blue", "", 0.0f) }

          case _ =>
        }

      case _ =>
    }

    // lwidget.topDownCata(Point(0, 0))(reposition)
    lwidget.universe.foreach(visit)

    def position(lw: LabelWidget): (PositionVector, LTBounds) = lw.unFix match {
      case Positioned(a, pvec, wbbox, tbbox, id) => (pvec, tbbox)
      case x => sys.error(s"found non-positioned LabelWidget ${x}")
    }

    val (_, widgetBounds) = position(lwidget)

    val fobjs = objStack.toList
      .sequenceU
      .map({ff =>
        ff.map(noControls(_))
        ff
      })

    (widgetBounds, fobjs)

  }
}
