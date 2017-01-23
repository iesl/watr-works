package edu.umass.cs.iesl.watr
package watrcolors
package client


import scala.scalajs.js

import textreflow._
import geometry._

import PageComponentImplicits._

import native.fabric
import native.fabric._

import scala.concurrent.{ Future, Promise }

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue


trait LabelerRendering extends PlainTextReflow with FabricCanvasOperations {
  import TextReflowF._
  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._

  override lazy val fabricCanvas =  {
    initFabric("canvas")
    getFabric("canvas")
  }


  def extractVisualLineTargetRegions(tr: TextReflow): Seq[TargetRegion] = {
    def render(t: TextReflowF[(TextReflow, Seq[TargetRegion])]): Seq[TargetRegion] = t match {
      case Rewrite    ((from, attr), to)      => attr
      case Bracket    (pre, post, (a, attr))  => attr
      case Flow       (atomsAndattrs)         => atomsAndattrs.flatMap(_._2)
      case Labeled    (labels, (a, attr))     =>
        val trs = for {
          l <- labels if l == LB.VisualLine
          value <- l.value
        } yield {
          TargetRegion.fromUri(value)
        }

        attr ++ trs

      case _ => Seq()
    }

    tr.cata(attributePara(render))
      .toPair._1
  }

  def displayBasicCanvasShapes(): Unit = {
    fabricCanvas.add(createShape(Point(240, 240), "black", "blue", 0.5f))
    fabricCanvas.add(createShape(LBBounds(240, 240, 100, 200), "red", "black", 0.5f))
    fabricCanvas.add(createShape(LTBounds(240, 240, 100, 200), "black", "yellow", 0.5f))
    fabricCanvas.add(createShape(Line(Point(240, 40), Point(340, 440)), "black", "green", 0.5f))
  }

  def addBorder(width: Double, fobj: FabricObject): FabricObject = {
    val bbox = fabricObjectLTBounds(fobj)
    val tbBbox = LTBounds(0, 0,
      bbox.width,
      width
    )
    val lrBbox = LTBounds(0, 0,
      width,
      bbox.height + width*2
    )

    val top = createShape(tbBbox, "black", "black", 0.5f)
    val bottom = createShape(tbBbox, "blue", "blue", 0.5f)
    val left = createShape(lrBbox, "blue", "blue", 0.5f)
    val right = createShape(lrBbox, "blue", "blue", 0.5f)

    val group = hjoin(List(
      left,
      vjoin(List(top, fobj, bottom)),
      right
    ))
    group.setLeft(0)
    group.setTop(0)

    group
  }

  def fabricObjectLTBounds(fobj: FabricObject): LTBounds = {
    val t = fobj.top
    val l = fobj.left
    val w = fobj.width
    val h = fobj.height

    LTBounds(l.doubleValue(), t.doubleValue(),
      w.doubleValue(), h.doubleValue())
  }

  // def makeTargetRegionImage(targetRegion: TargetRegion): Unit = {
  //   val bbox = targetRegion.bbox
  //   val targetRegionURI = targetRegion.uriString

  //   val scb = (img:Image) => {
  //     img.top = bbox.top
  //     img.left = bbox.left
  //     img.width = bbox.width
  //     img.height = bbox.height

  //     fabricCanvas.add(img)
  //     fabricCanvas.renderAll()
  //     ()
  //   }
  //   val jscb: js.Function1[Image, Unit] = scb

  //   Image.fromURL(s"/img/${targetRegionURI}", jscb)
  // }


  def makeImageForTargetRegion(tr: TargetRegion): Future[FabricObject] = {
    val targetRegionURI = tr.uriString

    val promise = Promise[FabricObject]()

    val callback: js.Function1[Image, Unit] =
      (img:Image) => {
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

  def hjoin(fobjs: Seq[FabricObject]): FabricObject = {
    var currLeft: Int = 0
    fobjs.foreach { fobj =>
      fobj.setLeft(currLeft)
      currLeft = (currLeft + fobj.width.intValue())
    }
    val g = fabric.Group(fobjs)
    noControls(g)
    g
  }
  def vjoin(fobjs: Seq[FabricObject]): FabricObject = {
    var currTop: Int = 0
    fobjs.foreach { fobj =>
      fobj.setTop(currTop)
      currTop = (currTop + fobj.height.intValue())
    }
    val g = fabric.Group(fobjs)

    noControls(g)
    g
  }

  def vcatWidgets(trs: Seq[TextReflow]): Unit = {
    var currTop: Int = 0
    trs.foreach { tr =>
      val widget = createAnnotWidget(tr)
      widget.setTop(currTop)
      currTop = (currTop + widget.height.intValue())
      fabricCanvas.add(widget)
    }
  }

  import display._
  import LabelWidgetF._

  import scalaz.std.list._
  import scalaz.std.scalaFuture._
  import scalaz.syntax.traverse._

  def renderLabelWidget(lwidget: LabelWidget): Future[FabricObject] = {
    def visit(t: LabelWidgetF[(LabelWidget, Future[FabricObject])]): Future[FabricObject] = t match {
      case Panel((content, attr))  =>
        attr.map({ fobj =>
          addBorder(4.0, fobj)
        })

      case Target(tr, emboss)  =>
        makeImageForTargetRegion(tr)

      case MouseOverlay((bkplane, attr), selects) =>
        // createShape(tr.bounds, "black", "yellow", 1f)
        attr
      case Col(attrs) =>
        val lls: List[Future[FabricObject]] = attrs.map(_._2)

        lls.sequenceU
          .map({ widgets =>
            vjoin(widgets)
          })


      case _ => sys.error("echoLabeler: TODO")
    }

    lwidget
      .cata(attributePara(visit))
      .toPair._1
  }
}
