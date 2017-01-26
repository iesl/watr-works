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

// import utils.{CompassDirection => CDir}

// import scalaz.std.list._
// import scalaz.std.scalaFuture._
// import scalaz.syntax.traverse._

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


  // def extractVisualLineTargetRegions(tr: TextReflow): Seq[TargetRegion] = {
  //   def render(t: TextReflowF[(TextReflow, Seq[TargetRegion])]): Seq[TargetRegion] = t match {
  //     case Rewrite    ((from, attr), to)      => attr
  //     case Bracket    (pre, post, (a, attr))  => attr
  //     case Flow       (atomsAndattrs)         => atomsAndattrs.flatMap(_._2)
  //     case Labeled    (labels, (a, attr))     =>
  //       val trs = for {
  //         l <- labels if l == LB.VisualLine
  //         value <- l.value
  //       } yield {
  //         TargetRegion.fromUri(value)
  //       }

  //       attr ++ trs

  //     case _ => Seq()
  //   }

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


  def renderLabelWidget(lwidget: LabelWidget): Future[FabricObject] = {

    // case Target(tr, emboss, sels)  =>  makeImageForTargetRegion(tr)
    // case Col(attrs) => lls.map(vjoinAttrs(_))
    // case Panel((content, attr))  =>   attr.map({ fobj =>  addBorder(4.0, fobj)  })
    // case MouseOverlay((bkplane, fattr)) => createShape(tr.bounds, "black", "yellow", 1f)
    val zeroShape = createShape(LTBounds(0, 0, 0, 0), "black", "yellow", 0f)

    // var currObj = Future { zeroShape }

    def makeObj(t: LabelWidget): Option[FabricObject] = t.project match {
      case l @ Positioned(Fix(fa), pvec, area, id)    => fa match {
        case TargetImage(tr) =>
          Some(createShape(area, "black", "yellow", 1f))
        case  TargetSelection(bk, sels)   =>
          Some(createShape(area, "black", "yellow", 1f))
        case  RangeSelection(range)       =>
          Some(createShape(area, "black", "yellow", 1f))
        case  Reflow(tr)                  =>
          Some(createShape(area, "black", "yellow", 1f))
        case  Button()                    =>
          Some(createShape(area, "black", "yellow", 1f))
        case  MouseOverlay(bkplane)       =>
          Some(createShape(area, "black", "yellow", 1f))
        case  Panel(content)              =>
          Some(createShape(area, "black", "yellow", 1f))
        case  Row(as)                     =>
          Some(createShape(area, "black", "yellow", 1f))
        case  Col(as)                     =>
          Some(createShape(area, "black", "yellow", 1f))
        case  Overlay(overs, under)       =>
          Some(createShape(area, "black", "yellow", 1f))
      }
      case _ => None
    }

    val objs = lwidget.universe.map({ tf =>
      makeObj(tf)
    })

    val g = fabric.Group(objs.flatten)
    noControls(g)

    Future { g }

  }
}
