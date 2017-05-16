package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages


import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue


import scalatags.JsDom
// import JsDom.{TypedTag, tags}
import geometry._
import geometry.syntax._
// import watrmarks._
import labeling._
import LabelWidgetF._


import rx._
import utils.Color
import utils.Colors
import domtags._


import scala.scalajs.js
import scala.scalajs.js.`|`
import org.scalajs.dom
// import org.singlespaced.d3js.selection.Update
// import org.singlespaced.d3js.Selection
import org.singlespaced.d3js.d3
// import org.singlespaced.d3js.Ops._

import scala.scalajs.js
  // import scala.collection.mutable
import js.JSConverters._

trait D3BasicShapes {
  def d3SvgSelection =
    d3.select("#d3-svg-container")
      .select("svg")

  def uiShapeClass = ".ui-shape".clazz



  // def renderPosWidget(p: AbsPosWidget): Option[ElementTag] = {
  def renderPosWidget(p: AbsPosWidget): Option[dom.Element] = {
    val AbsPosWidget(fa, strictBounds, bleedBounds, transVec, zOrder, scaling)  = p

    fa match {
      case RegionOverlay(wid, targetRegion, overlays) =>

        Some(
          <.image(
            uiShapeClass,
            ^.xLinkHref := s"/img/region/${targetRegion.id}",
            ^.x         := strictBounds.left,
            ^.y         := strictBounds.top,
            ^.width     := strictBounds.width,
            ^.height    := strictBounds.height
          ).render
        )

      case  Reflow(wid, tr) =>
        Some(createTextWidget(tr.toString, strictBounds).render)

      case TextBox(wid, tb) =>
        Some(createTextWidget(tb, strictBounds).render)

      case Row(wid, as)                     =>
        // Some(
        //   <.rect(
        //     uiShapeClass,
        //     ^.x         := strictBounds.left,
        //     ^.y         := strictBounds.top,
        //     ^.width     := strictBounds.width,
        //     ^.height    := strictBounds.height
        //   ).render
        // )
        None

      case Col(wid, as)                     =>
        // Some(
        //   <.rect(
        //     uiShapeClass,
        //     ^.x         := strictBounds.left,
        //     ^.y         := strictBounds.top,
        //     ^.width     := strictBounds.width,
        //     ^.height    := strictBounds.height
        //   ).render
        // )
        None

      case Figure(wid, fig)              =>
        val g1 = createShape(fig)
        val tx =  -transVec.x.toInt
        val ty =  -transVec.y.toInt
        Some(
          <.g(
            ^.transform := s"translate($tx $ty)",
            uiShapeClass,
            "ui-fig".clazz,
            g1
          ).render
        )

      case LabelWidgetF.Labeled(wid, a, key, value) =>
        // println(s"creating labeled at strict:${strictBounds} / bleed:${bleedBounds}")
        // val g1 = createTooltip(value, strictBounds)
        // val showRegion = Colorized(
        //   strictBounds,
        //   fg=Colors.Black, bg=Colors.Yellow,
        //   fgOpacity=0.3f, bgOpacity=0.3f
        // )

        // Define the div for the tooltip
        val tooltipdiv = d3.select("body").append("div")
          .attr("class", "tooltip")
          .style("opacity", 0)


        // val hoverArea = createShape(strictBounds).asInstanceOf[JsDom.TypedTag[dom.svg.RectElement]]
        val hoverArea = createShape(strictBounds).asInstanceOf[JsDom.TypedTag[dom.svg.RectElement]]


        val hoverIn: js.Function1[dom.MouseEvent, Unit] =
          (event: dom.MouseEvent) => {

            tooltipdiv.transition()
              .duration(200)
              .style("opacity", .9)


            tooltipdiv.html(value)
              .style("left", (event.pageX) + "px")
              .style("top", (event.pageY - 28) + "px");

            ()
          }
        val hoverOut: js.Function1[dom.MouseEvent, Unit] =
          (event: dom.Event) => {
            tooltipdiv.transition()
              .duration(500)
              .style("opacity", 0);

            ()
          }

        val hr = hoverArea(
          uiShapeClass
        ).render

        hr.onmouseover = hoverIn
        hr.onmouseout = hoverOut

        Some(hr)

      case Pad(wid, a, padding, maybeColor) =>

        val color = maybeColor.getOrElse(Color.White).toRGB
        // val fillColor = s"rgba(${color.red}, ${color.green}, ${color.blue}, ${color.alpha})"
        // val fillOpacit = s"rgba(${color.red}, ${color.green}, ${color.blue}, ${color.alpha})"

        val colorStyle =
          s"""|fill: ${color.cssHash};
              |fill-opacity: 0.2;
              |stroke: ${color.cssHash};
              |stroke-opacity: 1.0;
              |""".stripMargin

        val fringe = makeFringeParts(strictBounds, padding)
        val fs = fringe.map(createShape(_))

        Some(
          <.g(
            uiShapeClass,
            "ui-pad".clazz,
            ^.style:=colorStyle
          )(fs:_*).render
        )

      case Identified(_, _, _, _) => None
      case Panel(_, _, _) => None
      case Terminal => None
    }
  }


  def createTextWidget(text: String, bbox: LTBounds): ElementTag = {

    val spans = text.split("\n").toList
      .zipWithIndex
      .map{ case (line, i) =>
        val leadingSpaces = line.takeWhile(_==' ').length()
        val padl = leadingSpaces*4
        <.tspan(
          ^.x   := (bbox.left+padl),
          ^.y   := (bbox.top+(i*20)),
          line
        )
      }
    val style =
      s"""|style="font-family: Times New Roman;
          |font-size: 20px;
          |stroke: #00ffff;
          |fill: #00ffff;"
          |""".stripMargin

    <.text(
      uiShapeClass,
      ^.style     := style,
      ^.x         := bbox.left,
      ^.y         := bbox.top,
      ^.width     := bbox.width,
      ^.height    := bbox.height
    )(spans)


    // val ftext = fabric.Text(text)
    // ftext.setFontSize(14)
    // ftext.top     = bbox.top
    // ftext.left    = bbox.left
    // val scaleX = bbox.width  / ftext.width.doubleValue
    // val scaleY = bbox.height / ftext.height.doubleValue
    // ftext.setScaleX(scaleX)
    // ftext.setScaleY(scaleY)
    // noControls(ftext)

    // ftext
  }

  def createShape(shape: GeometricFigure): ElementTag = {

    def go(shape0: GeometricFigure, fgColor: String, bgColor: String, fgOpacity: Float, bgOpacity: Float): ElementTag = {
      shape0 match {
        case p: Point =>
          <.circle(
            ^.x := p.x, ^.y := p.y
          )

        case l@ Line(p1: Point, p2: Point) =>
          <.line(
            ^.x1:=p1.x, ^.y1:=p1.y,
            ^.x2:=p2.x, ^.y2:=p2.y,
            ^.stroke        := fgColor,
            ^.fill          := bgColor,
            ^.strokeOpacity := fgOpacity,
            ^.fillOpacity   := bgOpacity
          )

        case bbox:LTBounds =>
          <.rect(
            ^.x             := bbox.left,
            ^.y             := bbox.top,
            ^.width         := bbox.width,
            ^.height        := bbox.height,
            ^.stroke        := fgColor,
            ^.fill          := bgColor,
            ^.strokeOpacity := fgOpacity,
            ^.fillOpacity   := bgOpacity
          )

        case b:LBBounds =>
          go(b.toLTBounds, fgColor, bgColor, fgOpacity, bgOpacity)

        case g @ GeometricGroup(bounds, figs) =>
          val shapes = figs.map(go(_, Colors.Black.cssHash(), bgColor, 0.2f, 0.2f))
            <.g(shapes:_*)

        case g @ Colorized(fig: GeometricFigure, fg: Color, bg: Color, fgOpacity: Float, bgOpacity: Float) =>
          go(fig, fg.cssHash(), bg.cssHash(), fgOpacity, bgOpacity)
      }
    }

    go(shape, "", "", 0f, 0f)
  }
}


// class TData(val i: Int, w: AbsPosWidget)
class JoinElem(val id: Int, val elem: ElementTag)

trait UIUpdateCycle extends D3BasicShapes {

  def doUIUpdateCycle(r: UIRequest): Future[UIResponse]

  def uiRequestCycle(req: UIRequest)(implicit ctx: Ctx.Owner): Future[Unit] = for {
    uiResponse  <- doUIUpdateCycle(req)
  } yield {
    println("complete:uiRequest ")

    joinResponseData(uiResponse.changes)
  }

  def setD3SvgDimensions(changes: Seq[WidgetMod]): Unit = {
    changes.collect({
      case WidgetMod.AddLw(wid, widget) => widget.get
    }).headOption.foreach { firstWidget =>
      d3SvgSelection
        .attr("width", firstWidget.strictBounds.width)
        .attr("height", firstWidget.strictBounds.height)
    }
  }

  def joinResponseData(changes: Seq[WidgetMod]): Unit = {
    setD3SvgDimensions(changes)


    val incomingData: js.Array[AbsPosWidget] = changes.collect({
      case WidgetMod.AddLw(wid, widget) => widget
    }).flatten.toJSArray

    val keyFunc = (t: dom.Node | js.Array[AbsPosWidget], d:js.UndefOr[AbsPosWidget], i:Int) => {
      d.map(_.widget.wid.toString).getOrElse("0")
    }

    val dataFunc: js.Function3[AbsPosWidget, Double, Double, dom.EventTarget] =
      (joinElem:AbsPosWidget, a: Double, b: Double) => {
        renderPosWidget(joinElem)
          .getOrElse(<.g().render)
      }


    val select0 = d3SvgSelection
      .selectAll[dom.EventTarget](".ui-shape")
      .data[AbsPosWidget](incomingData, keyFunc)

    select0
      .enter()
      .append(dataFunc)

    select0.exit()
      .remove()

  }

}
