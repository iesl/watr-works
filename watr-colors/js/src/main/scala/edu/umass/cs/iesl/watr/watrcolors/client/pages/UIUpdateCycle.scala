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

  def uiShapeClass(wid: Int@@WidgetID, cs: String*) = {
    ("ui-shape" :: ("wid-"+wid.unwrap.toString()) :: cs.toList).mkString(" ").clazz
  }


  def renderPosWidget(p: AbsPosWidget): Option[dom.Element] = {
    val AbsPosWidget(fa, strictBounds, bleedBounds, transVec, zOrder, scaling)  = p

    fa match {
      case RegionOverlay(wid, targetRegion, overlays) =>
        Some(
          <.image(
            uiShapeClass(wid, "reg-overlay"),
            ^.xLinkHref := s"/img/region/${targetRegion.id}",
            ^.x         := strictBounds.left.asInt,
            ^.y         := strictBounds.top.asInt,
            ^.width     := strictBounds.width.asInt,
            ^.height    := strictBounds.height.asInt
          ).render
        )

      case  Reflow(wid, tr) =>
        Some(createTextWidget(tr.toString, strictBounds)(
          uiShapeClass(wid, "reflow")
        ).render)

      case TextBox(wid, tb) =>
        Some(createTextWidget(tb.toString, strictBounds)(
          uiShapeClass(wid, "textbox")
        ).render)


      case Figure(wid, fig) =>
        // TODO use radial gradient for
        // <defs>
        //   <radialGradient id="exampleGradient">
        //   <stop offset="70%" stop-color="yellow" stop-opacity="0.1"/>
        //   <stop offset="95%" stop-color="black" stop-opacity="0.3"/>
        //   </radialGradient>
        //  </defs>
        val g1 = createShape(fig)
        val Point.Doubles(tx, ty) = transVec
        // val tx =  transVec.x.asInt
        // val ty =  transVec.y.asInt
        Some(
          <.g(
            ^.transform := s"translate($tx $ty)",
            uiShapeClass(wid, "ui-fig"),
            wid.toString.id,
            g1
          ).render
        )

      case LabelWidgetF.Labeled(wid, a, key, value) =>

        val hoverIn: js.Function1[dom.MouseEvent, Unit] =
          (event: dom.MouseEvent) => {
            // Remove any old tooltips
            d3.select(s"#tooltip-${wid}").remove()

            d3.select("body").append("div")
              .attr("class", s"tooltip ${value}")
              .attr("id", s"tooltip-${wid}")
              .html(value)
              .style("left", (event.pageX) + "px")
              .style("top", (event.pageY - 28) + "px")
              .transition()
                .duration(200)
                .style("opacity", .9)
              .transition()
                .duration(4000)
              .transition()
                .duration(200)
                .style("opacity", 0)
              .transition()
                .remove()


            ()
          }
        val hoverOut: js.Function1[dom.MouseEvent, Unit] =
          (event: dom.Event) => {
            d3.select(s"#tooltip-${wid}")
              .transition()
              .duration(200)
              .style("opacity", 0)
              .transition()
              .remove()

            ()
          }

        val hoverArea = createShape(makeTransparent(strictBounds))(
          uiShapeClass(wid, "ui-labeled", value)
        ).asInstanceOf[JsDom.TypedTag[dom.html.Element]].render


        hoverArea.onmouseover = hoverIn
        hoverArea.onmouseout = hoverOut

        Some(hoverArea)


      case Pad(wid, a, padding, maybeColor) =>


        val fringe = makeFringe(strictBounds, padding)
        val fringeColored = maybeColor.map{ color =>
          Colorized(
            fringe,
            fg=color, bg=color,
            fgOpacity=1f, bgOpacity=1f
          )
        } getOrElse {
          fringe
        }

        Some(
          createShape(fringeColored)(
            uiShapeClass(wid, "ui-pad")
          ).render
        )

        // Some(
        //   <.g(
        //     uiShapeClass(wid, "ui-pad"),
        //     ^.style:=colorStyle
        //   )(fs:_*).render
        // )


      case Row(wid, as)           => None
      case Col(wid, as)           => None
      case ZStack(wid, as)        => None
      case Identified(_, _, _, _) => None
      case Panel(_, _, _)         => None
      case Terminal               => None
    }
  }

  def makeTransparent(g: GeometricFigure): GeometricFigure = {
    Colorized(
      g, fg=Color.White, bg=Color.White,
      fgOpacity=0f, bgOpacity=0f
    )
  }


  def createTextWidget(text: String, bbox: LTBounds): ElementTag = {

    val spans = text.split("\n").toList
      .zipWithIndex
      .map{ case (line, i) =>
        val leadingSpaces = line.takeWhile(_==' ').length()
        val padl = leadingSpaces*4d

        <.text(
          ^.x   := (bbox.left+padl).asInt,
          ^.y   := (bbox.top+(i*20d)).asInt,
          line
        )
      }
    val style =
      s"""|style="font-family: Times New Roman;
          |font-size: 20px;
          |stroke: #020202;
          |fill: #020202;"
          |""".stripMargin

    <.g(
      ^.style     := style,
      ^.x         := bbox.left.asInt,
      ^.y         := bbox.top.asInt,
      ^.width     := bbox.width.asInt,
      ^.height    := bbox.height.asInt
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
    def go(shape0: GeometricFigure, fgColor: Option[String], bgColor: Option[String], fgOpacity: Option[Float], bgOpacity: Option[Float]): ElementTag = {
      val svg0 = shape0 match {
        case p: Point =>
          <.circle(
            ^.x := p.x.asInt, ^.y := p.y.asInt
          )
        case l@ Line(p1: Point, p2: Point) =>
          <.line(
            ^.x1:=p1.x.asInt,     ^.y1:=p1.y.asInt,
            ^.x2:=p2.x.asInt,     ^.y2:=p2.y.asInt
          )

        case bbox:LTBounds =>
          <.rect(
            ^.x := bbox.left.asInt,  ^.y := bbox.top.asInt,
            ^.width := bbox.width.asInt, ^.height := bbox.height.asInt
          )

        case b:LBBounds =>
          go(b.toLTBounds, fgColor, bgColor, fgOpacity, bgOpacity)

        case g @ GeometricGroup(bounds, figs) =>
          val shapes = figs.map(go(_, Option(Colors.Black.cssHash()), bgColor, Some(0.2f), Some(0.2f)))

          <.g(shapes:_*)

        case g @ Colorized(fig: GeometricFigure, fg: Color, bg: Color, fgOpacity: Float, bgOpacity: Float) =>
          go(fig, Some(fg.cssHash()), Some(bg.cssHash()), Some(fgOpacity), Some(bgOpacity))

      }
      // val extraAttrs = List(
      //   ^.stroke        := fgColor.getOrElse(""),
      //   ^.fill          := bgColor.getOrElse(""),
      //   ^.strokeOpacity := fgOpacity.getOrElse(0f),
      //   ^.fillOpacity   := bgOpacity.getOrElse(0f)
      // )

      val extraAttrs = List(
        fgColor.map(^.stroke := _),
        bgColor.map(^.fill := _),
        fgOpacity.map(^.strokeOpacity := _),
        bgOpacity.map(^.fillOpacity := _)
      ).flatten

      svg0(extraAttrs)
    }

    go(shape, None, None, None, None)
  }
}


trait UIUpdateCycle extends D3BasicShapes {

  def doUIUpdateCycle(r: UIRequest): Future[UIResponse]
  def updateUIState(state: UIState): Unit

  def uiRequestCycle(req: UIRequest)(implicit ctx: Ctx.Owner): Future[Unit] = for {
    uiResponse  <- doUIUpdateCycle(req)
  } yield {
    println("complete:uiRequest ")
    uiResponse.changes.foreach { mods =>
      joinResponseData(mods)
    }
    updateUIState(uiResponse.uiState)
  }

  def setD3SvgDimensions(changes: Seq[WidgetMod]): Unit = {
    changes.headOption.foreach { _ match {
      case WidgetMod.Added(wid, widget) =>
        val bbox = widget.get.strictBounds
        d3SvgSelection
          .attr("width", bbox.width.asInt)
          .attr("height", bbox.height.asInt)
      case _ =>
    }}
  }

  type KeyFunction = js.ThisFunction2[dom.Node|js.Array[WidgetMod],js.UndefOr[WidgetMod], Int, String]
  type MyDatumFunction = js.Function3[WidgetMod, Int, js.UndefOr[Int], dom.EventTarget]

  def joinResponseData(changes: Seq[WidgetMod]): Unit = {
    // changes.foreach { mod =>
    //   mod match {
    //     case WidgetMod.Added(wid, widget) => println(s"${mod}")
    //     case WidgetMod.Removed(wid) => println(s"${mod}")
    //     case _ =>
    //   }
    // }

    setD3SvgDimensions(changes)

    val incomingData: js.Array[WidgetMod] =
      changes.collect({
        case w: WidgetMod.Unmodified  => w
        case w: WidgetMod.Added       => w
      }).map(_.asInstanceOf[WidgetMod])
        .toJSArray


    val keyFunc: KeyFunction = (t: dom.Node|js.Array[WidgetMod], d:js.UndefOr[WidgetMod], i:Int) => {
      d.map(_.id.toString).getOrElse(sys.error(s"keyFunc error on t:${t} d:${d}, i:${i}"))
    }

    val emptyTarget = <.g(".empty".clazz).render

    val dataFunc: js.Function3[WidgetMod, Double, Double, dom.EventTarget] =
      (mod:WidgetMod, a: Double, b: Double) => {
        mod match {
          case WidgetMod.Added(wid, widget) =>
            val maybeElem = for {
              w <- widget
              e <- renderPosWidget(w)
            } yield e

            maybeElem.getOrElse(emptyTarget)

          case _ =>
            emptyTarget
        }
      }


    val select0 = d3SvgSelection
      .selectAll[dom.EventTarget](".ui-shape")
      .data[WidgetMod](incomingData, keyFunc)

    select0
      .enter()
      .append(dataFunc)

    select0.exit()
      .remove()

  }

}
