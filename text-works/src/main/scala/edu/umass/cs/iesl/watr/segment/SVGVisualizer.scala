package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}, fs._
// import edu.umass.cs.iesl.watr.tracing.VisualTracer
// import watrmarks._

import geometry._
import spindex._
import utils.Colors
import utils.Color
import ammonite.{ops => fs}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SVGVisualization(
  visName: String,
  pageIndex: PageIndex,
  outputRoot: Path
  // labelColors: Map[Label, Color],
  // vtrace: VisualTracer
) extends ScalatagsDefs {
  import texttags._
  val rTreeIndex = pageIndex.componentRTree
  val pageNum = pageIndex.pageGeometry.id

  val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageIndex.pageGeometry.bounds
  val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageIndex.pageGeometry.bounds

  def segvisFile(name: String) = s"${name}.pg${pageNum}.svg"

  val svgStack = ListBuffer[TextTag]()

  def init(): TextTag = {
    val LTBounds.Doubles(l, t, w, h) = pageIndex.pageGeometry.bounds

    <.svg(
      ^.width := svgW,
      ^.height := svgH,
      ^.xmlns := "http://www.w3.org/2000/svg"
    )

  }

  // def rescale(bboxScale1: LTBounds, pageScale1: LTBounds, pageScale2: LTBounds): LTBounds = {
  def rescale(bboxScale1: LTBounds): LTBounds = {
    val LTBounds.Doubles(l, t, w, h) = bboxScale1

    val scaleX = pageW / svgW
    val scaleY = pageH / svgH
    val l2 = l * scaleX
    val t2 = t * scaleY
    val w2 = w * scaleX
    val h2 = h * scaleY

    LTBounds.Doubles(l2, t2, w2, h2)
  }


  def write(): Unit = {

    val result = svgStack.reverse.foldLeft(init){
      case (acc, e) =>
        acc.apply(e)
    }

    val htmlWrap = <.html(
      <.head(
        <.script(^.src := "https://d3js.org/d3.v4.min.js")
      ),
      <.body(result)
    )

    if (!fs.exists(outputRoot)) {
      fs.mkdir(outputRoot)
    }

    val outPath = outputRoot / segvisFile(visName)

    if (fs.exists(outPath)) {
      fs.rm(outPath)
    }
    fs.write(outPath, htmlWrap.toString())
  }


  def indicateRegion(name: String, bbox: LTBounds, lineColor: Color=Colors.Blue, fillColor: Color=Colors.Yellow): Unit = {
    val LTBounds.Doubles(l, t, w, h) = rescale(bbox)
    svgStack += <.g(
      <.rect(
        ^.x := l,
        ^.y := t,
        ^.width := w,
        ^.height := h,
        *.fill := fillColor.toCss,
        *.opacity := 0.3,
        *.stroke := lineColor.toCss,
        *.strokeWidth := 3
      )
    )

  }

  def indicateRegions(name: String, bboxes: Seq[LTBounds]): Unit = {
  }

  import play.api.libs.json, json._
  def jstr(s: String) = JsString(s)
  def num(s: Double) = JsNumber(s)

  val logs = mutable.ListBuffer[JsObject]()

  def logRegions(name: String, bboxes: Seq[LTBounds], lineColor: Color=Colors.Blue, fillColor: Color=Colors.Yellow): Unit = {
    val boxBlock = bboxes.map{ bbox =>
      val LTBounds.Doubles(l, t, w, h) = rescale(bbox)

      Json.obj(
        ("x" -> num(l)),     ("y" -> num(t)),
        ("width" -> num(w)), ("height" -> num(h))
      )
    }

    val obj = Json.obj(
      ("desc" -> jstr(name)),
      ("shapes" -> boxBlock)
    )

    logs += obj
  }


  def jsonLogFile(name: String) = s"${name}.pg${pageNum}.json"

  def writeLogs(): Unit = {

    val logJson = Json.toJson(logs.toList)
    val jsonStr = Json.prettyPrint(logJson)

    if (!fs.exists(outputRoot)) {
      fs.mkdir(outputRoot)
    }

    val outPath = outputRoot / jsonLogFile(visName)

    if (fs.exists(outPath)) {
      fs.rm(outPath)
    }

    fs.write(outPath, jsonStr)

  }

}
