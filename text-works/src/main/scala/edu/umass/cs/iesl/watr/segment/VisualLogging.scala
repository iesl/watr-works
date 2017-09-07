package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}, fs._

import geometry._
import spindex._
import utils.Colors
import utils.Color
import ammonite.{ops => fs}
import scala.collection.mutable
import play.api.libs.json, json._

class VisualLogger(
  logName: String,
  pageIndex: PageIndex,
  outputRoot: Path
) {
  val rTreeIndex = pageIndex.componentRTree
  val pageNum = pageIndex.pageGeometry.id

  val logs = mutable.ListBuffer[JsObject]()

  val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageIndex.pageGeometry.bounds
  val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageIndex.pageGeometry.bounds

  def jsonLogFile(name: String) = s"${name}.pg${pageNum}.json"


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



  def logRegions(name: String, bboxes: Seq[LTBounds], lineColor: Color=Colors.Blue, fillColor: Color=Colors.Yellow): Unit = {
    val boxBlock = bboxes.map{ bbox =>
      val LTBounds.Doubles(l, t, w, h) = rescale(bbox)

      Json.obj(
        ("x" -> JsNumber(l)),     ("y" -> JsNumber(t)),
        ("width" -> JsNumber(w)), ("height" -> JsNumber(h))
      )
    }

    val obj = Json.obj(
      ("desc" -> JsString(name)),
      ("shapes" -> boxBlock)
    )

    logs += obj
  }



  def writeLogs(): Unit = {

    val logJson = Json.toJson(logs.toList)
    val jsonStr = Json.prettyPrint(logJson)

    if (!fs.exists(outputRoot)) {
      fs.mkdir(outputRoot)
    }

    val outPath = outputRoot / jsonLogFile(logName)

    if (fs.exists(outPath)) {
      fs.rm(outPath)
    }

    fs.write(outPath, jsonStr)

  }

}
