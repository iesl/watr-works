package edu.umass.cs.iesl.watr
package utils

// import geometry._
// import utils.ExactFloats._
// import utils.{RelativeDirection => Dir}
import scala.scalajs.js
import scala.scalajs.js.annotation._

import GraphPaper._


@js.native
trait DrawingApi extends js.Object {
  // override
  def drawChar(cell: GridCell, char: Char): Unit = js.native
  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit = js.native
  def applyBgColor(x: Int, y: Int, color: Color): Unit = js.native
  def applyColor(x: Int, y: Int, color: Color): Unit = js.native
  def gradientHorizontal(box: Box): Unit = js.native
}

@JSExportTopLevel("watr.utils.ProxyGraphPaper")
class ProxyGraphPaper(
  override val width: Int,
  override val height: Int,
  drawingApi: DrawingApi
) extends GraphPaper {

  // override
  def drawChar(cell: GridCell, char: Char): Unit = drawingApi.drawChar(cell, char)

  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit = drawingApi.drawBox(box, borderChars)
  def applyBgColor(x: Int, y: Int, color: Color): Unit = drawingApi.applyBgColor(x, y, color)
  def applyColor(x: Int, y: Int, color: Color): Unit = drawingApi.applyColor(x, y, color)
  def gradientHorizontal(box: Box): Unit = drawingApi.gradientHorizontal(box)

}
