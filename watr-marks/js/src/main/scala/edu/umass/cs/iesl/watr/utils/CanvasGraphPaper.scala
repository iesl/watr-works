package edu.umass.cs.iesl.watr
package utils

// import geometry._
// import utils.{RelativeDirection => Dir}
import scala.scalajs.js
import scala.scalajs.js.annotation._

import GraphPaper._

@js.native
trait DrawingApi extends js.Object {
  // override
  def drawChar(cell: GridCell, char: Char): Unit
  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit
  def applyBgColor(x: Int, y: Int, color: Color): Unit
  def applyColor(x: Int, y: Int, color: Color): Unit
  def gradientHorizontal(box: Box): Unit
}

trait CanvasApi extends DrawingApi {
  def drawChar(cell: GridCell, char: Char): Unit
}

@JSExportTopLevel("watr.utils.ProxyGraphPaper")
@JSExportAll
class ProxyGraphPaper(
  override val width: Int,
  override val height: Int,
  drawingApi: DrawingApi
) extends GraphPaper {


  // override
  def drawChar(cell: GridCell, char: Char): Unit
  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit
  def applyBgColor(x: Int, y: Int, color: Color): Unit
  def applyColor(x: Int, y: Int, color: Color): Unit
  def gradientHorizontal(gridbox: GraphPaper.Box): Unit

}
