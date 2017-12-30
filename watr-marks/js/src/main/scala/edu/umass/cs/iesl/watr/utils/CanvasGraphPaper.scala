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
  def applyBgColor(box: Box, color: Color): Unit = js.native
  def applyColor(box: Box, color: Color): Unit = js.native
  def gradientHorizontal(box: Box): Unit = js.native
  def cellWidth: Int = js.native
  def cellHeight: Int = js.native
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
  def applyBgColor(box: Box, color: Color): Unit = drawingApi.applyBgColor(box, color)
  def applyColor(box: Box, color: Color): Unit = drawingApi.applyColor(box, color)
  def gradientHorizontal(box: Box): Unit = drawingApi.gradientHorizontal(box)


  @JSExport
  def cellDimensions(): CellDimensions = {
    CellDimensions(
      drawingApi.cellWidth,
      drawingApi.cellHeight
    )
  }

}
