package org.watrworks
package utils

import geometry._
import scala.collection.mutable
import utils.{M3x3Position => M3}

import GraphPaper._

case class Color(
  red: Int,
  green: Int,
  blue: Int
)

abstract class DrawableGraphPaper {

  def drawChar(cell: GridCell, char: Char): Unit

  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit

  def applyBgColor(box: Box, color: Color): Unit
  def applyFgColor(box: Box, color: Color): Unit
  def gradientHorizontal(gridbox: Box): Unit
  def cellDimensions(): CellDimensions

  def drawString(x: Int, y: Int, str: String): Unit = {
    str.zipWithIndex.foreach { case (ch, i) =>
      drawChar(GraphPaper.GridCell(x + i, y), ch)
    }
  }

  def fillChar(box: Box, fill: Char): Unit = {
    box.getCells().foreach { cell => drawChar(cell, fill) }
  }

  def border(gridbox: Box, color: Color): Unit = {
    borderLeftRight(gridbox, color)
    borderTopBottom(gridbox, color)
  }

  def borderLeftRight(gridbox: Box, color: Color): Unit = {
    for { y <- gridbox.origin.y until (gridbox.origin.y + gridbox.spanDown) } {
      val x1 = gridbox.origin.x
      val x2 = gridbox.origin.x + gridbox.spanRight - 1
      applyBgColor(boxAt(x1, y), color)
      applyBgColor(boxAt(x2, y), color)
    }
  }

  def borderTopBottom(gridbox: Box, color: Color): Unit = {
    for { x <- gridbox.origin.x until (gridbox.origin.x + gridbox.spanRight) } {
      val y1 = gridbox.origin.y
      val y2 = gridbox.origin.y + gridbox.spanDown - 1
      applyBgColor(boxAt(x, y1), color)
      applyBgColor(boxAt(x, y2), color)
    }
  }

  def shadeBackground(gridbox: Box, color: Color): Unit = {
    for {
      y <- gridbox.origin.y until (gridbox.origin.y + gridbox.spanDown)
      x <- gridbox.origin.x until (gridbox.origin.x + gridbox.spanRight)
    } {
      applyBgColor(boxAt(x, y), color)
    }
  }
}

case class BorderChars(str: String) {
  val chars = str.toCharArray()
  def charFor(dir: M3) = dir match {
    case M3.Top         => chars(0)
    case M3.Bottom      => chars(1)
    case M3.Right       => chars(2)
    case M3.Left        => chars(3)
    case M3.TopLeft     => chars(4)
    case M3.TopRight    => chars(5)
    case M3.BottomLeft  => chars(6)
    case M3.BottomRight => chars(7)
    case M3.Center      => ' '
  }

  def char1For(dir: M3) = dir match {
    case M3.Top  => chars(0)
    case M3.Left => chars(2)
    case _        => sys.error(s"no char1 for dir ${dir}")
  }

  def endcapFor(dir: M3) = dir match {
    case M3.Center => chars(8)
    case M3.Top    => chars(9 + 0)
    case M3.Bottom => chars(9 + 1)
    case M3.Left   => chars(9 + 2)
    case M3.Right  => chars(9 + 3)
    case _          => sys.error(s"no endcaps for dir ${dir}")
  }
}

object BorderLineStyle {
  //                             0123456789
  val DoubleWidth = BorderChars("══║║╔╗╚╝◻╦╩╠╣")
  val SingleWidth = BorderChars("──││┌┐└┘◻┬┴├┤")
  val Bold        = BorderChars("━━┃┃┏┓┗┛◻┳┻┣┫")
}

class ConsoleGraphPaper(
  val initWidth: Int,
  val initHeight: Int,
  useColor: Boolean = true
) extends DrawableGraphPaper {

  var width: Int  = initWidth
  var height: Int = initHeight

  private def resizeToFit(cell: GridCell): Unit = {
    val vpad = cell.y - height + 1
    val hpad = cell.x - width + 1
    if (vpad > 0) {
      charBuffer.appendAll(
        Array.fill(vpad)(mutable.ArrayBuffer.fill(width)('░'))
      )
      charMods.appendAll(
        Array.fill(vpad)(mutable.ArrayBuffer.fill(width)(fansi.Attrs.Empty))
      )
      colorMods.appendAll(
        Array.fill(vpad)(mutable.ArrayBuffer.fill(width)(fansi.Attrs.Empty))
      )
      height = height + vpad
    }
    if (hpad > 0) {
      charBuffer.foreach { lineBuffer => lineBuffer.appendAll("░" * hpad) }
      charMods.foreach { lineBuffer =>
        lineBuffer.appendAll(Array.fill(hpad)(fansi.Attrs.Empty))
      }
      colorMods.foreach { lineBuffer =>
        lineBuffer.appendAll(Array.fill(hpad)(fansi.Attrs.Empty))
      }
      width = width + hpad
    }

  }

  val charBuffer: mutable.ArrayBuffer[
    mutable.ArrayBuffer[Char]
  ] = mutable.ArrayBuffer.tabulate(height, width) { case _ =>
    '░'
  }

  def cellDimensions(): CellDimensions = {
    CellDimensions(1, 1)
  }

  val colorMods: mutable.ArrayBuffer[
    mutable.ArrayBuffer[fansi.Attrs]
  ] = mutable.ArrayBuffer.tabulate(height, width) { case _ =>
    fansi.Attrs.Empty
  }

  val charMods: mutable.ArrayBuffer[
    mutable.ArrayBuffer[fansi.Attrs]
  ] = mutable.ArrayBuffer.tabulate(height, width) { case _ =>
    fansi.Attrs.Empty
  }

  def drawChar(cell: GridCell, char: Char): Unit = {
    resizeToFit(cell)
    charBuffer(cell.y)(cell.x) = char
  }

  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit = {

    if (box.spanRight == 0 && box.spanDown == 0) {
      box.getCells(M3.Left).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(M3.Center))
      }
    } else if (box.spanRight == 0) {
      box.getCells(M3.Left).foreach { cell =>
        drawChar(cell, borderChars.char1For(M3.Left))
      }
      box.getCells(M3.Top).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(M3.Top))
      }
      box.getCells(M3.Bottom).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(M3.Bottom))
      }
    } else if (box.spanDown == 0) {
      box.getCells(M3.Top).foreach { cell =>
        drawChar(cell, borderChars.char1For(M3.Top))
      }
      box.getCells(M3.Left).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(M3.Left))
      }
      box.getCells(M3.Right).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(M3.Right))
      }
    } else {
      List(M3.Top, M3.Bottom, M3.Left, M3.Right).foreach { d =>
        box.getCells(d).foreach(cell => drawChar(cell, borderChars.charFor(d)))
      }

      List(M3.TopLeft, M3.TopRight, M3.BottomLeft, M3.BottomRight).foreach { d =>
        box
          .getCells(d)
          .foreach(cell => drawChar(cell, borderChars.charFor(d)))
      }

    }
  }

  private def modColor(cell: GridCell, a: fansi.Attrs): Unit = {
    val y  = cell.y
    val x  = cell.x
    val qq = colorMods(y)(x)
    colorMods(y)(x) = qq ++ a
  }
  private def modFormat(cell: GridCell, a: fansi.Attrs): Unit = {
    val y  = cell.y
    val x  = cell.x
    val qq = charMods(y)(x)
    charMods(y)(x) = qq ++ a
  }

  def underline(box: Box): Unit = {
    box.getCells().foreach { cell => modFormat(cell, fansi.Underlined.On) }
  }

  def applyBgColor(box: Box, color: Color): Unit = {
    val rgb        = color
    val fansiColor = fansi.Back.True(rgb.red, rgb.green, rgb.blue)
    box.getCells().foreach { cell => modColor(cell, fansiColor) }
  }

  def applyFgColor(box: Box, color: Color): Unit = {
    val rgb        = color
    val fansiColor = fansi.Color.True(rgb.red, rgb.green, rgb.blue)
    box.getCells().foreach { cell => modColor(cell, fansiColor) }
  }

  def gradientHorizontal(gridbox: Box): Unit = {
    var r = 20
    val g = 20
    var b = 20
    // val xstep = 256 / gridbox.spanRight
    // val ystep = 256 / gridbox.spanDown
    for {
      y <- gridbox.origin.y until (gridbox.origin.y + gridbox.spanDown)
      _ <- List[Unit]({
             r = (r + 3) % 256
             b = 0
           })
      x <- gridbox.origin.x until (gridbox.origin.x + gridbox.spanRight)
    } {
      // g = (g + 3) % 256
      b = (b + 2) % 256
      val qq = colorMods(y)(x)
      colorMods(y)(x) = qq ++ fansi.Back.True(r, g, b)
    }
  }

  def asColorString(): String = {
    if (useColor) {
      val rws = charBuffer.zipWithIndex.map { case (charRow, rowNum) =>
        val chs = charRow.zipWithIndex.map { case (char, colNum) =>
          val clr = colorMods(rowNum)(colNum)
          val fmt = charMods(rowNum)(colNum)
          clr(fmt(char.toString()))
        }
        chs.mkString
      }
      rws.mkString("\n")
    } else {
      asMonocolorString()
    }
  }

  def asMonocolorString(): String = {
    val rws = charBuffer.zipWithIndex.map { case (charRow, rowNum) =>
      val chs = charRow.zipWithIndex.map { case (char, colNum) =>
        val fmt = charMods(rowNum)(colNum)
        fmt(char.toString())
      }
      chs.mkString
    }
    rws.mkString("\n")
  }

}
