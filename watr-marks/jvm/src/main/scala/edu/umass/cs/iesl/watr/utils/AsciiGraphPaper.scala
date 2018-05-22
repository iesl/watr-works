package edu.umass.cs.iesl.watr
package utils

import scala.collection.mutable
import utils.{RelativeDirection => Dir}

class AsciiGraphPaper(
  override val width: Int,
  override val height: Int,
  useColor: Boolean=true
) extends GraphPaper {

  import GraphPaper._

  val charBuffer: mutable.ArrayBuffer[
    mutable.ArrayBuffer[Char]
  ] = mutable.ArrayBuffer.tabulate(height, width){ case (y, x) =>
      '░'
  }

  def cellDimensions(): CellDimensions= {
    CellDimensions(1, 1)
  }

  val charMods: mutable.ArrayBuffer[
    mutable.ArrayBuffer[fansi.Attrs]
  ] = mutable.ArrayBuffer.tabulate(height, width){ case (y, x) =>
      fansi.Attrs.Empty
  }

  def drawChar(cell: GridCell, char: Char): Unit = {
    charBuffer(cell.y)(cell.x) = char
  }

  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit = {

    if (box.spanRight==0 && box.spanDown==0) {
      box.getCells(Dir.Left).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Center))
      }
    } else if (box.spanRight==0) {
      box.getCells(Dir.Left).foreach { cell =>
        drawChar(cell, borderChars.char1For(Dir.Left))
      }
      box.getCells(Dir.Top).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Top))
      }
      box.getCells(Dir.Bottom).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Bottom))
      }
    } else if (box.spanDown==0) {
      box.getCells(Dir.Top).foreach { cell =>
        drawChar(cell, borderChars.char1For(Dir.Top))
      }
      box.getCells(Dir.Left).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Left))
      }
      box.getCells(Dir.Right).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Right))
      }
    } else {
      List(Dir.Top, Dir.Bottom, Dir.Left, Dir.Right)
        .foreach{ d =>
          box.getCells(d).foreach(cell => drawChar(cell, borderChars.charFor(d)))
        }

      List(Dir.TopLeft, Dir.TopRight, Dir.BottomLeft, Dir.BottomRight)
        .foreach{ d =>
          box.getCells(d).foreach(cell => drawChar(cell, borderChars.charFor(d)))
        }

    }
  }

  def applyBgColor(box: Box, color: Color): Unit = {
    val x = box.origin.x
    val y = box.origin.y
    val xy = charMods(y)(x)
    val rgb = color.toRGB
    val fansiColor = fansi.Back.True(rgb.red, rgb.green, rgb.blue)
    charMods(y)(x) = xy ++ fansiColor
  }

  def applyColor(box: Box, color: Color): Unit = {
    val x = box.origin.x
    val y = box.origin.y
    val xy = charMods(y)(x)
    val rgb = color.toRGB
    val fansiColor = fansi.Color.True(rgb.red, rgb.green, rgb.blue)
    charMods(y)(x) = xy ++ fansiColor
  }

  def gradientHorizontal(gridbox: Box): Unit = {
    var r = 20
    val g = 20
    var b = 20
    // val xstep = 256 / gridbox.spanRight
    // val ystep = 256 / gridbox.spanDown
    for {
      y <- gridbox.origin.y until (gridbox.origin.y+gridbox.spanDown)
      _ <- List[Unit]({
        r = (r + 3) % 256
        b=0
      })
      x <- gridbox.origin.x until (gridbox.origin.x+gridbox.spanRight)
    } {
      // g = (g + 3) % 256
      b = (b + 2) % 256
      val qq = charMods(y)(x)
      charMods(y)(x) = qq ++ fansi.Back.True(r,g,b)
    }
  }

  def asColorString(): String = {
    if (useColor) {
      val rws = charBuffer.zipWithIndex.map{ case (charRow, rowNum) =>
        val chs = charRow.zipWithIndex.map{ case (char, colNum) =>
          val mod = charMods(rowNum)(colNum)
          mod(char.toString())
        }
        chs.mkString
      }
      rws.mkString("\n")
    } else {
      asMonocolorString()
    }
  }

  def asMonocolorString(): String = {
    charBuffer
      .map(_.mkString)
      .mkString("\n")
  }

}


object AsciiGraphPaper {

  case class BorderChars(str: String) {
    val chars = str.toCharArray()
    def charFor(dir: Dir) = dir match {
      case Dir.Top         => chars(0)
      case Dir.Bottom      => chars(1)
      case Dir.Right       => chars(2)
      case Dir.Left        => chars(3)
      case Dir.TopLeft     => chars(4)
      case Dir.TopRight    => chars(5)
      case Dir.BottomLeft  => chars(6)
      case Dir.BottomRight => chars(7)
      case Dir.Center      => ' '
    }

    def char1For(dir: Dir) = dir match {
      case Dir.Top         => chars(0)
      case Dir.Left        => chars(2)
      case _ => sys.error(s"no char1 for dir ${dir}")
    }

    def endcapFor(dir: Dir) = dir match {
      case Dir.Center      => chars(8)
      case Dir.Top         => chars(9+0)
      case Dir.Bottom      => chars(9+1)
      case Dir.Left        => chars(9+2)
      case Dir.Right       => chars(9+3)
      case _ => sys.error(s"no endcaps for dir ${dir}")
    }
  }

  object BorderLineStyle {
    //                             0123456789
    val DoubleWidth = BorderChars("══║║╔╗╚╝◻╦╩╠╣")
    val SingleWidth = BorderChars("──││┌┐└┘◻┬┴├┤")
    val Bold        = BorderChars("━━┃┃┏┓┗┛◻┳┻┣┫")
  }

}
