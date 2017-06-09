package edu.umass.cs.iesl.watr
package utils

import geometry._
import scala.collection.mutable

class GraphPaper(
  width: Int, height: Int, useColor: Boolean=true
) {

  lazy val box = GraphPaper.Box(0, 0, width, height)
  lazy val bbox = GraphPaper.box2ltb(box)

  val gridBuffer = mutable.ArrayBuffer
    .tabulate(height, width){ case (y, x) =>
      // val fcolor = fansi.Color.Blue("╌")
      fansi.Color.Blue(" ")
    }


  def fillFg(fill: Char, gridbox: GraphPaper.Box): Unit = {
    for {
      y <- gridbox.top until (gridbox.top+gridbox.height)
      x <- gridbox.left until (gridbox.left+gridbox.width)
    } {
      gridBuffer(y)(x) = fill.toString()
    }
  }


  def applyBgColor(x: Int, y: Int, color: Color): Unit = {
    val xy = gridBuffer(y)(x)
    val rgb = color.toRGB
    val fansiColor = fansi.Back.True(rgb.red, rgb.green, rgb.blue)
    gridBuffer(y)(x) = fansiColor(xy)
  }

  def applyColor(x: Int, y: Int, color: Color): Unit = {
    val xy = gridBuffer(y)(x)
    val rgb = color.toRGB
    val fansiColor = fansi.Color.True(rgb.red, rgb.green, rgb.blue)
    gridBuffer(y)(x) = fansiColor(xy)
  }

  def border(gridbox: GraphPaper.Box, color: Color): Unit = {
    borderLeftRight(gridbox, color)
    borderTopBottom(gridbox, color)
  }

  def borderLeftRight(gridbox: GraphPaper.Box, color: Color): Unit = {
    for { y <- gridbox.top until (gridbox.top+gridbox.height) } {
      val x1 = gridbox.left
      val x2 = gridbox.left+gridbox.width-1
      applyBgColor(x1, y, color)
      applyBgColor(x2, y, color)
    }
  }

  def borderTopBottom(gridbox: GraphPaper.Box, color: Color): Unit = {
    for { x <- gridbox.left until (gridbox.left+gridbox.width) } {
      val y1 = gridbox.top
      val y2 = gridbox.top+gridbox.height-1
      applyBgColor(x, y1, color)
      applyBgColor(x, y2, color)
    }
  }

  def gradientHorizontal(gridbox: GraphPaper.Box): Unit = {
    var r = 20
    val g = 20
    var b = 20
    // val xstep = 256 / gridbox.width
    // val ystep = 256 / gridbox.height
    for {
      y <- gridbox.top until (gridbox.top+gridbox.height)
      _ <- List[Unit]({
        r = (r + 3) % 256
        b=0
      })
      x <- gridbox.left until (gridbox.left+gridbox.width)
    } {
      // g = (g + 3) % 256
      b = (b + 2) % 256
      val qq = gridBuffer(y)(x)
      // applyColor(x, y, color)
      gridBuffer(y)(x) = fansi.Back.True(r,g, b)(qq)
    }
  }

  def shadeBackground(gridbox: GraphPaper.Box, color: Color): Unit = {
    for {
      y <- gridbox.top until (gridbox.top+gridbox.height)
      x <- gridbox.left until (gridbox.left+gridbox.width)
    } {
      applyBgColor(x, y, color)
    }
  }

  def asString(): String = {
    gridBuffer.map(_.mkString).mkString("\n")
  }
  def asMonocolorString(): String = {
    gridBuffer
      .map(_.map(_.plainText).mkString)
      .mkString("\n")
  }

}


object GraphPaper {
  def box2ltb(box: Box): LTBounds = {
    val Box(l, t, w, h) = box
    LTBounds.Ints(l, t, w, h)
  }

  def ltb2box(bbox: LTBounds): GraphPaper.Box = {
    val LTBounds.Ints(l, t, w, h) = bbox
    GraphPaper.Box(l, t, w, h)
  }

  case class Box(
    left: Int, top: Int, width: Int, height: Int
  )

  def create(w: Int, h: Int) = {
    new GraphPaper(w, h)
  }

}
