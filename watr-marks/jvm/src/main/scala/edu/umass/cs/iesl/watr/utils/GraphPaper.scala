package edu.umass.cs.iesl.watr
package utils

import geometry._
import scala.collection.mutable

class GraphPaper(
  width: Int, height: Int
) {

  val gridBuffer = mutable.ArrayBuffer
    .tabulate(height, width){ case (y, x) =>
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

  def borderLeftRight(gridbox: GraphPaper.Box): Unit = {
    for { y <- gridbox.top until (gridbox.top+gridbox.height) } {
      val x1 = gridbox.left
      val x2 = gridbox.left+gridbox.width-1
      val cur0 = gridBuffer(y)(x1)
      val cur1 = gridBuffer(y)(x2)
      val col0 = fansi.Color.Red(cur0)
      val col1 = fansi.Color.Red(cur1)
      gridBuffer(y)(x1) = col0
      gridBuffer(y)(x2) = col1
    }
  }

  def borderTopBottom(gridbox: GraphPaper.Box): Unit = {
    for { x <- gridbox.left until (gridbox.left+gridbox.width) } {
      val y1 = gridbox.top
      val y2 = gridbox.top+gridbox.height-1
      // println(s"  ($x, $y1/$y2) := row")
      val cur0 = gridBuffer(y1)(x)
      val cur1 = gridBuffer(y2)(x)
      val col0 = fansi.Color.Magenta(cur0)
      val col1 = fansi.Color.Magenta(cur1)
      gridBuffer(y1)(x) = col0
      gridBuffer(y2)(x) = col1
    }
  }

  def gradientHorizontal(gridbox: GraphPaper.Box): Unit = {
    var r = 20
    var g = 20
    var b = 20
    val xstep = 256 / gridbox.width
    val ystep = 256 / gridbox.height
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
      gridBuffer(y)(x) = fansi.Back.True(r,g, b)(qq)
    }
  }

  def shadeBackground(gridbox: GraphPaper.Box): Unit = {
    for {
      y <- gridbox.top until (gridbox.top+gridbox.height)
      x <- gridbox.left until (gridbox.left+gridbox.width)
    } {
      val qq = gridBuffer(y)(x)
      gridBuffer(y)(x) = fansi.Back.True(10, 20, 100)(qq)
    }
  }

  def asString(): String = {
    gridBuffer.map(_.mkString).mkString("\n")
  }

}


object GraphPaper {
  def ltb2box(bbox: LTBounds): GraphPaper.Box = {
    val LTBounds(l, t, w, h) = bbox
    GraphPaper.Box(
      l.intValue(), t.intValue(),
      w.intValue(), h.intValue()
    )
  }

  case class Box(
    left: Int, top: Int, width: Int, height: Int
  )

  def create(w: Int, h: Int) = {
    new GraphPaper(w, h)
  }

}
