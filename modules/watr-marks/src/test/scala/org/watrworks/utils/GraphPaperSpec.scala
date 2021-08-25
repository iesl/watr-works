package org.watrworks
package utils

import textboxing.{TextBoxing => TB}, TB._
import geometry.GraphPaper

class GraphPaperSpec extends WatrSpec {


  it should "draw ascii-based rectangles" in {

    val allBoxes = for {
      x <- 0 to 1
      y <- 0 to 1
      w <- 0 to 2
      h <- 0 to 2
    } yield {
      val graphPaper = new ConsoleGraphPaper(6, 6)
      val box = GraphPaper.Box(GraphPaper.GridCell(x, y), w, h)
      val border = BorderLineStyle.SingleWidth
      graphPaper.drawBox(box, border)
      vjoin(center1,
        graphPaper.asMonocolorString().box,
        s"[o:(${x}, ${y}), w:${w}, h:${h}]".box
      )
    }

    val rows = allBoxes.grouped(10).map{group =>
      hjoinWith(top, hspace(3),group)
    }

    val grid = vjoinWith(left, vspace(2), rows.toList)

    // println(grid.toString())
  }

}
