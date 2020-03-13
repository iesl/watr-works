package org.watrworks
package utils

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import textboxing.{TextBoxing => TB}, TB._

class GraphPaperSpec extends AnyFlatSpec with Matchers {

  it should "maintain height/width dimensions when rendered as string" in {
    val graphPaper = new AsciiGraphPaper(2, 2)
    // val (x, y, w, h) = (0, 0, 0, 0)
    // val box = GraphPaper.Box(GraphPaper.GridCell(x, y), w, h)
    // val border = GraphPaper.BorderLineStyle.SingleWidth
    val asString = graphPaper.asMonocolorString()
    val lens = asString.split("\n").map(_.length()).toList// .mkString(", ")
    println("lens: " + lens)
    graphPaper.asMonocolorString().box
  }

  it should "draw non-empty rectangles" in {

    val allBoxes = for {
      x <- 0 to 1
      y <- 0 to 1
      w <- 0 to 2
      h <- 0 to 2
    } yield {
      val graphPaper = new AsciiGraphPaper(6, 6)
      val box = GraphPaper.Box(GraphPaper.GridCell(x, y), w, h)
      val border = GraphPaper.BorderLineStyle.SingleWidth
      graphPaper.drawBox(box, border)
      // val asString = graphPaper.asMonocolorString()
      // val lens = asString.split("\n").map(_.length()).mkString(", ")
      // println("lens: " + lens)
      vjoin(center1,
        graphPaper.asMonocolorString().box,
        s"[o:(${x}, ${y}), w:${w}, h:${h}]".box
      )
    }

    val rows = allBoxes.grouped(10).map{group =>
      hjoinWith(top, hspace(3),group)
    }


    val grid = vjoinWith(left, vspace(2), rows.toList)

    println(grid.toString())
  }

  it should "indicate empty rectangle drawing" in {
  }
}
