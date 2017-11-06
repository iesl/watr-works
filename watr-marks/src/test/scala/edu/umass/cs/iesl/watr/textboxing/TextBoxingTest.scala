package edu.umass.cs.iesl.watr
package textboxing

import org.scalatest._


class TextBoxingTest extends FlatSpec with Matchers {
  // import TextBoxing._

  behavior of "textboxing"

  // it should "pad to center" in {
  //   val str = "abcd"
  //   val pos = 0 until str.length
  //   val widths = 0 until 10
  //   for {
  //     w <- widths
  //     p <- pos
  //   } {
  //     val qu = OneRow.dquote(
  //       "abcd".padCentered(w, p)
  //     )
  //     val res = s"w:${w}, ctr:${p}:  $qu"
  //     println(res)
  //   }

  // }

  it should "create grids" in {

    // val grid = Grid.widthAligned(
    //   (10, AlignRight),
    //   (10, AlignCenter),
    //   (10, AlignLeft)
    // )
    // val gFinal = grid
    //   .addRow("1%>","-!-", "<2%" )
    //   .addRow("1%>","-!-", "<2%" )
    //   .addRow("1%>","-!-", "<2%" )

    // gFinal.toBox().toString() shouldBe {
    //   """|          1%>   -!-    <2%
    //      |          1%>   -!-    <2%
    //      |          1%>   -!-    <2%
    //      |""".stripMargin
    // }


  }

}
