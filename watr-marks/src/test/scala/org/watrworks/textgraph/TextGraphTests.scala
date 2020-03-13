package edu.umass.cs.iesl.watr
package textgraph

import TypeTags._
import textboxing.{TextBoxing => TB}, TB._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AnyFreeSpec
import watrmarks.Label
import utils.GraphPaper
import utils.ScalazTreeImplicits._

trait TextGraphSpec extends AnyFreeSpec with Matchers with TextGraphConstruction {

  import geometry.GeometryTestUtils

  val Authors = Label.auto
  val Author = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val LastName = Label.auto
  val Journal = Label.auto
  val RefMarker = Label.auto
  val RefNumber = Label.auto

  def infobox(heading: String, b: TB.Box): Unit = {
    info(heading)
    info("\n" + indent(4, b).toString() + "\n")
  }

  val stableId = DocumentID("SampleDocument")

  val sampleDoc = "abc\n  defg\nhijkl"

  def makeSample(): TextGraphJvm = {
    val textGraph = TextGraphJvm.create(stableId)
    val cells = stringToTextGraphCells(stableId, sampleDoc, PageNum(3))

    cells.foreach { row =>
      textGraph.appendRow(row)
    }

    textGraph
  }

  def makeBishopClarkSample(): TextGraphJvm = {
    val labelSpans = List(
      ((0, 2), RefMarker, None ),
      ((0, 1), RefNumber, Some(RefMarker) ),
      ((2, 5), Authors  , None ),
      // ((2, 2), Author   , Some(Authors) ),
      // ((2, 1), LastName , Some(Author) ),
      // ((3, 1), FirstName, Some(Author) ),
      // ((5, 2), Author   , Some(Authors) ),
      // ((7, 1), Journal  , None )
    )
    val rawText = {
      """|1
         |.
         |Bishop-Clark,
         |C
         |and
         |Wheeler,
         |D;
         |S.Eng. P-Hall
         |""".stripMargin
    }
    val textGraph = TextGraphJvm.create(stableId)
    val cells = stringToTextGraphCells(stableId, rawText, PageNum(0))

    cells.foreach { row =>
      textGraph.appendRow(row)
    }

    labelSpans.foreach { case ((begin, len), label, parent)=>
      val clipped = textGraph.clipToRows(begin, len)
      clipped.foreach { rows =>
        rows.rows
      }
      parent match {
        case Some(p) =>
          textGraph.labelSequence(label, parent, begin, len)
        case None =>
          textGraph.labelSequence(label, None, begin, len)
      }
    }


    textGraph
  }

  import _root_.io.circe
  import circe._
  val JsonPrettyPrinter = circe.Printer(
    dropNullValues = false,
    indent = " "*4,
    lbraceRight = "",
    rbraceLeft = "\n",
    lbracketRight = "",
    rbracketLeft = "",
    lrbracketsEmpty = "",
    arrayCommaRight = " ",
    objectCommaRight = "\n",
    colonLeft = " ",
    colonRight = " "
  )
}

class TextGraphTests extends TextGraphSpec {
  import TextGraphShape._



  "construct empty graph" in {
    val textGraph = makeSample()

    textGraph.labelSequence(Author, None, 3, 9)

    val graphPaper = TextGraphJvm.textGraphToGraphPaper(textGraph)
    println(graphPaper.asColorString())
  }

  "construct nested labeling " in {
    val bishopClarkGraph = makeBishopClarkSample()

    val graphPaper = TextGraphJvm.textGraphToGraphPaper(bishopClarkGraph)
    println(graphPaper.asColorString())
  }


  // "apply nested labels" in {
  //   val textGraph = makeSample()

  //   val l0 = textGraph.addLabel(0, 1, Label(s"L0"))
  //   println(s"Added (0): ${l0}")

  //   for {
  //     i <- 1 until 4
  //   } {
  //     val l1 = textGraph.addLabel(0, 1, Label(s"L${i}"), Label(s"L${i-1}"))
  //     println(s"Added (${i}): ${l1}")
  //   }

  //   val labelTrees = textGraph.findLabelTrees(textGraph.graphArea())
  //   labelTrees.foreach { tree =>
  //     println(s"Tree==== ")
  //     println(tree.drawBox)
  //   }

  // }




  // "convert region-based labels to sequence-based labels" in {
  //   val bishopClarkGraph = makeBishopClarkSample()

  //   val graphPaper = TextGraphJvm.textGraphToGraphPaper(bishopClarkGraph)
  //   infobox("bishopClarkGraph", graphPaper.asMonocolorString())

  //   val labelTrees = bishopClarkGraph.findLabelTrees(bishopClarkGraph.graphArea())
  //   labelTrees.foreach { tree =>
  //     infobox("==Tree==", tree.drawBox)
  //   }

  //   val matrixContent = bishopClarkGraph.getMatrixContent()
  //   matrixContent.map { matrixRows =>

  //     val rowIntervals = matrixRows.rows.map { matrixRow =>
  //       val rowNum = matrixRow.area.origin.y
  //       val rowBegin = matrixRow.area.origin.x
  //       val rowLen = matrixRow.area.width
  //       (rowBegin, rowLen)
  //     }

  //     labelTrees.map { tree =>
  //       tree.map { labelShape =>
  //         labelShape.graphBox()
  //       }
  //     }
  //   }
  // }

}
