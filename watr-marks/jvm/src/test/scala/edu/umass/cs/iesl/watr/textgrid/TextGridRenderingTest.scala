package edu.umass.cs.iesl.watr
package textgrid


import corpora._
import TypeTags._
// import _root_.io.circe
// import circe._
// import circe.syntax._
// import circe.literal._
// import textboxing.{TextBoxing => TB}, TB._
import org.scalatest._

import watrmarks._
// import utils.SlicingAndDicing._
import utils.ScalazTreeImplicits._
// import scalaz.{@@ => _, _} , Scalaz._

case class LineRenderInfo(
  text: String,
  indent: Int,
  isHoverable: Boolean,
  canSplit: Boolean,
  canJoin: Boolean,
  canLabel: Boolean
)

sealed trait TextGridLayout

object TextGridLayout {
  case class Node(
    sub: Seq[TextGridLayout]
  ) extends TextGridLayout

  case class Leaf(
    cells: Seq[(TextGrid.GridCell, Int)]
  ) extends TextGridLayout

}


trait TextGridSpec extends FreeSpec with Matchers with TextGridBuilder

class TextGridRenderingTests extends TextGridSpec {
  override val docStore: DocumentZoningApi = new MemDocZoningApi
  val docs = List(
    List(
      "exit-\ning\n",
      "cellar\ndoor\n",
      "close-\nup\n"
    )
  )

  for { (doc, i) <- docs.zipWithIndex } {
    addDocument(DocumentID(s"doc#${i}"), doc)
  }


  val Authors = Label.auto
  val Author = Label.auto
  val LastName = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val Journal = Label.auto
  val RefMarker = Label.auto
  val RefNumber = Label.auto


  val inlineSpec = {
    // j: Journal
    // s: Authors
    // n: Name
    // f/m/l: First/Middle/LastName

  }
  val _ = {

    """|
       |>R    |1.
       |>snL  |Bishop-Clark
       |>║║   |,
       |>║╨F  |C.
       |>║    |and
       |>║nL  |Wheeler
       |>║║   |,
       |>║╨F  |D.
       |>║    |and
       |>╨N   |Boehm, B.W.
       |>     |;
       |>AJ   |Software Engineering Economics. Prentice-Hall
       |"""
  }



  val indentedRendering = {
    """|
       |1.
       |Bishop-Clark,  C. and Wheeler, D. and Boehm, B.W.
       |    Bishop-Clark,  C.
       |        Bishop-Clark
       |        ,
       |        C.
       |    and
       |    Wheeler, D.
       |        Wheeler
       |        ,
       |        D.
       |    and
       |    Boehm, B.W.
       |;
       |Software Engineering Economics. Prentice-Hall
       |""".stripMargin
  }



  val labelSpans = List(
    ((0, 1),   RefMarker),
    ((0, 0),   RefNumber),
    ((3, 33),  Authors),
    ((3, 17),  Author),
    ((3, 14),  LastName),
    ((17, 17), FirstName),
    ((24, 30), Author),
    ((36, 48), Journal)
  )

  val unlabeledText = {
    //"0         1         2         3         4         5
    // 012345678901234567890123456789012345678901234567899 """
    """1. Bishop-Clark, C  and Wheeler, D; S.Eng. P-Hall"""

  }


  import TextGridRendering._

  val stableId = DocumentID("docXX")

  "Behavior of Textgrid Widget Creation" - {
    info("Starting with unlabeled TextGrid")

    val textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)

    info(textGrid.toText())

    info("... With labels applied")

    val labeledRow = addLabelsToGridRow(textGrid.rows.head, labelSpans)

    info("\n"+ labeledRow.showRow().toString()+ "\n")

    info("... and normalized to one leaf label per line")

    val textGrid2 = TextGrid.fromRows(stableId, Seq(labeledRow))
    val labelPerLineGrid = textGrid2.splitOneLeafLabelPerLine()

    info(s"\n${labelPerLineGrid.toText()}\n\n-------------------")

    info(s"Create a tree structure out of the BIO labels")

    val labelTree = textGridToLabelTree(labelPerLineGrid)
    info(labelTree.drawBox.toString())

    // info(s"Create a TextGrid Labeling Widget")
    // textGridToLabelingWidget(labelPerLineGrid)

  }
}


// val layout = textgridToLayout(textGrid2)
// println(layout)

// val renderedWidgetBlock = splitGrid.rows
//   .zipWithIndex.flatMap { case (row, rowNum) =>
//     val rowText = row.toText()
//     val rowPins = row.pins.toList
//     val textIndent = rowPins.filterNot(_.isUnit).length
//     val headerPins = rowPins.filter(_.isBegin)
//     println(s"R-------\n${row.showRow()}\n\n")

  //     val thisRow = LineRenderInfo(
  //       rowText,
  //       textIndent,
  //       isHoverable = true,
  //       canSplit = false,
  //       canJoin = false,
  //       canLabel = false
  //     )

  //     val headerLines = headerPins.zipWithIndex.map{ case (pin, pinNum) =>
  //       val hdr = textGrid.rows.drop(rowNum)
  //         .takeWhile{ r => r.hasPin(pin.label.I) }
  //         .take(1)

  //       val text = hdr.map(_.toText()).mkString(" ")

  //       LineRenderInfo(
  //         text,
  //         pinNum,
  //         isHoverable = false,
  //         canSplit = false,
  //         canJoin = false,
  //         canLabel = false
  //       )
  //     }

  //     headerLines :+ thisRow
  //   }

  // val lines = renderedWidgetBlock.map{ renderInfo =>
  //   indent(renderInfo.indent, renderInfo.text)
  // }

  // val block = vcat(left, lines)

  // println(block.toString)
  // def makeRuler(str: String): String = {
  //   val height = str.length.toString.length
//   val topRulerList = (height to 2 by -1).map(level => {
//     (bb.startIndex until next).map(i => {
//       if (i == bb.startIndex || (i % 10) == 0){
//         val divisor = Math.pow(10,level).toInt
//         val digit = (i % divisor)/(divisor/10)
//         if (digit == 0 && level == height) " " else digit
//       } else " "
//     }).mkString("")+"|"
//   })
//   val bottomRuler = "| |" + (bb.startIndex until next).map(_ % 10).mkString("") + "|"
//   val ruler = (topRulerList :+ bottomRuler).mkString("\n")
//   "\n" + bb.annotationMap.values.toList.reverse.distinct.map(renderAnnotation(_, (next - bb.startIndex))).mkString("\n") + "\n" + ruler + "\n "
// }

// val jsonLabelSpec = {
//   Json.obj(
//     "label" := "Affiliations",
//     "children" := Seq(
//       Json.obj(
//         "label" := "Authors",
//         "children" := Seq(
//           Json.obj(
//             "label" := "Author"
//           )
//         )
//       )
//     )
//   )
// }

  // def textgridToLayout(textGrid: TextGrid): TextGridLayout = {
  //   cellsToLayout(textGrid.indexedCells, 0)
  // }

  // def cellsToLayout(cells: Seq[(TextGrid.GridCell, Int)], level: Int): TextGridLayout = {
  //   val groups = cells.groupByPairs{ case ((a, ar), (b, br)) => {

  //     val alen = a.pins.length - level
  //     val blen = b.pins.length - level

  //     val emptyPins = alen==0 && blen==0
  //     val samePinCount = alen == blen

  //     emptyPins || { samePinCount && {
  //       val pin1 = a.pins.head
  //       val pin2 = b.pins.head
  //       val isBIL = pin1.isBegin && (pin2.isInside || pin2.isLast)
  //       val isIIL = pin1.isInside && (pin2.isInside || pin2.isLast)
  //       val sameLabel = pin1.label == pin2.label

  //       sameLabel && (isBIL || isIIL)
  //     }}
  //   }}

  //   val subNodes = groups.map{ group =>
  //     val glen = group.head._1.pins.length - level
  //     if (glen > 0) {
  //       cellsToLayout(group, level+1)
  //     } else {
  //       TextGridLayout.Leaf(group)
  //     }
  //   }

  //   TextGridLayout.Node(subNodes)

  // }
