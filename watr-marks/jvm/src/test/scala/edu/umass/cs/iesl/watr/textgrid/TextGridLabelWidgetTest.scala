package edu.umass.cs.iesl.watr
package textgrid

import corpora._
import TypeTags._
import org.scalatest._

import watrmarks._
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


trait TextGridSpec extends FreeSpec with Matchers with TextGridBuilder

class TextGridLabelWidgetTests extends TextGridSpec {
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


  import TextGridLabelWidget._

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

    info(s"Create a TextGrid Labeling Widget")
    textGridToLabelingWidget(labelPerLineGrid)

  }
}

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

