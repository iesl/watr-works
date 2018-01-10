package edu.umass.cs.iesl.watr
package textgrid


import corpora._
import TypeTags._
import _root_.io.circe
// import circe._
// import circe.syntax._
// import circe.literal._
// import geometry._
import textboxing.{TextBoxing => TB}, TB._


class TextGridTests extends TextGridSpec {
  val jsonPrinter = circe.Printer(
    preserveOrder = true,
    dropNullValues = false,
    indent = "    ",
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


  override val docStore: DocumentZoningApi = new MemDocZoningApi

  val docs = List(
    List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "rst\nuvw\nxyz"
    ),
    List(
      "exit-\ning\n",
      "cellar\ndoor\n",
      "close-\nup\n"
    )
  )

  for { (doc, i) <- docs.zipWithIndex } {
    addDocument(DocumentID(s"doc#${i}"), doc)
  }

  // info("behavior of TextGrid Rows")


  // "it should join two rows into one, with optional space or dehyphenation" in {}

  // it should "support alphabet soup regions" in {}
  // it should "clip single row to target region(s)" in {}
  // it should "clip multiple rows to target region(s)" in {}

  // behavior of "TextGrid Cursors"

  // it should "slurp/barf" in {}

  import TextGridLabelWidget._
  def infobox(heading: String, b: TB.Box): Unit = {
    info(heading)
    info("\n" + indent(4, b).toString() + "\n")
  }

  "it should find the span of cells that have a particular label" - {
    val unlabeledText = "11\n22\n33"
    val textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    textGrid.labelRow(1, Author)

    textGrid.findLabelExtents(1, 1, Author).foreach { extent =>
      val res = extent.map(c => (c._2, c._3))
      res shouldEqual Seq((1, 0), (1, 1))
    }

    textGrid.split(1, 1).foreach { splitGrid =>
      splitGrid.findLabelExtents(1, 0, Author).foreach {extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0), (2, 0))
      }
      splitGrid.findLabelExtents(2, 0, Author).foreach {extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0), (2, 0))
      }
    }
  }


  "it should find the span of cells that have identical labeling (including perhaps unlabeled)" in {
    val unlabeledText = "11\n22\n33"
    val textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    textGrid.labelRow(1, Author)
    textGrid.findIdenticallyLabeledSiblings(0, 1).foreach { extent =>
      val res = extent.map(c => (c._2, c._3))
      res shouldEqual Seq((0, 0), (0, 1))
    }
    textGrid.findIdenticallyLabeledSiblings(1, 0).foreach { extent =>
      val res = extent.map(c => (c._2, c._3))
      res shouldEqual Seq((1, 0), (1, 1))
    }

    textGrid.split(1, 1).foreach { splitGrid =>

      splitGrid.findIdenticallyLabeledSiblings(1, 0).foreach { extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0), (2, 0))
      }
      splitGrid.findIdenticallyLabeledSiblings(2, 0).foreach { extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0), (2, 0))
      }

      splitGrid.labelRow(2, FirstName)

      splitGrid.findIdenticallyLabeledSiblings(1, 0).foreach { extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0))
      }
      splitGrid.findIdenticallyLabeledSiblings(2, 0).foreach { extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((2, 0))
      }
    }
  }

  "it should find row reorderings" - {

  }



  "Behavior of labeled TextGrid serialization" - {
    val labelSpans = List(
      ((0, 1),   RefMarker),
      ((0, 0),   RefNumber),
      ((3, 33),  Authors),
      ((3, 17),  Author),
      ((3, 14),  LastName),
      ((17, 17), FirstName),
      ((24, 33), Author),
      ((36, 48), Journal)
    )

    val unlabeledText = {
      //"0         1         2         3         4         5
      // 012345678901234567890123456789012345678901234567899 """
      """1. Bishop-Clark, C  and Wheeler, D; S.Eng. P-Hall"""

    }
    var textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    val labeledRow = addLabelsToGridRow(textGrid.rows.head, labelSpans)
    textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()
    textGrid = textGrid.split(9, 7).get

    val asJson = textGrid.toJson // .pretty(jsonPrinter)
    val roundTripGrid = TextGrid.fromJson(asJson)
    val rtJson = roundTripGrid.toJson()

    // val indentedBlock = textGridToIndentedBox(roundTripGrid)
    // val labelTree = textGridToLabelTree(roundTripGrid)
    // val expMarginals = labelTreeToMarginals(labelTree, compactMarginals=false)
    // val emarginBlock = marginalGlossToTextBlock(expMarginals)
    // val expBlock = emarginBlock + indentedBlock
    // println("post: ")
    // println(expBlock.toString())
    // val cmpare = asJson.toString().mbox besideS rtJson.toString().mbox
    // println("\n\n\n----------------------------------------------")
    // println(cmpare)
    // println("========================================================")
    assert(asJson.toString() === rtJson.toString())
  }


}
