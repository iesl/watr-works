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

  // "it should support plaintext rows" in {
  //   val ds = docStore.getDocuments()
  //   println(s"ds: ${ds}")
  //   visualizeDocStore()
  // }

  // "it should join two rows into one, with optional space or dehyphenation" in {}

  // it should "support alphabet soup regions" in {}
  // it should "clip single row to target region(s)" in {}
  // it should "clip multiple rows to target region(s)" in {}

  // behavior of "TextGrid Cursors"

  // it should "slurp/barf" in {}

  info("behavior of windows")

  info("behavior of Serialization")

  // "it should ser grids" in {
  //   val stableId = DocumentID("docXX")
  //   for {
  //     (doc, i) <- docs.zipWithIndex
  //     pages <- docs
  //     page <- pages
  //   } {
  //     val textGrid = stringToPageTextGrid(stableId, page,  PageNum(1), None)
  //     val asJson = textGrid.toJson
  //     val roundTripGrid = TextGrid.fromJson(asJson)
  //     val rtJson = roundTripGrid.toJson()
  //     // val cmpare = asJson.toString().mbox besideS rtJson.toString().mbox
  //     // println("\n\n\n----------------------------------------------")
  //     // println(cmpare)
  //     // println("========================================================")
  //     assert(asJson.toString() === rtJson.toString())
  //   }
  // }

  "it should round-trip ser/unser" in {
    // val stableId = DocumentID("docXX")
    // val codecs =  new TextGridCodecs(stableId)
    // val pageRegion0 = PageRegion(
    //   StablePage(
    //     DocumentID("xyz"),
    //     PageNum(0)
    //   ),
    //   LTBounds.IntReps(100, 200, 300, 400)
    // )

    // val charAtom = CharAtom(
    //   CharID(0),
    //   pageRegion0,
    //   "A"
    // )
    // val cell: TextGrid.GridCell = TextGrid.PageItemCell(charAtom, Seq(), 'A')
    // val cell2: TextGrid.GridCell = TextGrid.InsertCell('B', pageRegion0)

    // json""" { "g": [ [ "2", 4, [25081, 4269, 337, 530]] ] } """
    // val enc = codecs.encodeCell(cell)
    // val enc2 = codecs.encodeCell(cell2)
    // val roundTrip = codecs.decodeCell(enc)
    // // val roundTrip = cell.asJson.as[TextGrid.GridCell]

    // println(cell)
    // println(s"encoded> ${enc}")
    // println(s"encoded2> ${enc2}")
    // println(roundTrip)

  }

  // "it should render to a textgrid labeling widget" in {
  //   val stableId = DocumentID("docXX")
  //   for {
  //     (doc, i) <- docs.zipWithIndex
  //     pages <- docs
  //     page <- pages
  //   } {
  //     val textGrid = stringToPageTextGrid(stableId, page,  PageNum(1), None)

  //     textGrid.rows.map { row =>
  //       row.cells.head
  //     }
  //   }
  // }

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

  import TextGridLabelWidget._

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
    println(asJson)
    val roundTripGrid = TextGrid.fromJson(asJson)
    val rtJson = roundTripGrid.toJson()


    val indentedBlock = textGridToIndentedBox(roundTripGrid)
    val labelTree = textGridToLabelTree(roundTripGrid)
    val expMarginals = labelTreeToMarginals(labelTree, compactMarginals=false)
    val emarginBlock = marginalGlossToTextBlock(expMarginals)
    val expBlock = emarginBlock + indentedBlock
    println("post: ")
    println(expBlock.toString())
    // val cmpare = asJson.toString().mbox besideS rtJson.toString().mbox
    // println("\n\n\n----------------------------------------------")
    // println(cmpare)
    // println("========================================================")
    assert(asJson.toString() === rtJson.toString())
  }


}
