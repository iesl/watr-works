package edu.umass.cs.iesl.watr
package formats


import textgrid._
import org.scalatest._
import corpora._
import watrmarks._
import TypeTags._
import TextGridLabelWidget._

trait TextGridSpec extends FreeSpec with Matchers with TextGridBuilder {
  val Authors = Label.auto
  val Author = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val LastName = Label.auto
  val Journal = Label.auto
  val RefMarker = Label.auto
  val RefNumber = Label.auto

  val stableId = DocumentID("docXX")
}

class TextGridOutputFormatsTest extends TextGridSpec {

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




  "Behavior of labeled TextGrid serialization" in {
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

    val indentedBlock = textGridToIndentedBox(roundTripGrid)
    println(asJson.spaces4)
    // println(indentedBlock)
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
    // assert(asJson.toString() === rtJson.toString())
  }

}
