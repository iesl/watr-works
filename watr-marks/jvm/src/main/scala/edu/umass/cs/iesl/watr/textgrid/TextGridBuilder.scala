package edu.umass.cs.iesl.watr
package textgrid

import TypeTags._
import corpora._

trait TextGridBuilder extends TextGridConstruction {
  def docStore: DocumentZoningApi
  def annotApi: DocumentAnnotationApi

  def addDocument(stableId: String@@DocumentID, pages:Seq[String]): Seq[TextGrid]  = {
    docStore.addDocument(stableId)
    val pageRegions = for {
      (page, n) <- pages.zipWithIndex
    } yield {
      val textGrid = loadPageFromString(stableId, PageNum(n), page)
      (textGrid, textGrid.pageBounds().head)
    }

    // docStore.labelRegions(LB.FullPdf, pageRegions.map(_._2))
    pageRegions.map(_._1)
  }

  def loadPageFromString(
    stableId: String@@DocumentID,
    pageNum: Int@@PageNum,
    pageBlock: String
  ): TextGrid = {
    val docId = docStore.getDocument(stableId).get
    val pageId = docStore.addPage(docId, pageNum)

    stringToPageTextGrid(stableId, pageBlock, pageNum, Some(pageId))
  }

  def visualizeDocStore(): Unit = {
    for {
      stableId     <- docStore.getDocuments()
      _             = println(s"stableId: ${stableId}")
      docId        <- docStore.getDocument(stableId).toSeq
      _             = println(s"Document $stableId id:${docId}")
      pageId       <- docStore.getPages(docId)
      pageGeometry  = docStore.getPageGeometry(pageId)
      _             = println(s"  Page  ${pageId}: ${pageGeometry}")
      // pageTextGrid <- docStore.getPageText(pageId)
    } {
      // println(pageTextGrid.toText())
      ???
    }
  }

}

trait TextGridTestExamples extends TextGridConstruction {
  import _root_.io.circe
  import watrmarks._

  val JsonPrettyPrinter = circe.Printer(
    preserveOrder = true,
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

  val sample3PageDocs = List(
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

  val Authors = Label.auto
  val Author = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val LastName = Label.auto
  val Journal = Label.auto
  val RefMarker = Label.auto
  val RefNumber = Label.auto

  val bishopClarkLabelSpans = List(
    ((0, 1),   RefMarker),
    ((0, 0),   RefNumber),
    ((3, 33),  Authors),
    ((3, 17),  Author),
    ((3, 14),  LastName),
    ((17, 17), FirstName),
    ((24, 33), Author),
    ((36, 48), Journal)
  )

  def makeBishopClarkTextGrid(): TextGrid = {

    val unlabeledText = {
      //"0         1         2         3         4         5
      // 012345678901234567890123456789012345678901234567899 """
      """1. Bishop-Clark, C  and Wheeler, D; S.Eng. P-Hall"""
    }
    val stableId = DocumentID("Bishop-Clark")
    var textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    val labeledRow = addLabelsToGridRow(textGrid.rows.head, bishopClarkLabelSpans)
    textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()
    textGrid = textGrid.split(9, 7).get
    textGrid
  }
}
