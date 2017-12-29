package edu.umass.cs.iesl.watr
package textgrid

import scala.scalajs.js.annotation._
import TypeTags._
import watrmarks._

@JSExportTopLevel("watr.textgrid.TextGridConstruction")
class TextGridConstructor(
) extends TextGridConstruction {


  def makeTextGrid(stableId: String, pageNum: Int, pageStr: String): TextGrid = {
    stringToPageTextGrid(DocumentID(stableId), pageStr, PageNum(pageNum), None)
  }

  val Authors = Label.auto
  val Author = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val LastName = Label.auto
  val Journal = Label.auto
  val RefMarker = Label.auto
  val RefNumber = Label.auto

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

  @JSExport
  def getTestTextGrid(): TextGrid = {
    val stableId = DocumentID("docXX")
    var textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    val labeledRow = addLabelsToGridRow(textGrid.rows.head, labelSpans)
    textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()
    textGrid = textGrid.split(9, 7).get


    textGrid
  }

  @JSExport
  def getTestLabelSchema(): LabelSchemas = {

    val authorNameSchema = LabelSchema(
      Author, Some(('a', 'u')), List(
        LabelSchema(FirstName),
        LabelSchema(MiddleName),
        LabelSchema(LastName))
    )

    val authorListSchema = LabelSchema(
      Authors, Some(('a', 's')), List(
        authorNameSchema)
    )

    val refMarkerSchema = LabelSchema(
      RefMarker, None, List(
        LabelSchema(RefNumber))
    )

    LabelSchemas(
      List(
        authorListSchema,
        refMarkerSchema)
    )
  }

  import utils.GraphPaper
  import geometry._
  import TextGridLabelWidget._

  @JSExport
  def drawTextGridToGraphPaper(textGrid: TextGrid, labelSchemas: LabelSchemas, graphPaper: GraphPaper): Unit = {
    val labelTree = textGridToLabelTree(textGrid)
    val gridRegions = labelTreeToGridRegions(labelTree, labelSchemas, 2, 3)
    println(s"drawTextGridToGraphPaper: ")

    gridRegions.foreach { region => region match {
      case GridRegion.Cell(cell, row, col, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawString(l, t, cell.char.toString())

      case GridRegion.Heading(heading, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        // graphPaper.drawString(l, t, heading)
        graphPaper.drawBox(GraphPaper.Box(GraphPaper.GridCell(l, t), 10, 0))

      case GridRegion.LabelCover(label, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawBox(GraphPaper.Box(GraphPaper.GridCell(l, t), w-1, h-1), GraphPaper.BorderLineStyle.SingleWidth)

      case GridRegion.LabelKey(labelIdent, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawString(l, t, labelIdent)

    }}
  }

}
