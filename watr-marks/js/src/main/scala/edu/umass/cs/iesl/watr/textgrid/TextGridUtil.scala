package edu.umass.cs.iesl.watr
package textgrid

import scala.scalajs.js
import scala.scalajs.js.annotation._

import TypeTags._
import watrmarks._
import utils._
import geometry._
import TextGridLabelWidget._
import utils.ExactFloats._

@JSExportTopLevel("watr.textgrid.TextGridConstructor")
class TextGridConstructor extends TextGridConstruction {

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

  val stableId = DocumentID("docXX")


  @JSExport
  def getTestTextGrid(): TextGrid = {
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


    textGrid
  }

  @JSExport
  def getSampleTextGrid1(): TextGrid = {
    val labelSpans = List(
      ((0, 1),   RefMarker),
      ((0, 0),   RefNumber),
      // ((3, 33),  Authors),
      // ((3, 17),  Author),
      // ((3, 14),  LastName),
      // ((17, 17), FirstName),
      // ((24, 33), Author),
      // ((36, 48), Journal)
    )
    val unlabeledText = {
      //"0         1         2         3         4         5
      // 012345678901234567890123456789012345678901234567899 """
      """1. """

    }
    var textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    val labeledRow = addLabelsToGridRow(textGrid.rows.head, labelSpans)
    textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()

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


  @JSExport
  def drawTextGridToGraphPaper(textGrid: TextGrid, labelSchemas: LabelSchemas, graphPaper: GraphPaper): Unit = {
    val labelTree = textGridToLabelTree(textGrid)
    val gridRegions = labelTreeToGridRegions(labelTree, labelSchemas, 0, 0)
    println(s"drawTextGridToGraphPaper: ")

    gridRegions.foreach { region => region match {
      case GridRegion.Cell(cell, row, col, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawString(l, t, cell.char.toString())
        // println(s"${bounds}:   GridRegion.Cell(cell:${cell.char}, row:${row}, col:${col}) ")

      case GridRegion.Heading(heading, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawString(l, t, heading)
        graphPaper.drawBox(GraphPaper.Box(GraphPaper.GridCell(l, t), heading.length(), 0))
        // println(s"${bounds}:   GridRegion.Heading() ")

      case GridRegion.LabelCover(label, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawBox(GraphPaper.Box(GraphPaper.GridCell(l, t), w-1, h-1), GraphPaper.BorderLineStyle.SingleWidth)
        // println(s"${bounds}:   GridRegion.LabelCover() ")

      case GridRegion.LabelKey(labelIdent, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawString(l, t, labelIdent)
        // println(s"${bounds}:   GridRegion.LabelKey() ")
    }}
  }
  import js.JSConverters._

  @JSExport
  def writeTextGrid(
    textGrid: TextGrid,
    labelSchemas: LabelSchemas,
    graphPaper: GraphPaper,
    rtreeApi: RTreeApi
  ): Unit = {
    val labelTree = textGridToLabelTree(textGrid)
    val gridRegions = labelTreeToGridRegions(labelTree, labelSchemas, 0, 0)

    println(s"writeTextGrid")

    val rtreeData = gridRegions.map { region => region match {
      case r@ GridRegion.Cell(cell, row, col, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawString(l, t, cell.char.toString())
        // println(s"${bounds}:   GridRegion.Cell(cell:${cell.char}, row:${row}, col:${col}) ")
        new RTreeRect(r)

      case r@ GridRegion.Heading(heading, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawString(l, t, heading)
        graphPaper.drawBox(GraphPaper.Box(GraphPaper.GridCell(l, t), heading.length(), 0))
        // println(s"${bounds}:   GridRegion.Heading() ")
        new RTreeRect(r)

      case r@ GridRegion.LabelCover(label, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawBox(GraphPaper.Box(GraphPaper.GridCell(l, t), w-1, h-1), GraphPaper.BorderLineStyle.SingleWidth)
        // println(s"${bounds}:   GridRegion.LabelCover() ")
        new RTreeRect(r)

      case r@  GridRegion.LabelKey(labelIdent, bounds, classes) =>
        val LTBounds.Ints(l, t, w, h) = bounds
        graphPaper.drawString(l, t, labelIdent)
        // println(s"${bounds}:   GridRegion.LabelKey() ")
        new RTreeRect(r)
    }}

    // val data = gridRegions.map { region => region match {
    //   // case r@ GridRegion.Cell(cell, row, col, bounds, classes) => new RTreeData.Rect(r)
    //   case r: GridRegion.Cell => new RTreeRect(r)
    //   // case r@ GridRegion.Heading(heading, bounds, classes)     => new RTreeData.Rect[GridRegion](r, bounds)
    //   // case r@ GridRegion.LabelCover(label, bounds, classes)    => new RTreeData.Rect[GridRegion](r, bounds)
    //   // case r@ GridRegion.LabelKey(labelIdent, bounds, classes) => new RTreeData.Rect[GridRegion](r, bounds)
    // }}

    rtreeApi.loadData(rtreeData.toJSArray)
  }

}


@JSExportTopLevel("watr.textgrid.RTreeRect")
@JSExportAll
class RTreeRect(
  val region: GridRegion,
) {
  val bounds = region.bounds

  val left:Int   = bounds.left.asInt
  val top:Int    = bounds.top.asInt
  val width:Int  = bounds.width.asInt
  val height:Int = bounds.height.asInt

  val minX = left
  val minY = top
  val maxX = left + width
  val maxY = top + height

  val x = left
  val y = top

  val x1 = left
  val x2 = left + width
  val y1 = top
  val y2 = top + height

  val bottom = top + height
  val right  = left + width

  // def topLeft() = new PointPolyfill{
  //   override val x = self.left
  //   override val y = self.top
  // }
}

@js.native
trait RTreeApi extends js.Object {

  def loadData(data: js.Array[RTreeRect]): Unit = js.native

}
