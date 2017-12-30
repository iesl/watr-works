package edu.umass.cs.iesl.watr
package textgrid

import scala.scalajs.js
import scala.scalajs.js.annotation._
import js.JSConverters._

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
  def textGridToWidgetGrid(
    textGrid: TextGrid,
    labelSchemas: LabelSchemas,
    originX: Int,
    originY: Int
  ): WidgetDisplayGridProps = {
    val labelTree = textGridToLabelTree(textGrid)
    val gridRegions = labelTreeToGridRegions(labelTree, labelSchemas, originX, originY)
    new  WidgetDisplayGridProps(
      labelTree,
      gridRegions
    )
  }

  @JSExport
  def writeTextGrid(
    gridProps: WidgetDisplayGridProps,
    graphPaper: GraphPaper,
    rtreeApi: RTreeApi
  ): Unit = {

    val rtreeData = gridProps.gridRegions.map { region => region match {
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

    rtreeApi.loadData(rtreeData.toJSArray)
  }

}

@JSExportTopLevel("watr.textgrid.WidgetDisplayGridProps")
class WidgetDisplayGridProps(
  val labelTree: scalaz.Tree[TreeNode],
  val gridRegions: Seq[GridRegion]
) {
  lazy val regionExtents = gridRegions.map{r =>
    (r.bounds.getRight.toInt, r.bounds.getBottom.toInt)
  }

  @JSExport
  def getGridRowCount(): Int = {
    regionExtents.map(_._2).max
  }

  @JSExport
  def getGridColCount(): Int = {
    regionExtents.map(_._1).max
  }

}

@JSExportTopLevel("watr.textgrid.RTreeRect")
@JSExportAll
class RTreeRect(
  val region: GridRegion,
) {

  val bounds = {
    val LTBounds.Ints(l, t, w, h) = region.bounds
    LTBounds.Ints(l*4, t*4, w*4, h*4)
  }

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
