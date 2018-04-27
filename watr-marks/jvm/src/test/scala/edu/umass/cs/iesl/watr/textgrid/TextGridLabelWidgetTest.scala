package edu.umass.cs.iesl.watr
package textgrid

import corpora._
import TypeTags._
import org.scalatest._

import watrmarks._
import utils.ScalazTreeImplicits._
import scala.collection.mutable
import scalaz.{@@ => _, _} , Scalaz._
import _root_.io.circe, circe._, circe.syntax._
import textboxing.{TextBoxing => TB}, TB._
import TextGridLabelWidget._
import TextGridFunctions._

case class LineRenderInfo(
  text: String,
  indent: Int,
  isHoverable: Boolean,
  canSplit: Boolean,
  canJoin: Boolean,
  canLabel: Boolean
)


trait TextGridSpec extends FreeSpec with Matchers with TextGridTestExamples {

  def infobox(heading: String, b: TB.Box): Unit = {
    info(heading)
    info("\n" + indent(4, b).toString() + "\n")
  }

}

class TextGridLabelWidgetTests extends TextGridSpec {
  import geometry._
  import utils.AsciiGraphPaper
  import utils.GraphPaper
  import utils.ExactFloats._

  def makeGraph(
    graphDimension: LTBounds
  ): AsciiGraphPaper = {
    val w: Int = graphDimension.width.asInt()
    val h: Int = graphDimension.height.asInt()
    val g = new AsciiGraphPaper(w+1, h+1)
    g.drawBox(GraphPaper.Box(GraphPaper.GridCell(0, 0), w, h))
    g
  }


  val jsonLabelSchema = {

    val authorNameSchema = LabelSchema(
      Author, Some(('a', 'u')), None, List(
        LabelSchema(FirstName),
        LabelSchema(MiddleName),
        LabelSchema(LastName))
    )

    val authorListSchema = LabelSchema(
      Authors, Some(('a', 's')), None, List(
        authorNameSchema)
    )

    val refMarkerSchema = LabelSchema(
      RefMarker, None, None, List(
        LabelSchema(RefNumber))
    )

    LabelSchemas(
      LabelSchemaName("TestSchema"),
      List(
        authorListSchema,
        refMarkerSchema)
    )
  }


  val unlabeledText = {
    //"0         1         2         3         4         5
    // 012345678901234567890123456789012345678901234567899 """
    """1. Bishop-Clark, C  and Wheeler, D; S.Eng. P-Hall"""
  }
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

  val stableId = DocumentID("")

  "Behavior of Textgrid Widget Creation" - {

    var textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)


    info("Starting with labeled TextGrid")

    val labeledRow = addLabelsToGridRow(textGrid.rows.head, labelSpans)

    info("\n"+ labeledRow.showRow().toString()+ "\n")

    info("... and normalized to one leaf label per line")

    textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()
    textGrid = textGrid.split(9, 7).get

    info("Split Journal")
    val gridTextBox = textGrid.toText().mbox
    info(s"==\n${textGrid.toText()}\n-------------------")

    val labelTree = textGridToLabelTree(textGrid)

    infobox(s"Create a tree structure out of the BIO labels", labelTree.drawBox)

    val indentedBlock = textGridToIndentedBox(textGrid)

    val cMarginals = labelTreeToMarginals(labelTree, compactMarginals=true)
    val expMarginals = labelTreeToMarginals(labelTree, compactMarginals=false)

    val cmarginBlock = marginalGlossToTextBlock(cMarginals)
    val emarginBlock = marginalGlossToTextBlock(expMarginals)
    val ltextBlock = cmarginBlock + gridTextBox

    val schemaBox = LabelSchemas.labelSchemaToBox(jsonLabelSchema)

    val expBlock = ((emarginBlock + indentedBlock) atop vspace(2) atop schemaBox)

    infobox(s"Create compact marginal labels", ltextBlock)

    infobox(s"Create indented tree-view marginal labels", expBlock)

  }

  "Behavior of Textgrid Labeling and Unlabeling" - {
    val textGrid = makeBishopClarkTextGrid()

    val indexedCells = textGrid.indexedCells()
    bishopClarkLabelSpans.reverse.foreach{ case ((lbegin, lend), label) =>
      val (cell, row, col) = indexedCells(lbegin)
      println(s"****unlabeling: ${row}, ${col}, ${label}")
      val extents = textGrid.findLabelExtents(row, col, label)

      val textExtents = extents.map { cells =>
        cells.map { case (cell, begin, end) =>
          cell.char
        }.mkString
      } getOrElse { "<not found>" }

      infobox(s"Unlabeled ${label}", textExtents)
      textGrid.unlabelNear(row, col, label)
      val indentedBlock = textGridToIndentedBox(textGrid)
      val labelTree = textGridToLabelTree(textGrid)
      val expMarginals = labelTreeToMarginals(labelTree, compactMarginals=false)
      val emarginBlock = marginalGlossToTextBlock(expMarginals)
      val expBlock = emarginBlock + indentedBlock
      infobox(s"Unlabeled ${label}", expBlock)
    }
  }


  // "Layout for Textgrid Widget" - {
  //   var textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
  //   val labeledRow = addLabelsToGridRow(textGrid.rows.head, labelSpans)
  //   textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
  //   textGrid = textGrid.splitOneLeafLabelPerLine()
  //   textGrid = textGrid.split(9, 7).get
  //   val labelTree = textGridToLabelTree(textGrid)
  //   val gridRegions = labelTreeToGridRegions(labelTree, jsonLabelSchema, 2, 3)

  //   val graphSize = LTBounds.Ints(0, 0, 50, 30)
  //   val graphPaper = makeGraph(graphSize)

  //   gridRegions.foreach { region => region match {
  //     case GridRegion.Cells(List(cell), row, bounds, classes) =>
  //       val LTBounds.Ints(l, t, w, h) = bounds
  //       graphPaper.drawString(l, t, cell.char.toString())

  //     case GridRegion.Heading(heading, bounds, classes) =>
  //       val LTBounds.Ints(l, t, w, h) = bounds
  //       graphPaper.drawBox(GraphPaper.Box(GraphPaper.GridCell(l, t), 10, 0))

  //     case GridRegion.LabelCover(label, bounds, classes) =>
  //       val LTBounds.Ints(l, t, w, h) = bounds
  //       graphPaper.drawBox(GraphPaper.Box(GraphPaper.GridCell(l, t), w-1, h-1), GraphPaper.BorderLineStyle.SingleWidth)

  //     case GridRegion.LabelKey(labelIdent, bounds, classes) =>
  //       val LTBounds.Ints(l, t, w, h) = bounds
  //       graphPaper.drawString(l, t, labelIdent)

  //   }}
  //   println(s"==\n${textGrid.toText()}\n-------------------")
  //   println(labelTree.drawBox)
  //   println(graphPaper.asColorString())
  // }
}
