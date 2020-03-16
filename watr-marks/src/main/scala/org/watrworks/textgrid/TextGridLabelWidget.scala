package org.watrworks
package textgrid

import textboxing.{TextBoxing => TB}, TB._
import scalaz.{@@ => _, _} // , Scalaz._

// import scala.scalajs.js.annotation._
import scala.annotation.meta.field

import watrmarks._
import geometry._
import annots._
import utils.GraphPaper



case class MarginalGloss(
  columns: List[MarginalGloss.Column]
)

object MarginalGloss {

  case class Column(
    gloss: List[Gloss]
  )

  sealed trait Gloss
  case class VSpace(len: Int) extends Gloss
  case class Labeling(label: Label, len: Int) extends Gloss

}


sealed trait GridRegion  {
  def bounds(): LTBounds
  def classes(): List[String]

  val gridBox: GraphPaper.Box =
    GraphPaper.boundsToBox(bounds)

  def isCells(): Boolean = false
  def isHeading(): Boolean = false
  def isLabelCover(): Boolean = false
  def isLabelKey(): Boolean = false

}



object GridRegion {

  case class Cells[A <: LabelTarget](
    // cells: Seq[TextGrid.GridCell],
    cells: Seq[A],
    row: Int,
    override val bounds: LTBounds,
    override val classes: List[String]
  ) extends GridRegion {
    override def isCells(): Boolean = true
  }

  case class Heading(
    heading: String,
    override val bounds: LTBounds,
    override val classes: List[String]
  ) extends GridRegion {
    override def isHeading(): Boolean = true
  }

  case class LabelCover(
    label: Label,
    override val bounds: LTBounds,
    override val classes: List[String]
  ) extends GridRegion {
    override def isLabelCover(): Boolean = true
  }

  case class LabelKey(
    labelIdent: String,
    override val bounds: LTBounds,
    override val classes: List[String]
  ) extends GridRegion {
    override def isLabelKey(): Boolean = true
  }

}



object TextGridLabelWidget {

  import LabeledSequenceTreeTransforms._

  val Indent: Int = 4

  def labelTreeToMarginals[A <: LabelTarget](labelTree: Tree[LabelTreeNode[A]], compactMarginals: Boolean): MarginalGloss = {
    val tree = labelTreeToMarginalSpanTree(labelTree, compactMarginals)

    val columns = tree.levels.toList.map{ level =>

      val spacers = level.foldLeft(List.empty[(Int, Int)]) { case (acc, (_, st, len)) =>
        val (lastEnd, lastSpace) = acc.headOption.getOrElse( (0, 0) )
        val thisSpace = (st+len, st-lastEnd)
        thisSpace :: acc
      }

      val spaces = spacers.map(_._2).reverse.map(MarginalGloss.VSpace(_))

      val glossColumn = (level zip spaces).toList
        .flatMap { case ((lbl, st, len), space) =>
          val gloss = lbl
            .map{ MarginalGloss.Labeling(_, len) }
            .getOrElse { MarginalGloss.VSpace(len) }

          List(space, gloss)
        }

      MarginalGloss.Column(List(glossColumn:_*))
    }
    MarginalGloss(List(columns:_*))
  }

  def marginalGlossToTextBlock(marginalLabels: MarginalGloss): TB.Box = {

    val colBoxes = marginalLabels.columns.map{ col =>
      val colPins = col.gloss.map{ _ match {
        case MarginalGloss.VSpace(len) =>
          vspace(len)

        case MarginalGloss.Labeling(label, len) =>
          if (len==1) {
            label.fqn.take(2).mkString.toUpperCase().box
          } else {
            val ch = label.fqn.take(2).mkString.toLowerCase().box
            vjoin(ch, vjoins(List.fill(len-2)("│┆")), "└─")
          }
      }}

      vjoins(colPins)
    }
    borderLeftRight("|", ":")(hcat(top, colBoxes))
  }

  def labelTreeToGridRegions[A <: LabelTarget](
    labelTree: Tree[LabelTreeNode[A]],
    labelSchemas: LabelSchemas,
    originX: Int,
    originY: Int
  ) (implicit ShowA: Show[A]) : Seq[GridRegion] = {

    def marginalGlossToGridRegions(marginalLabels: MarginalGloss, x: Int, y: Int): Seq[GridRegion] = {
      val allRegions = marginalLabels.columns.zipWithIndex.map{ case (col, colNum) =>
        val colRegions = col.gloss.foldLeft(
          (List[GridRegion](), y)
        ){ case ((regionAcc, accLen), e) =>
          e match {
            case MarginalGloss.VSpace(len) =>
              (regionAcc, accLen + len)

            case MarginalGloss.Labeling(label, len) =>
              val bounds = LTBounds.Ints(x+(colNum*2), accLen, 2, len)
              val classes = List(label.fqn)
              val gridRegion = GridRegion.LabelCover(label, bounds, classes)
              (gridRegion +: regionAcc, accLen + len)
          }
        }
        colRegions._1
      }
      allRegions.flatten
    }

    def labeledRowsToGridRegions(gridLines: List[LabeledRowElem], x: Int, y: Int): List[GridRegion] = {

      gridLines.zipWithIndex.flatMap { case (labeledLine, labeledLineNum) =>
        labeledLine match {
          case LabeledRowElem.CellGroupRow(labels, cells0, depthMod) =>

            val cells = cells0.flatMap(_.cells)
            // val rowNums = cells0.map(_.gridRow).toSet
            // if (rowNums.size != 1) {
            //   sys.error(s"more than one grid row found in label tree structure")
            // }
            // val rowNum = rowNums.head
            val rowNum = 1

            val classes = labels.map(_.fqn)

            val cellsStart = x + (Indent * (labels.length+1+depthMod))

            val top = y + labeledLineNum
            val height = 1
            val bounds = LTBounds.Ints(cellsStart, top, cells.length, height)
            List(
              GridRegion.Cells(cells, rowNum, bounds, classes)
            )

          case LabeledRowElem.HeadingRow(labels, heading) =>
            val clippedHeading = if (heading.length() > 75) {
              heading.substring(0, 75) ++ " ..."
            } else {
              heading
            }

            val left = x + (Indent * labels.length)
            val top = y + labeledLineNum
            val width = clippedHeading.length()
            val height = 1

            val bounds = LTBounds.Ints(left, top, width, height)
            val classes = labels.map(_.fqn)
            List(
              GridRegion.Heading(clippedHeading, bounds, classes))

        }}

    }

    def labelSchemaToGridRegions(s: LabelSchemas, x0: Int, y0: Int)
        : Seq[GridRegion] = {

      def loop(s: LabelSchema, x: Int, y: Int): Seq[GridRegion] = {
        val labelText = s.getAbbrev + ": " + s.label.fqn
        val width = labelText.length()
        val height = 1
        val bounds = LTBounds.Ints(x, y, width, height)
        val classes = List(s.label.fqn)

        val childRegions: List[GridRegion] = s.children.zipWithIndex
          .flatMap{ case (ch, chi) => loop(ch, x+Indent, y+chi+1) }

        GridRegion.LabelKey(labelText, bounds, classes) +: childRegions
      }

      val init = Seq[GridRegion]()
      val res = s.schemas.foldLeft(init) { case  (accRegions, schema) =>
        accRegions ++ loop(schema, x0, y0+accRegions.length)
      }

      res
    }

    val marginalGloss = labelTreeToMarginals(labelTree, compactMarginals = false)
    val gridLines = flattenLabelTreeToLines(labelTree)

    val glossRegions = marginalGlossToGridRegions(marginalGloss, originX, originY)
    val gridRegions = labeledRowsToGridRegions(gridLines, x=originX+(marginalGloss.columns.length*2)+2, y=originY)
    val schemaRegions = labelSchemaToGridRegions(labelSchemas, originX+8, originY+gridLines.length+4)

    gridRegions ++ schemaRegions ++ glossRegions
  }

  def flattenLabelTreeToLines[A <: LabelTarget : Show](labelTree: Tree[LabelTreeNode[A]])
      : List[LabeledRowElem] = {

    def histo(node: LabelTreeNode[A], children: Stream[Tree[LabeledRows]]): List[LabeledRowElem] = {
      node match {
        case n: LabelTreeNode.CellGroup[A] =>
          List(LabeledRowElem.CellGroupRow(List(), List(n)))

        case LabelTreeNode.RootNode =>
          children.toList.flatMap { _.rootLabel }

        case n @ LabelTreeNode.LabelNode(label) =>
          val childRowElems: List[LabeledRowElem] = children.toList.flatMap { _.rootLabel }

          val headerList = childRowElems.collect{
            case r @ LabeledRowElem.CellGroupRow(labels, cells, depthMod) =>
              r.getRowText()
          }

          val localText = headerList.mkString

          val localHeader = LabeledRowElem.HeadingRow(List(label), localText)

          val updatedChildren:List[LabeledRowElem] = childRowElems.map{ _ match {
            case r @ LabeledRowElem.CellGroupRow(labels, cells, depthMod) =>
              r.copy(labels = label+:labels)(r.showA)

            case r @ LabeledRowElem.HeadingRow(labels, heading) =>
              r.copy(labels = label+:labels)
          }}

          val shouldShiftFirstChild = updatedChildren.headOption.exists { firstChild =>
            firstChild.isInstanceOf[LabeledRowElem.CellGroupRow[_]] && firstChild.getRowText == localText
          }

          val shiftedChildren = if (shouldShiftFirstChild) {
            updatedChildren.headOption.map{head =>
              head.asInstanceOf[LabeledRowElem.CellGroupRow[A]].copy(
                depthMod = -1
              ) :: updatedChildren.tail
            } getOrElse { updatedChildren }
          } else {
            updatedChildren
          }

          val shouldPrependHeader = updatedChildren.headOption.exists { firstChild =>
            firstChild.getRowText != localText
          }

          if (shouldPrependHeader) {
            localHeader :: shiftedChildren
          } else shiftedChildren
      }
    }

    labelTree.scanr(histo).rootLabel
  }

  // def textGridToIndentedBox(textGrid: TextGrid): TB.Box = {
  //   val labelTree = textGridToLabelTree(textGrid)

  //   val lls = flattenLabelTreeToLines(labelTree)

  //   val dbg = lls.map { _ match {
  //     case LabeledRowElem.CellGroupRow(labels, cells, depthMod) =>
  //       val text = cells.map(_.cells.map(_.char).mkString).mkString
  //       val depth = Indent * (labels.length+1+depthMod)
  //       (
  //         labels.map(_.fqn).mkString(", "),
  //         indent(
  //           depth,
  //           s"${text}"
  //         )
  //       )
  //     case LabeledRowElem.HeadingRow(labels, heading) =>
  //       val text = heading
  //       val depth = Indent * labels.length
  //       (
  //         labels.map(_.fqn).mkString(", "),
  //         indent(
  //           depth,
  //           s"▸ ${text}"
  //         )
  //       )
  //   }}

  //   vjoins(left, dbg.map(_._2))
  // }



}
