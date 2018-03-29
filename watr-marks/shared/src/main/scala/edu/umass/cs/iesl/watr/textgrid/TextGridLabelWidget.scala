package edu.umass.cs.iesl.watr
package textgrid

import textboxing.{TextBoxing => TB}, TB._
import scalaz.{@@ => _, _} // , Scalaz._

import scala.scalajs.js.annotation._

import watrmarks._
import geometry._
import utils.GraphPaper

import TextGridFunctions._

sealed trait TreeNode

object TreeNode {

  case class CellGroup(
    cells: List[TextGrid.GridCell],
    gridRow: Int
  ) extends TreeNode

  case class LabelNode(
    label: Label
  ) extends TreeNode

  case object RootNode extends TreeNode

  implicit val ShowTreeNode = Show.shows[TreeNode]{ treeNode =>
    treeNode match {
      case TreeNode.CellGroup(cells, gridRow) => cells.map(_.char.toString()).mkString
      case TreeNode.LabelNode(l) => l.fqn
      case TreeNode.RootNode => "()"
    }
  }
}

sealed trait LabeledRowElem {
  def labels: List[Label]
  def getRowText: String
}

object LabeledRowElem {

  case class CellGroupRow(
    override val labels: List[Label],
    cells: Seq[TreeNode.CellGroup],
    depthMod: Int = 0
  ) extends LabeledRowElem {
   def getRowText: String = {
      cells.map(_.cells.map(_.char).mkString).mkString
    }

  }

  case class HeadingRow(
    override val labels: List[Label],
    heading: String
  ) extends LabeledRowElem {
    def getRowText: String = heading
  }

}


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

  @JSExport val gridBox: GraphPaper.Box = {
    GraphPaper.boundsToBox(bounds)
  }

  @JSExport def isCells(): Boolean = false
  @JSExport def isHeading(): Boolean = false
  @JSExport def isLabelCover(): Boolean = false
  @JSExport def isLabelKey(): Boolean = false

}

import scala.annotation.meta.field

@JSExportAll
@JSExportTopLevel("watr.textgrid.GridRegion")
object GridRegion {

  case class Cells(
    @(JSExport @field) cells: Seq[TextGrid.GridCell],
    @(JSExport @field) row: Int,
    @(JSExport @field) override val bounds: LTBounds,
    override val classes: List[String]
  ) extends GridRegion {
    override def isCells(): Boolean = true
  }

  case class Heading(
    @(JSExport @field) heading: String,
    @(JSExport @field) override val bounds: LTBounds,
    override val classes: List[String]
  ) extends GridRegion {
    override def isHeading(): Boolean = true
  }

  case class LabelCover(
    @(JSExport @field) label: Label,
    @(JSExport @field) override val bounds: LTBounds,
      override val classes: List[String]
  ) extends GridRegion {
    override def isLabelCover(): Boolean = true
  }

  case class LabelKey(
    @(JSExport @field) labelIdent: String,
    @(JSExport @field) override val bounds: LTBounds,
    override val classes: List[String]
  ) extends GridRegion {
    override def isLabelKey(): Boolean = true
  }

}

@JSExportTopLevel("watr.textgrid.TextGridLabelWidget") @JSExportAll
object TextGridLabelWidget {

  val Indent: Int = 4

  def labelTreeToMarginals(labelTree: Tree[TreeNode], compactMarginals: Boolean): MarginalGloss = {
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
            // val ch = label.fqn(0).toLower.toString()
            val ch = label.fqn.take(2).mkString.toLowerCase().box
            vjoin(ch, vjoins(List.fill(len-2)("│┆")), "└─")
          }
      }}

      vjoins(colPins)
    }
    borderLeftRight("|", ":")(hcat(top, colBoxes))
  }

  def labelTreeToGridRegions(labelTree: Tree[TreeNode], labelSchemas: LabelSchemas, originX: Int=0, originY: Int=0): Seq[GridRegion] = {

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
            val rowNums = cells0.map(_.gridRow).toSet
            if (rowNums.size != 1) {
              sys.error(s"more than one grid row found in label tree structure")
            }
            val rowNum = rowNums.head

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

    def labelSchemaToGridRegions(s: LabelSchemas, x0: Int, y0: Int): Seq[GridRegion] = {

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

  def flattenLabelTreeToLines(labelTree: Tree[TreeNode]): List[LabeledRowElem] = {

    def histo(node: TreeNode, children: Stream[Tree[LabeledRows]]): List[LabeledRowElem] = {
      node match {
        case n: TreeNode.CellGroup =>
          List(LabeledRowElem.CellGroupRow(List(), List(n)))

        case TreeNode.RootNode =>
          children.toList.flatMap { _.rootLabel }

        case n @ TreeNode.LabelNode(label) =>
          val childRowElems: List[LabeledRowElem] = children.toList.flatMap { _.rootLabel }

          val headerList = childRowElems.collect{
            case r @ LabeledRowElem.CellGroupRow(labels, cells, depthMod) =>
              cells.map(_.cells.map(_.char).mkString).mkString
          }

          val localText = headerList.mkString

          val localHeader = LabeledRowElem.HeadingRow(List(label), localText)

          val updatedChildren:List[LabeledRowElem] = childRowElems.map{ _ match {
            case r @ LabeledRowElem.CellGroupRow(labels, cells, depthMod) =>
              r.copy(labels = label+:labels)

            case r @ LabeledRowElem.HeadingRow(labels, heading) =>
              r.copy(labels = label+:labels)
          }}

          val shouldShiftFirstChild = updatedChildren.headOption.exists { firstChild =>
            firstChild.isInstanceOf[LabeledRowElem.CellGroupRow] && firstChild.getRowText == localText
          }

          val shiftedChildren = if (shouldShiftFirstChild) {
            updatedChildren.headOption.map{head =>
              head.asInstanceOf[LabeledRowElem.CellGroupRow].copy(
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

  def textGridToIndentedBox(textGrid: TextGrid): TB.Box = {
    val labelTree = textGridToLabelTree(textGrid)

    val lls = flattenLabelTreeToLines(labelTree)

    val dbg = lls.map { _ match {
      case LabeledRowElem.CellGroupRow(labels, cells, depthMod) =>
        val text = cells.map(_.cells.map(_.char).mkString).mkString
        val depth = Indent * (labels.length+1+depthMod)
        (
          labels.map(_.fqn).mkString(", "),
          indent(
            depth,
            s"${text}"
          )
        )
      case LabeledRowElem.HeadingRow(labels, heading) =>
        val text = heading
        val depth = Indent * labels.length
        (
          labels.map(_.fqn).mkString(", "),
          indent(
            depth,
            s"▸ ${text}"
          )
        )
    }}

    vjoins(left, dbg.map(_._2))
  }



}
