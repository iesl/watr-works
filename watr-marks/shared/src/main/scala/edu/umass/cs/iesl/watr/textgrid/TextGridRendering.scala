package edu.umass.cs.iesl.watr
package textgrid


// import textboxing.{TextBoxing => TB}, TB._
import utils.ScalazTreeImplicits._
import scalaz.{@@ => _, _}, Scalaz._

import watrmarks._

sealed trait TreeNode

object TreeNode {

  case class CellGroup(
    cells: List[TextGrid.GridCell],
    gridRow: Int
  ) extends TreeNode

  case class LabelNode(
    label: Label
  ) extends TreeNode

  case object UnlabeledNode extends TreeNode

  implicit val ShowTreeNode = Show.shows[TreeNode]{ treeNode =>
    treeNode match {
      case TreeNode.CellGroup(cells, gridRow) => cells.map(_.char.toString()).mkString
      case TreeNode.LabelNode(l) => l.fqn
      case TreeNode.UnlabeledNode => "()"
    }
  }
}

sealed trait LabeledRowElem {
  def labels: List[Label]
}

object LabeledRowElem {

  case class CellGroupRow(
    override val labels: List[Label],
    cells: Seq[TreeNode.CellGroup]
  ) extends LabeledRowElem

  case class HeadingRow(
    override val labels: List[Label],
    heading: String
  ) extends LabeledRowElem


}


object TextGridRendering {

  val Indent: Int = 2

  type LabeledLines = List[(List[Label], String)]


  type LabeledRows = List[LabeledRowElem]

  def flattenLabelTreeToLines(labelTree: Tree[TreeNode]): List[LabeledRowElem] = {

    def histo(node: TreeNode, children: Stream[Tree[LabeledRows]]): List[LabeledRowElem] = {
      node match {
        case n: TreeNode.CellGroup =>
          List(LabeledRowElem.CellGroupRow(List(), List(n)))

        case TreeNode.UnlabeledNode =>
          children.toList.flatMap { _.rootLabel }

        case n @ TreeNode.LabelNode(label) =>
          val childRowElems: List[LabeledRowElem] = children.toList.flatMap { _.rootLabel }

          childRowElems.map{ _ match {
            case r @ LabeledRowElem.CellGroupRow(labels, cells) =>
              r.copy(labels = label::labels)

            case r @ LabeledRowElem.HeadingRow(labels, heading) =>
              r.copy(labels = label::labels)
          }}
      }
    }

    labelTree.scanr(histo).rootLabel
  }

  def textGridToLabelingWidget(textGrid: TextGrid): Unit = {
    val labelTree = textGridToLabelTree(textGrid)

    val lls = flattenLabelTreeToLines(labelTree)

    val dbg = lls.map { _ match {

      case r0 @ LabeledRowElem.CellGroupRow(labels, cells) =>
        val text = cells.map(_.cells.map(_.char).mkString).mkString
        s"${labels}: ${text}"
      case r0 @ LabeledRowElem.HeadingRow(labels, heading) =>
        val text = heading
        s"${labels}: ${text}"
    }}

    println(
      dbg.mkString("{\n  ", "\n  ", "\n}")
    )


    // - output is a list of grid data points, with bboxes, classes,  or spacers, which may also have classes
    //     or create indentation. classes allow hover highlighting for indentation spaces
    // - to create left-side controls...
    //   - "Marginalize" the BIO labels
  }



  def textGridToLabelTree(textGrid: TextGrid): Tree[TreeNode] = {
    val init = Tree.Node[TreeNode](TreeNode.UnlabeledNode, Stream.empty)
    var currLoc = init.loc

    def up(): Unit = {
      currLoc = currLoc.parent.getOrElse(sys.error("no parent found"))
    }

    for { (cell, row, col) <- textGrid.indexedCells() } {
      val pinStack = cell.pins.reverse
      val basePins = pinStack.drop(currLoc.parents.length)

      basePins.takeWhile(p => p.isBegin || p.isUnit)
        .foreach { pin =>
          val n = Tree.Node[TreeNode](TreeNode.LabelNode(pin.label), Stream.empty)
          currLoc = currLoc.insertDownLast(n)
        }

      val maybeAppend = for {
        lastChild <- currLoc.lastChild
      } yield {
        lastChild.getLabel match {
          case prevCell@ TreeNode.CellGroup(cells, prevRow) if prevRow == row =>
            lastChild.modifyLabel { p =>
              TreeNode.CellGroup(cells++List(cell), row): TreeNode
            }
          case _ =>
            currLoc.insertDownLast(
              Tree.Leaf[TreeNode](TreeNode.CellGroup(List(cell), row))
            )
        }
      }

      currLoc = maybeAppend.getOrElse {
        currLoc.insertDownLast(
          Tree.Leaf[TreeNode](TreeNode.CellGroup(List(cell), row))
        )
      }

      up()

      cell.pins.takeWhile(p => p.isLast || p.isUnit)
        .foreach { _ => up()  }
    }

    currLoc.root.toTree
  }


}

