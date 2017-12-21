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

          childRowElems.foldLeft(List.empty[LabeledRowElem]) { case (accList, rowElem) =>

            val expandedRow = (accList.headOption, rowElem) match {
              case (Some(
                r0 @ LabeledRowElem.CellGroupRow(_, cells0) ),
                r1 @ LabeledRowElem.CellGroupRow(_, cells1)
              ) =>
                val cellsAreSameRow = (cells0 ++ cells1).map(_.gridRow).toSet.size==1
                if (cellsAreSameRow) {
                  List(LabeledRowElem.CellGroupRow(List(), cells0 ++ cells1))
                } else {
                  List(r0, r1)
                }

              case (None, r) => List(r)
              case (Some(r0), r1) => List(r0, r1)
            }

            expandedRow ++ accList
          }

      }

    }

    labelTree.scanr(histo).rootLabel

  }

  def textGridToLabelingWidget(textGrid: TextGrid): Unit = {
    // val labelTree = textGridToLabelTree(textGrid)

    // val rowLengths = textGrid.rows.map(_.cells.length)

    // val lls = flattenLabelTreeToLines(labelTree)

    // val dbg = lls.map { case (labels: List[Label], text: String) =>
    //   s"${labels}: ${text}"
    // }

    // println(
    //   dbg.mkString("{\n  ", "\n  ", "\n}")
    // )


    // val gridTreeLocs = mapGridCellsToTreeLocs(textGrid)

    // val rows = gridTreeLocs.map { treeLocRow =>
    //   val rowText = treeLocRow.map(_.getLabel.cell.char).mkString
    //   val depth = treeLocRow.head.parents.length-1
    //   val headLoc = treeLocRow.head
    //   val headCell = headLoc.getLabel.cell
    //   val lastCell = treeLocRow.last.getLabel.cell

    //   // val mod1 = headLoc.pins.isEmpty ? -1 else tailLoc.pins.top.isLast || locRow.len==1 && headLoc.pins.top.isUnit
    //   // headCell.pins.isEmpty
    //   val rowHeaders = parentHeaders(headLoc).drop(1)
    //   val truncatedHeaders = rowHeaders.dropWhile { case (headerText, _) =>
    //     headerText == rowText
    //   }

    //   println(s"For Row ${rowText}")
    //   println("Row Headers")
    //   println(rowHeaders.map(si => s"${si._2}: ${si._1}").mkString("\n  ", "\n  ", "\n"))

    //   val headerList = truncatedHeaders.reverse

    //   println("Truncated Headers")
    //   println(truncatedHeaders.map(si => s"${si._2}: ${si._1}").mkString("\n  ", "\n  ", "\n"))

    //   println("Final Headers")
    //   println(headerList.map(si => s"${si._2}: ${si._1}").mkString("\n  ", "\n  ", "\n"))

    //   println()
    //   println()
    //   val indentedHdrs = headerList.map{ case (h, i) =>
    //     indent(i*Indent, "+".besideS(h))
    //   }

    //   if (headerList.nonEmpty) {
    //     vjoin(left,
    //       vjoins(left, indentedHdrs),
    //       indent((depth)*Indent, ">".besideS(rowText))
    //     )
    //   } else {
    //     indent((depth)*Indent, ">".besideS(rowText))
    //   }
    // }

    // val block = vjoins(left, rows)
    // println(block.toString)

    // - output is a list of grid data points, with bboxes, classes,  or spacers, which may also have classes
    //     or create indentation. classes allow hover highlighting for indentation spaces
    // - to create left-side controls...
    //   - "Marginalize" the BIO labels
  }

  // def getTextAtTreeLoc(loc: TreeLoc[TreeNode]): String = {
  //   val rleaves = loc.tree.loc.cojoin.toStream.filter(_.isLeaf)
  //   rleaves.map(_.getLabel.asInstanceOf[TreeNode.CellNode].cell.char).mkString
  // }

  // def parentHeaders(loc: TreeLoc[TreeNode.CellNode]): Seq[(String, Int)] = {
  //   def _parentHeaders(loc: TreeLoc[TreeNode]): Seq[(String, Int)] = {
  //     val parentNotRoot = loc.parents.length > 1
  //     if (loc.isFirst && parentNotRoot) {
  //       loc.parent.map{ p =>
  //         (getTextAtTreeLoc(p), p.parents.length-1) +: _parentHeaders(p)
  //       } getOrElse {
  //         Seq()
  //       }
  //     } else {
  //       Seq()
  //     }
  //   }
  //   _parentHeaders(loc.asInstanceOf[TreeLoc[TreeNode]])
  // }

  // def mapGridCellsToTreeLocs(textGrid: TextGrid): Seq[Seq[TreeLoc[TreeNode.CellNode]]] = {
  //   val labelTree = textGridToLabelTree(textGrid)

  //   val leafLocs: List[TreeLoc[TreeNode.CellNode]] = labelTree.loc.cojoin.toStream
  //     .filter(_.isLeaf)
  //     .map(_.asInstanceOf[TreeLoc[TreeNode.CellNode]])
  //     .toList

  //   val init = List[List[TreeLoc[TreeNode.CellNode]]]()

  //   val rowLengths = textGrid.rows.map(_.cells.length)
  //   val locRowsR = rowLengths.foldLeft { (init, leafLocs) } {
  //     case ((acc, locs), e) =>
  //       val (row, rest) = (locs.take(e), locs.drop(e))
  //       val asdf: List[List[TreeLoc[TreeNode.CellNode]]] = row +: acc
  //       // val asdf: List[List[TreeLoc[TreeNode.CellNode]]] = acc

  //       (asdf, rest)
  //   }

  //   locRowsR._1.reverse

  // }


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

      println("Current Tree:")
      println(currLoc.toTree.drawBox.toString())
    }

    currLoc.root.toTree

  }


  def bioPinsToRoseTreeStringVer(textGrid: TextGrid): Tree[String] = {
    val init = Tree.Node("<empty label>", Stream.empty)
    var currLoc = init.loc

    def up(): Unit = {
      currLoc = currLoc.parent.getOrElse(sys.error("no parent found"))
    }

    for {
      (cell, row, col) <- textGrid.indexedCells()
    } {
      // println(s"cell.pins: ${cell.pins}")
      val pinStack = cell.pins.reverse
      val basePins = pinStack.drop(currLoc.parents.length)
      // println(s"========")
      // println(s"tree depth: ${currLoc.parents.length}")
      // println(s"pinStack: ${pinStack}")
      // println(s"basePins: ${basePins}")

      basePins
        .takeWhile(p => p.isBegin || p.isUnit)
        .foreach { pin =>
          val n = Tree.Node(pin.label.fqn, Stream.empty)
          currLoc = currLoc.insertDownLast(n)
        }


      currLoc = currLoc.insertDownLast(Tree.Leaf(s"${cell.char}"))
      up()

      // println("Current: ")
      // println(currLoc.root.toTree.drawTree)
      // println()

      cell.pins
        .takeWhile(p => p.isLast || p.isUnit)
        .foreach { _ => up()  }
    }

    currLoc.root.toTree

  }
}

          // children.toList.foldLeft(
          //   List.empty[LabeledRowElem]
          // )({case (accList, elem: Tree[List[LabeledRowElem]]) =>


          //   val childLabeledRow = elem.rootLabel

          //   childLabeledRow.foldLeft(List.empty[LabeledRowElem]) {
          //     case (accList, rowElem: LabeledRowElem) =>
          //       val expandedRow = rowElem match {
          //         case r1 @ LabeledRowElem.CellGroupRow(labels, cells1) =>

          //           accList.headOption.map{ _ match {
          //             case r0 @ LabeledRowElem.CellGroupRow(labels, cells0) =>
          //               val cellsAreSameRow = (cells0 ++ cells1).map(_.rowNum).toSet.size==1
          //               if (cellsAreSameRow) {
          //                 List(LabeledRowElem.CellGroupRow(List(), cells0 ++ cells1))
          //               } else {
          //                 List(r0, r1)
          //               }

          //             case r0 @ LabeledRowElem.HeadingRow(labels, heading) =>
          //               List(r0, r1)
          //           }}

          //         case r1 @ LabeledRowElem.HeadingRow(labels, heading) =>
          //           List(r1)
          //       }
          //       expandedRow :: accList
          //   }

          //   childLabeledRow.map{ rowElem =>
          //     rowElem match {
          //       case LabeledRowElem.CellGroupRow(labels: List[Label], cells: Seq[TreeNode.CellNode]) =>
          //       case LabeledRowElem.HeadingRow(labels: List[Label], heading: String) =>
          //     }
          //   }



          //   accList
          // })
          // val childrenAreLabeled = children.exists { _.rootLabel.exists { _._1.nonEmpty }}

          // val text = children.map{ ch =>
          //   ch.rootLabel
          //     .map{ case (labels: List[Label], text: String) => text }
          //     .mkString
          // }.mkString

          // if (childrenAreLabeled) {
          //   val childs: LabeledRows = children.toList.flatMap { _.rootLabel.map {
          //     case (labels: List[Label], text: String) =>
          //       (label :: labels, text)
          //   }}

          //   (List(label), text) :: childs
          // } else {
          //   List((List(label), text))
          // }

  // def flattenLabelTreeToLinesVer2(labelTree: Tree[TreeNode]): LabeledLines = {
  //   val labeledLines = labelTree.scanr { (node: TreeNode, children: Stream[Tree[LabeledLines]]) =>

  //     val res: LabeledLines = node match {

  //       case TreeNode.CellNode(cell, gridRow) =>
  //         List((List[Label](), cell.char.toString()))

  //       case TreeNode.LabelNode(label) =>
  //         val childrenAreLabeled = children.exists { _.rootLabel.exists { _._1.nonEmpty }}

  //         val text = children.map{ ch =>
  //           ch.rootLabel
  //             .map{ case (labels: List[Label], text: String) => text }
  //             .mkString
  //         }.mkString

  //         if (childrenAreLabeled) {
  //           val childs: LabeledLines = children.toList.flatMap { _.rootLabel.map {
  //             case (labels: List[Label], text: String) =>
  //               (label :: labels, text)
  //           }}

  //           (List(label), text) :: childs
  //         } else {
  //           List((List(label), text))
  //         }


  //       case TreeNode.UnlabeledNode =>
  //         children.toList.flatMap { _.rootLabel }
  //     }

  //     res
  //   }

  //   labeledLines.rootLabel
  // }
