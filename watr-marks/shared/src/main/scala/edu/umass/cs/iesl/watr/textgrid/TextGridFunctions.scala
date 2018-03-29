package edu.umass.cs.iesl.watr
package textgrid

import scalaz.{@@ => _, _} // , Scalaz._
import watrmarks._
import _root_.io.circe, circe._, circe.syntax._
import utils.DoOrDieHandlers._
import TypeTags._

object TextGridFunctions {

  def gridCellsToLabelTree(gridCells: Seq[TextGrid.GridCell]): Tree[TreeNode] = {
    textGridToLabelTree(
      TextGrid.fromCells(DocumentID(""), gridCells)
    )
  }

  def textGridToLabelTree(textGrid: TextGrid): Tree[TreeNode] = {
    val init = Tree.Node[TreeNode](TreeNode.RootNode, Stream.empty)
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
              TreeNode.CellGroup(cell :: cells, row): TreeNode
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

    currLoc.root.toTree.map { n => n match {
      case TreeNode.CellGroup(cells, row) => TreeNode.CellGroup(cells.reverse, row)
      case n => n
    }}
  }


  type LabeledLines = List[(List[Label], String)]

  type LabeledRows = List[LabeledRowElem]

  type Start = Int
  type Len = Int
  type Attr = (Option[Label], Start, Len)

  implicit val ShowAttr = Show.shows[Attr]{ case(lbl, st, len) =>
    s"${lbl}: (${st}-${st+len})"
  }

  private def attrEndIndex(attr: Attr) = {
    val (_, st, len) = attr
    st+len
  }

  private def shiftChildren(ch: Stream[Tree[Tree[Attr]]], init: Int) =
    ch.foldLeft(Stream.empty[Tree[Attr]]){
      case (acc, child: Tree[Tree[Attr]]) =>
        val offset = acc.headOption.map { h: Tree[Attr] =>
          attrEndIndex(h.rootLabel)
        }.getOrElse(init)

        val adjusted = child.rootLabel.map{ case attr@ (label, begin, len) =>
          (label, begin+offset, len)
        }

        adjusted #:: acc
    }


  def spanTreeToJson(spanTree: Tree[Attr]): Json = {
    def histo(node: Attr, children: Stream[Tree[Option[Json]]]): Option[Json]= {
      val (label, begin, len) = node
      val childs = children.map(_.rootLabel).flatten.asJson
      val atRootNode = begin==0 && len==0 && label.isEmpty
      if (atRootNode) Some(childs) else {
        label.map { l =>
          Json.arr(
            Json.arr(l.asJson, Json.fromInt(begin), Json.fromInt(len)),
            childs
          )
        }
      }
    }

    spanTree.scanr(histo).rootLabel.orDie("root node should always have Json")
  }

  def labelTreeToSpanTree(labelTree: Tree[TreeNode]): Tree[Attr] = {

    def histo(node: TreeNode, children: Stream[Tree[Tree[Attr]]]): Tree[Attr] = {

      node match {
        case TreeNode.RootNode         => Tree.Node((None, 0, 0), shiftChildren(children, 0).reverse)
        case cells: TreeNode.CellGroup => Tree.Leaf((None, 0, cells.cells.length))
        case TreeNode.LabelNode(label) =>
          val shifted = shiftChildren(children, 0)
          val endOffset = shifted.headOption.map(_.rootLabel).map(attrEndIndex).getOrElse(0)
          Tree.Node((Some(label), 0, endOffset), shifted.reverse)
      }
    }

    labelTree.scanr(histo).rootLabel
  }

  def labelTreeToMarginalSpanTree(labelTree: Tree[TreeNode], compactMarginals: Boolean = true): Tree[Attr] = {

    def histo(node: TreeNode, children: Stream[Tree[Tree[Attr]]]): Tree[Attr] = {

      node match {
        case TreeNode.RootNode         => Tree.Node((None, 0, 0), shiftChildren(children, 0).reverse)
        case _: TreeNode.CellGroup     => Tree.Leaf((None, 0, 1))
        case TreeNode.LabelNode(label) =>

          val initOffset: Int = if (compactMarginals || children.length==1) 0 else 1
          val shifted = shiftChildren(children, initOffset)
          val endOffset = shifted.headOption.map(_.rootLabel).map(attrEndIndex).getOrElse(0)
          Tree.Node((Some(label), 0, endOffset), shifted.reverse)
      }
    }

    labelTree.scanr(histo).rootLabel
  }



}
