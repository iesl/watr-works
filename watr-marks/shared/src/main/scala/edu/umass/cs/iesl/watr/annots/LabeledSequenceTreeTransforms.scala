package edu.umass.cs.iesl.watr
package annots

import scalaz.{@@ => _, _} // , Scalaz._
import watrmarks._
import _root_.io.circe, circe._, circe.syntax._
import utils.DoOrDieHandlers._
import TypeTags._

sealed trait LabelTreeNode[+A <: LabelTarget]

object LabelTreeNode {

  case class CellGroup[A <: LabelTarget](
    cells: List[A],
    beginOffset: Int
  ) extends LabelTreeNode[A]

  case class LabelNode(
    label: Label
  ) extends LabelTreeNode[Nothing]

  case object RootNode extends LabelTreeNode[Nothing]

  implicit def ShowLabelTreeNode[A <: LabelTarget](implicit ShowA: Show[A]) = Show.shows[LabelTreeNode[A]]{ treeNode =>
    treeNode match {
      case LabelTreeNode.CellGroup(cells, beginOffset) => cells.map(ShowA.shows(_)).mkString
      case LabelTreeNode.LabelNode(l) => l.fqn
      case LabelTreeNode.RootNode => "()"
    }
  }
}

object LabeledSequenceTreeTransforms {

  // def gridCellsToLabelTree(gridCells: Seq[TextGrid.GridCell]): Tree[LabelTreeNode] = {
  def labeledSequenceToLabelTree[A <: LabelTarget](labeledSequence: LabeledSequence[A]): Tree[LabelTreeNode[A]] = {
    val init = Tree.Node[LabelTreeNode[A]](LabelTreeNode.RootNode, Stream.empty)
    var currLoc = init.loc

    def up(): Unit = {
      currLoc = currLoc.parent.getOrElse(sys.error("no parent found"))
    }


    for { (cell, cellIndex) <- labeledSequence.labelTargets().zipWithIndex } {
      val pinStack = cell.pins.reverse
      val basePins = pinStack.drop(currLoc.parents.length)

      basePins.takeWhile(p => p.isBegin || p.isUnit)
        .foreach { pin =>
          val n = Tree.Node[LabelTreeNode[A]](LabelTreeNode.LabelNode(pin.label), Stream.empty)
          currLoc = currLoc.insertDownLast(n)
        }

      val maybeAppend = for {
        lastChild <- currLoc.lastChild
      } yield {
        lastChild.getLabel match {
          case prevCell@ LabelTreeNode.CellGroup(cells, beginIndex) =>

            lastChild.modifyLabel { p =>
              LabelTreeNode.CellGroup(cell :: cells, beginIndex): LabelTreeNode[A]
            }
          case _ =>
            currLoc.insertDownLast(
              Tree.Leaf[LabelTreeNode[A]](LabelTreeNode.CellGroup(List(cell), cellIndex))
            )
        }
      }

      currLoc = maybeAppend.getOrElse {
        currLoc.insertDownLast(
          Tree.Leaf[LabelTreeNode[A]](LabelTreeNode.CellGroup(List(cell), cellIndex))
        )
      }

      up()

      cell.pins.takeWhile(p => p.isLast || p.isUnit)
        .foreach { _ => up()  }
    }


    currLoc.root.toTree.map { n => n match {
      case LabelTreeNode.CellGroup(cells, beginIndex) => LabelTreeNode.CellGroup(cells.reverse, beginIndex)
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
    val (x, st, len) = attr
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

  def labelTreeToSpanTree[A <: LabelTarget](labelTree: Tree[LabelTreeNode[A]]): Tree[Attr] = {

    def histo(node: LabelTreeNode[A], children: Stream[Tree[Tree[Attr]]]): Tree[Attr] = {

      node match {
        case LabelTreeNode.RootNode         => Tree.Node((None, 0, 0), shiftChildren(children, 0).reverse)
        case cells: LabelTreeNode.CellGroup[A] => Tree.Leaf((None, 0, cells.cells.length))
        case LabelTreeNode.LabelNode(label) =>
          val shifted = shiftChildren(children, 0)
          val endOffset = shifted.headOption.map(_.rootLabel).map(attrEndIndex).getOrElse(0)
          Tree.Node((Some(label), 0, endOffset), shifted.reverse)
      }
    }

    labelTree.scanr(histo).rootLabel
  }

  def labelTreeToMarginalSpanTree[A <: LabelTarget](labelTree: Tree[LabelTreeNode[A]], compactMarginals: Boolean = true): Tree[Attr] = {

    def histo(node: LabelTreeNode[A], children: Stream[Tree[Tree[Attr]]]): Tree[Attr] = {

      node match {
        case LabelTreeNode.RootNode         => Tree.Node((None, 0, 0), shiftChildren(children, 0).reverse)
        case _: LabelTreeNode.CellGroup[A]     => Tree.Leaf((None, 0, 1))
        case LabelTreeNode.LabelNode(label) =>

          val initOffset: Int = if (compactMarginals || children.length==1) 0 else 1
          val shifted = shiftChildren(children, initOffset)
          val endOffset = shifted.headOption.map(_.rootLabel).map(attrEndIndex).getOrElse(0)
          Tree.Node((Some(label), 0, endOffset), shifted.reverse)
      }
    }

    labelTree.scanr(histo).rootLabel
  }



}
