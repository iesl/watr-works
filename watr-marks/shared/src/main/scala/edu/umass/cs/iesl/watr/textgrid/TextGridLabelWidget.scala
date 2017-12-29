package edu.umass.cs.iesl.watr
package textgrid

import textboxing.{TextBoxing => TB}, TB._
import scalaz.{@@ => _, _}, Scalaz._

import scala.scalajs.js.annotation._
import watrmarks._
import _root_.io.circe, circe._ // circe.syntax._
import geometry._



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


@JSExportAll
sealed trait GridRegion  {
  def bounds: LTBounds
  def classes: List[String]

  def isCell(): Boolean = false
  def isHeading(): Boolean = false
  def isLabelCover(): Boolean = false
  def isLabelKey(): Boolean = false

}

@JSExportTopLevel("watr.textgrid.GridRegion")
object GridRegion {

  @JSExportAll
  case class Cell(
    cell: TextGrid.GridCell,
    row: Int, col: Int,
    override val bounds: LTBounds,
    override val classes: List[String]
  ) extends GridRegion {
    override def isCell(): Boolean = true
  }

  @JSExportAll
  case class Heading(
    heading: String,
    override val bounds: LTBounds,
    override val classes: List[String],
  ) extends GridRegion {
    override def isHeading(): Boolean = true

  }

  @JSExportAll
  case class LabelCover(
    label: Label,
    override val bounds: LTBounds,
    override val classes: List[String],
  ) extends GridRegion {
    override def isLabelCover(): Boolean = true
  }

  @JSExportAll
  case class LabelKey(
    labelIdent: String,
    override val bounds: LTBounds,
    override val classes: List[String],
  ) extends GridRegion {
    override def isLabelKey(): Boolean = true
  }

}



@JSExportTopLevel("watr.textgrid.LabelSchema")
case class LabelSchema(
  label: Label,
  abbrev: Option[(Char, Char)] = None,
  children: List[LabelSchema] = List()
) {
  def getAbbrev(): String = {
    abbrev
      .map{ case (c1, c2) => ""+c1+c2 }
      .getOrElse{
        val uppers = label.fqn.filter(_.isUpper).map(_.toLower)
        val lowers = label.fqn.filter(_.isLower)
        (uppers ++ lowers).take(2).mkString("")
      }
  }
}

@JSExportTopLevel("watr.textgrid.LabelSchemas")
case class LabelSchemas(
  schemas: List[LabelSchema]
)

@JSExportTopLevel("watr.textgrid.LabelSchemasCompanion")
@JSExportAll
object LabelSchemas {
  def labelSchemaToBox(schema: LabelSchemas): TB.Box = {

    def renderSchema(s: LabelSchema): TB.Box = {
      val lbox = s.getAbbrev.box + ": " + s.label.fqn.box

      if (s.children.nonEmpty) {
        lbox atop indent(4,
          vcat(left, s.children.map(renderSchema(_)))
        )
      } else { lbox }
    }

    vjoin(left,
      "Label Schema", indent(4,
        vjoins(
          schema.schemas.map(renderSchema(_))
        ))
    )
  }

  val jsonPrinter = circe.Printer(
    preserveOrder = true,
    dropNullValues = false,
    indent = "    ",
    lbraceRight = "\n",
    rbraceLeft = "\n",
    lbracketRight = "",
    rbracketLeft = "\n",
    lrbracketsEmpty = "",
    arrayCommaRight = " ",
    objectCommaRight = "\n",
    colonLeft = " ",
    colonRight = " "
  )

  val testLabelSchema = {

    val Authors = Label.auto
    val Author = Label.auto
    val FirstName = Label.auto
    val MiddleName = Label.auto
    val LastName = Label.auto
    val RefMarker = Label.auto
    val RefNumber = Label.auto

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

}


@JSExportTopLevel("watr.textgrid.TextGridLabelWidget") @JSExportAll
object TextGridLabelWidget {
  import circe.generic.semiauto._
  implicit val Enc_Label: Encoder[Label] = Encoder.encodeString.contramap(_.fqn)
  implicit val Dec_Label: Decoder[Label] = Decoder.decodeString.map(Label(_))
  implicit val Enc_LabelSchema: Encoder[LabelSchema] = deriveEncoder
  implicit val Enc_LabelSchemas: Encoder[LabelSchemas] = deriveEncoder

  val Indent: Int = 4

  type LabeledLines = List[(List[Label], String)]

  type LabeledRows = List[LabeledRowElem]

  type Start = Int
  type Len = Int
  type Attr = (Option[Label], Start, Len)

  implicit val ShowAttr = Show.shows[Attr]{ case(lbl, st, len) =>
    s"${lbl}: (${st}-${st+len})"
  }


  def labelTreeToMarginals(labelTree: Tree[TreeNode], compactMarginals: Boolean): MarginalGloss = {

    def shiftChildren(ch: Stream[Tree[Tree[Attr]]], init: Int) =
      ch.foldLeft(Stream.empty[Tree[Attr]]){
        case (acc, child: Tree[Tree[Attr]]) =>
          val offset = acc.headOption.map { h: Tree[Attr] => h.rootLabel._2+h.rootLabel._3 }.getOrElse(init)
          val adjusted = child.rootLabel.map{ case (nlbl, nst, nlen) => (nlbl, nst+offset, nlen) }
          adjusted #:: acc
      }

    def attrEndIndex(attr: Attr) = {
      val (_, st, len) = attr
      st+len
    }


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

    val tree = labelTree.scanr(histo).rootLabel

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

      MarginalGloss.Column(glossColumn)
    }
    MarginalGloss(columns)
  }

  def marginalGlossToTextBlock(marginalLabels: MarginalGloss): TB.Box = {

    val colBoxes = marginalLabels.columns.map{ col =>
      val colPins = col.gloss.map{ _ match {
        case MarginalGloss.VSpace(len) =>
          vspace(len)

        case MarginalGloss.Labeling(label, len) =>
          if (len==1) {
            label.fqn(0).toUpper.toString.box
          } else {
            val ch = label.fqn(0).toLower.toString()
            vjoin(ch, vjoins(List.fill(len-2)("║")), "╨")
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
              val bounds = LTBounds.Ints(x+colNum, accLen, 1, len)
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

            val cells = cells0.flatMap(_.cells).zipWithIndex
            val rowNums = cells0.map(_.gridRow).toSet
            if (rowNums.size != 1) {
              sys.error(s"more than one grid row found in label tree structure")
            }
            val rowNum = rowNums.head

            val classes = labels.map(_.fqn)

            val cellsStart = x + (Indent * (labels.length+1+depthMod))

            cells.map{ case (cell, cellCol) =>
              val left = cellsStart + cellCol
              val top = y + labeledLineNum
              val width = 1
              val height = 1

              val bounds = LTBounds.Ints(left, top, width, height)
              GridRegion.Cell(cell, rowNum, cellCol, bounds, classes)
            }

          case LabeledRowElem.HeadingRow(labels, heading) =>

            val left = x + (Indent * labels.length)
            val top = y + labeledLineNum
            val width = heading.length()
            val height = 1

            val bounds = LTBounds.Ints(left, top, width, height)
            val classes = labels.map(_.fqn)
            List(
              GridRegion.Heading(heading, bounds, classes))

        }}

    }

    def labelSchemaToGridRegions(s: LabelSchemas, x0: Int, y0: Int): Seq[GridRegion] = {

      def loop(s: LabelSchema, x: Int, y: Int): Seq[GridRegion] = {
        val labelText = s.getAbbrev + ": " + s.label.fqn
        val width = labelText.length()
        val height = 1
        val bounds = LTBounds.Ints(x, y, width, height)
        val classes = List(s.label.fqn)

        val childRegions: Seq[GridRegion] = s.children.zipWithIndex
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
    val gridRegions = labeledRowsToGridRegions(gridLines, x=originX+marginalGloss.columns.length+2, y=originY)
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
              r.copy(labels = label::labels)

            case r @ LabeledRowElem.HeadingRow(labels, heading) =>
              r.copy(labels = label::labels)
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

    // val lbox = vjoins(left, dbg.map(_._1.box))
    val rbox = vjoins(left, dbg.map(_._2))

    rbox
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
