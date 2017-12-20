package edu.umass.cs.iesl.watr
package textgrid

import scala.collection.mutable
import watrmarks._
import geometry._
import geometry.syntax._
import geometry.PageComponentImplicits._
import textboxing.{TextBoxing => TB}, TB._
import TypeTags._

import utils.SlicingAndDicing._

import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._

sealed trait FontInfo

case object NoFonts extends FontInfo

trait TextGrid {
  import TextGrid._

  def stableId: String@@DocumentID

  def rows: Seq[Row]

  def toText(): String = {
    rows.map(_.toText).mkString("\n  ", "\n  ", "\n")
  }

  def splitOneLeafLabelPerLine(): TextGrid = {
    val splitRows = rows.flatMap { row =>
      row.splitOnLeafLabels()
    }

    TextGrid.fromRows(stableId, splitRows)
  }

  def indexedCells(): Seq[(GridCell, Int, Int)] = {
    for {
      (row, rowNum) <- rows.zipWithIndex
      (cell, colNum) <- row.cells.zipWithIndex
    } yield { (cell, rowNum, colNum) }
  }

  def split(row: Int, col: Int): Option[TextGrid] = {
    if (0 <= row && row < rows.length) {
      rows(row).split(col).map {
        case (row1, row2) =>
          val (pre, post) = rows.splitAt(row)
          val end = row1 +: row2 +: (post.drop(1))
          val newRows = pre ++ end
          TextGrid.fromRows(stableId, newRows)
      }
    } else None
  }

  def slurp(row: Int): Option[TextGrid] = {
    if (0 <= row && row < rows.length-1) {
      val (pre, post) = rows.splitAt(row)
      val r1 = pre.last
      val r2 = post.head
      val r12 = r1.append(r2)
      val newRows = pre.dropRight(1) ++ (r12 +: post.drop(1))
      Some(TextGrid.fromRows(stableId, newRows))
    } else None
  }

  def buildOutput() = new TextOutputBuilder(this)

  def pageBounds(): Seq[PageRegion] = {

    val allBounds = rows.flatMap{ row => row.pageBounds() }

    val regionsByPages = allBounds.groupBy(_.page.pageNum)
    regionsByPages.map { case (pageNum, pageRegions) =>
      val headRegion = pageRegions.head.page
      val pageBbox = pageRegions.map(_.bbox).reduce(_ union _)
      PageRegion(
        headRegion,
        pageBbox
      )
    }.toList
  }

  def toJson(): Json = {
    new TextOutputBuilder(this).gridToJson()
  }
}

object TextGrid {
  // type SetType[A] = mutable.Set[A]
  type SetType[A] = mutable.ArrayStack[A]
  type PinSet = SetType[BioPin]


  def fromJson(js: Json): TextGrid = {
    val cursor = js.hcursor

    cursor.downField("stableId").as[String].fold(fail => {
      sys.error(s" could not decode textgrid: ${fail}")
    }, stableId => {
      val codecs =  new AccumulatingTextGridCodecs(DocumentID(stableId))
      codecs.decodeGrid(js)
    })

  }

  sealed trait LabelTarget {
    val pins: PinSet = mutable.ArrayStack[BioPin]()

    def labels: SetType[Label] = pins.map(_.label)

    def addPin(p: BioPin): Unit = pins.push(p)

    def addLabel(l: Label): Unit = addPin(l.U)

    def removeLabel(l: Label): Unit = {
      if (pins.contains(l)) {
        while(pins.top.label != l) {
          pins.pop()
        }
      }
    }

    def hasLabel(l: Label): Boolean = {
      pins.exists(_.label == l)
    }

    def hasPin(p: BioPin): Boolean = {
      pins.contains(p)
    }

    def topLabel(): Option[Label] = {
      if (pins.nonEmpty) {
        Some(pins.top.label)
      } else None
    }

    def topPin(): Option[BioPin] = {
      if (pins.nonEmpty) {
        Some(pins.top)
      } else None
    }

    def showPinsVert(): Box = {
      vjoins(left, pins.toList.reverse.map(_.pinChar.toString.box))
    }
  }

  sealed trait GridCell extends LabelTarget {
    def pageRegion: PageRegion

    def char: Char

    def fonts: FontInfo = NoFonts

    def createInsert(ch: Char): InsertCell = InsertCell(ch, this.pageRegion)

    def showCell(): Box = {
      vjoin(left, char.toString(), showPinsVert())
    }
  }

  def mbrRegionFunc(h: PageItem, tail: Seq[PageItem]): PageRegion = {
    (h +: tail).map(_.pageRegion).reduce { _ union _ }
  }

  case class PageItemCell(
    headItem: PageItem,
    tailItems: Seq[PageItem] = Seq(),
    override val char: Char,
    regionFunc: (PageItem, Seq[PageItem]) => PageRegion = mbrRegionFunc(_, _)
  ) extends GridCell {
    override val pageRegion: PageRegion = regionFunc(headItem, tailItems)
  }


  case class InsertCell(
    char: Char,
    insertAt: PageRegion
  ) extends GridCell {
    override val pageRegion: PageRegion = insertAt
  }


  trait Row extends LabelTarget {

    def cells: Seq[GridCell]

    def split(col: Int): Option[(Row, Row)] = {
      if (0 < col && col < cells.length-1) {
        val (c1, c2) = cells.splitAt(col)
        Some((
          Row.fromCells(c1), Row.fromCells(c2)
        ))
      } else None
    }


    def splitOnLeafLabels(): Seq[Row] = {
      val groups = cells.groupByPairs((a, b) => {
        (a.topPin(), b.topPin()) match {
          case (Some(pin1), Some(pin2)) =>
            val isBIL = pin1.isBegin && (pin2.isInside || pin2.isLast)
            val isIIL = pin1.isInside && (pin2.isInside || pin2.isLast)
            val sameLabel = pin1.label == pin2.label
            sameLabel && (isBIL || isIIL)

          case (None, None) => true
          case _ => false
        }})


      groups.map{ group =>
        val r = Row.fromCells(group)
        r.cells.head.topPin().foreach { pin =>
          r.addLabel(pin.label)
        }
        r
      }
    }

    def append(row: Row): Row = {
      Row.fromCells(cells ++ row.cells)
    }

    def pageBounds(): Seq[PageRegion] = {
      val regionsByPages = cells.groupBy(_.pageRegion.page.pageNum)

      regionsByPages.map { case (pageNum, pageRegions) =>
        val headRegion = pageRegions.head.pageRegion
        val pageBbox = pageRegions.map(_.pageRegion.bbox).reduce(_ union _)
        PageRegion(
          headRegion.page,
          pageBbox
        )
      }.toList
    }

    def toCursor(): Option[GridCursor] = {
      GridCursor.init(cells.toList.toZipper)
    }

    def foreach(f: GridCell => Unit): Unit  = {
      cells.foreach(f(_))
    }


    def toText(): String = {
      cells.map(_.char).mkString("")
    }

    def showRow(): Box = {
      hcat(top, cells.map(_.showCell()))
    }

    protected[textgrid] def serialize(codecs: AccumulatingTextGridCodecs): Unit = {
      codecs.encodeRow(this)
    }
  }

  abstract class MutableRow extends Row {
    override val cells: mutable.ArrayBuffer[GridCell] = mutable.ArrayBuffer()
  }


  object Row {
    def fromCells(init: Seq[GridCell]): Row = new MutableRow {
      cells.appendAll(init)
    }
  }

  abstract class MutableTextGrid extends TextGrid {
    override val rows: mutable.ArrayBuffer[Row] = mutable.ArrayBuffer()
  }

  def fromRows(id: String@@DocumentID, init: Seq[Row]): TextGrid = new MutableTextGrid {
    override val stableId = id
    rows.appendAll(init)
  }

  def fromCells(stableId: String@@DocumentID, init: Seq[GridCell]): TextGrid =
    fromRows(stableId, Seq(Row.fromCells(init)))

}

case class TextGridSerialization(
  lineMap: Map[Int, (Json, String)]
)

class TextOutputBuilder(textGrid: TextGrid) {

  def getSerialization(): TextGridSerialization = {
    val codecs = new AccumulatingTextGridCodecs(textGrid.stableId)

    textGrid.rows.zipWithIndex
      .foreach{ case (row, rowi) =>
        row.serialize(codecs)
      }

    TextGridSerialization(
      codecs.lineMap.toMap
    )
  }

  def gridToJson(): Json = {
    val serProps = getSerialization()
    val lineNums = serProps.lineMap.keys.toList.sorted

    val textAndLoci = lineNums.map { lineNum =>
      val text = serProps.lineMap(lineNum)._2
      val loci = serProps.lineMap(lineNum)._1
      Json.obj(
        "text" := text,
        "loci" := loci
      )
    }

    Json.obj(
      "stableId" := textGrid.stableId.unwrap,
      "rows" := textAndLoci
    )
  }

  def getEnrichedTextOutput(): String = {
    val serProps = getSerialization()

    val lineNums = serProps.lineMap.keys.toList.sorted

    val textAndLoci = lineNums.map { lineNum =>
      val text = serProps.lineMap(lineNum)._2
      val loci = serProps.lineMap(lineNum)._1
      (text, loci)
    }

    val textBlock = textAndLoci.zipWithIndex.map{ case ((text, _), i) =>
      val linenum = "%04d".format(i)
      s"${linenum}>  ${text}"
    }
    val lociBlock = textAndLoci.zipWithIndex.map{ case ((_, loci), i) =>
      val linenum = "%04d".format(i)
      s"${linenum}: ${loci}"
    }

    val allLines = textBlock ++ List("##", "##") ++ lociBlock
    allLines.mkString("\n  ", "\n  ", "\n")
  }


}

protected class AccumulatingTextGridCodecs(stableId: String@@DocumentID) {

  val pageIdMap = mutable.Map[Int@@PageID, (String@@DocumentID, Int@@PageNum)]()
  val lineMap = mutable.Map[Int, (Json, String)]()

  def nextLineNum: Int = if (lineMap.keySet.isEmpty) 0 else lineMap.keySet.max + 1

  def decodeGlyphCells: Decoder[Seq[(String, Int, (Int, Int, Int, Int))]] = Decoder.instance { c =>
    c.as[(Seq[(String, Int, (Int, Int, Int, Int))])]
  }

  def decodeGlyphCell: Decoder[(String, Int, (Int, Int, Int, Int))] = Decoder.instance { c =>
    c.as[(String, Int, (Int, Int, Int, Int))]
  }

  def encodeRow(row: TextGrid.Row): Unit = {
    val rowAsJson = row.cells.map(c => c.asJson).asJson
    val lineNum = nextLineNum
    lineMap.put(lineNum, (rowAsJson, row.toText))
  }

  def encodeCell(c: TextGrid.GridCell): Json = {
    c.asJson
  }

  def decodeCell(c: Json):  TextGrid.GridCell = {
    c.as[TextGrid.GridCell].fold(decFail => {
      sys.error(s"decode fail ${decFail}")
    }, succ => succ)
  }

  def decodeGrid(js: Json): TextGrid = {
    val cursor = js.hcursor

    val rowsM = cursor
      .downField("rows").values.map { jsVals =>
        jsVals.toVector.map { js =>
          val lociM = js.hcursor.downField("loci").as[Seq[TextGrid.GridCell]]
          lociM.fold(fail => {
            sys.error(s"could not decode textgrid loci:${fail}: js=${js}")
          }, succ => {
            TextGrid.Row.fromCells(succ)
          })
        }
      }

    val rows = rowsM.getOrElse {
      sys.error(s"could not decode textgrid rows")
    }

    TextGrid.fromRows(stableId,  rows)
  }

  implicit def decodeGridCell: Decoder[TextGrid.GridCell] = Decoder.instance { c =>

    c.keys.map(_.toVector) match {
      case Some(Vector("g")) =>
        val res = c.downField("g").focus.map{ json =>
          val dec = decodeGlyphCells.decodeJson(json).map { cells =>
            val atoms = cells.map{ case(char, page, (l, t, w, h)) =>
              val bbox = LTBounds.IntReps(l, t, w, h)
              CharAtom(
                CharID(-1),
                PageRegion(
                  StablePage(
                    stableId,
                    PageNum(page)
                  ),
                  bbox
                ),
                char.toString()
              )
            }

            TextGrid.PageItemCell(atoms.head, atoms.tail, atoms.head.char.head)
          }

          dec.fold(decFail => {
            Left(decFail)
          }, succ => {
            Right(succ)
          })
        }

        res.getOrElse { Left(DecodingFailure("page item grid cell decoding error", List.empty)) }


      case Some(Vector("i")) =>
        val res = c.downField("i").focus.map{ json =>
          decodeGlyphCell.decodeJson(json)
            .map { case(char, page, (l, t, w, h)) =>
              val bbox = LTBounds.IntReps(l, t, w, h)

              val insertAt = PageRegion(
                StablePage(
                  stableId,
                  PageNum(page)
                ),
                bbox
              )

              TextGrid.InsertCell(char.head, insertAt: PageRegion)
            }
        }

        res.getOrElse { Left(DecodingFailure("insert grid cell decoding error", List.empty)) }

      case x => Left(DecodingFailure(s"unknown grid cell type ${x}", List.empty))
    }

  }

  implicit def GridCellEncoder: Encoder[TextGrid.GridCell] = Encoder.instance[TextGrid.GridCell]{ _ match {
    case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
      val items = (headItem +: tailItems).map{ pageItem =>
        val page = pageItem.pageRegion.page
        val pageNum = page.pageNum

        if (!pageIdMap.contains(page.pageId)) {
          pageIdMap.put(page.pageId, (page.stableId, page.pageNum))
        }

        val LTBounds.IntReps(l, t, w, h) = pageItem.bbox
        Json.arr(
          Json.fromString(char.toString()),
          Json.fromInt(pageNum.unwrap),
          Json.arr(Json.fromInt(l), Json.fromInt(t), Json.fromInt(w), Json.fromInt(h))
        )
      }

      Json.obj(
        "g" := items
      )

    case cell@ TextGrid.InsertCell(char, insertAt)     =>

      val pageNum = insertAt.page.pageNum
      val LTBounds.IntReps(l, t, w, h) = insertAt.bbox

      // json"""{"i": [${char}, ${pageNum.unwrap}, [$l, $t, $w, $h]]}"""
      val jsonRec = Json.arr(
        Json.fromString(char.toString()),
        Json.fromInt(pageNum.unwrap),
        Json.arr(Json.fromInt(l), Json.fromInt(t), Json.fromInt(w), Json.fromInt(h))
      )
      Json.obj(
        "i" := jsonRec
      )
  }}

}

object TextGridFunctions {
  import scalaz.{@@ => _, _} , Scalaz._

  sealed trait TreeNode

  object TreeNode {
    case class CellNode(
      cell: TextGrid.GridCell
    ) extends TreeNode

    case class LabelNode(
      label: Label
    ) extends TreeNode

    case object UnlabeledNode extends TreeNode

  }

  def bioPinsToRoseTreeNewVer(textGrid: TextGrid): Tree[TreeNode] = {
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
          // val n = Tree.Node(pin.label.fqn, Stream.empty)
          val n = Tree.Node[TreeNode](TreeNode.LabelNode(pin.label), Stream.empty)
          currLoc = currLoc.insertDownLast(n)
        }

      val leaf = Tree.Leaf[TreeNode](TreeNode.CellNode(cell))
      currLoc = currLoc.insertDownLast(leaf)

      up()

      cell.pins.takeWhile(p => p.isLast || p.isUnit)
        .foreach { _ => up()  }
    }

    currLoc.root.toTree

  }

  def bioPinsToRoseTree(textGrid: TextGrid): Tree[String] = {
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
