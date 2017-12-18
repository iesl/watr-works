package edu.umass.cs.iesl.watr
package textgrid

import scala.collection.mutable
import watrmarks._
import geometry._
import geometry.syntax._
import geometry.PageComponentImplicits._
import textboxing.{TextBoxing => TB}, TB._
import TypeTags._

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
  type SetType[A] = mutable.Set[A]
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

  sealed trait GridCell {
    def pageRegion: PageRegion

    def char: Char

    val pins: PinSet = mutable.HashSet[BioPin]()

    def fonts: FontInfo = NoFonts

    def labels: SetType[Label] = pins.map(_.label)

    def addPin(p: BioPin): Unit = pins.add(p)

    def addLabel(l: Label): Unit = addPin(l.U)

    def removeLabel(l: Label): Unit = {
      pins.retain(_.label != l)
    }

    def createInsert(ch: Char): InsertCell = InsertCell(ch, this.pageRegion)

    def hasLabel(l: Label): Boolean = {
      pins.exists(_.label == l)
    }

    def hasPin(p: BioPin): Boolean = {
      pins.contains(p)
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


  trait Row {

    def cells: Seq[GridCell]

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

  // def withLineNumbering(): Unit = {}
  // def withMarginLabels(): Unit = {}
  // def withTextLocations(): Unit = {}

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
