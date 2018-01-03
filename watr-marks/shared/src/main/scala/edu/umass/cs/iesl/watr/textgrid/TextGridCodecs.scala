package edu.umass.cs.iesl.watr
package textgrid

import scala.collection.mutable
import geometry._
import TypeTags._

import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._

import watrmarks._

case class TextGridSerialization(
  lineMap: Map[Int, (Json, String)],
  labelMap: Map[Label, Int],
  cellLabelBuffer: List[List[String]]

)

class TextOutputBuilder(textGrid: TextGrid) {

  def getSerialization(): TextGridSerialization = {
    val codecs = new AccumulatingTextGridCodecs(textGrid.stableId)

    textGrid.rows.zipWithIndex
      .foreach{ case (row, rowi) =>
        row.serialize(codecs)
      }

    TextGridSerialization(
      codecs.lineMap.toMap,
      codecs.labelMap.toMap,
      codecs.cellLabelBuffer.toList
    )
  }

  def gridToJson(): Json = {
    val serProps = getSerialization()
    val lineNums = serProps.lineMap.keys.toList.sorted
    val labelMapJson = serProps.labelMap// .asJson
    val cellLabelsJson = serProps.cellLabelBuffer.asJson

    val labelDefs = Json.obj(
      "labelMap" := serProps.labelMap.map{case (k, v) => (v, k.fqn)},
      "cellLabels" := serProps.cellLabelBuffer
    )


    val textAndLoci = lineNums.map { lineNum =>
      val text = serProps.lineMap(lineNum)._2
      val loci = serProps.lineMap(lineNum)._1
      Json.obj(
        "text" := text,
        "loci" := loci,
      )
    }

    Json.obj(
      "stableId" := textGrid.stableId.unwrap,
      "rows" := textAndLoci,
      "labels" := labelDefs
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
  val labelMap = mutable.Map[Label, Int]()
  val labelIdMap = mutable.Map[Int, Label]()
  val cellLabelBuffer = mutable.ArrayBuffer[List[String]]()

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
    val serLabelMap = cursor.downField("labels").downField("labelMap").as[Map[Int, String]]
    serLabelMap.fold(fail => {
      sys.error(s"could not decode textgrid loci:${fail} ")
    }, succ => {
      labelIdMap ++= succ.toList.map{ case (k, v) => (k, Label(v)) }
    })
    val cellLabels = cursor.downField("labels").downField("cellLabels").as[List[List[String]]]
    cellLabels.fold(fail => {
      sys.error(s"could not decode textgrid loci:${fail} ")
    }, succ => {
      cellLabelBuffer.appendAll(succ)
    })

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
            val cell = TextGrid.PageItemCell(atoms.head, atoms.tail, atoms.head.char.head)
            val cellPinRepr = cellLabelBuffer.remove(0)
            cellPinRepr.foreach { pinRep =>
              val Array(pinChar, labelId) = pinRep.split(":")
              val label = labelIdMap.get(labelId.toInt).getOrElse {
                sys.error(s"could not decode label ${labelId}")
              }
              val pin = pinChar match {
                case "B" => label.B
                case "I" => label.I
                case "O" => label.O
                case "L" => label.L
                case "U" => label.U
                case _ =>
                  sys.error(s"could not decode label pinChar ${pinChar}")
              }
              cell.addPin(pin)
            }

            cell
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

              val cell = TextGrid.InsertCell(char.head, insertAt: PageRegion)
              val cellPinRepr = cellLabelBuffer.remove(0)
              cellPinRepr.foreach { pinRep =>
                val Array(pinChar, labelId) = pinRep.split(":")
                val label = labelIdMap.get(labelId.toInt).getOrElse {
                  sys.error(s"could not decode label ${labelId}")
                }
                val pin = pinChar match {
                  case "B" => label.B
                  case "I" => label.I
                  case "O" => label.O
                  case "L" => label.L
                  case "U" => label.U
                  case _ =>
                    sys.error(s"could not decode label pinChar ${pinChar}")
                }
                cell.addPin(pin)
              }
              cell
            }
        }

        res.getOrElse { Left(DecodingFailure("insert grid cell decoding error", List.empty)) }

      case x => Left(DecodingFailure(s"unknown grid cell type ${x}", List.empty))
    }

  }

  val labelIdGen = utils.IdGenerator[LabelID]()

  implicit def GridCellEncoder: Encoder[TextGrid.GridCell] = Encoder.instance[TextGrid.GridCell]{ _ match {
    case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>

      val cellLabels = cell.pins.reverse.toList.map{ pin =>
        val labelId = labelMap.getOrElseUpdate(pin.label, labelIdGen.nextId.unwrap)
        s"${pin.pinChar}:${labelId}"
      }
      cellLabelBuffer.append(cellLabels)

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
      val cellLabels = cell.pins.reverse.toList.map{ pin =>
        val labelId = labelMap.getOrElseUpdate(pin.label, labelIdGen.nextId.unwrap)
        s"${pin.pinChar}:${labelId}"
      }
      cellLabelBuffer.append(cellLabels)


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
