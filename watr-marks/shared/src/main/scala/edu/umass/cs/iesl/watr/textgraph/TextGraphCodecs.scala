package edu.umass.cs.iesl.watr
package textgraph

import geometry._
import TypeTags._

import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._


import utils.DoOrDieHandlers._
import scala.collection.mutable

class TextGraphOutputBuilder(textGraph: TextGraph) {

  def getSerialization(): AccumulatingTextGraphCodecs = {
    val codecs = new AccumulatingTextGraphCodecs(textGraph.stableId)

    // textGraph.getMatrixContent().foreach{ content =>
    //   content
    // }
    // textGraph.rows.zipWithIndex
    //   .foreach{ case (row, rowi) =>
    //     row.serialize(codecs)
    //   }
    codecs
  }

  def gridToJson(): Json = {
    val codec = getSerialization()
    val lineNums = codec.lineMap.keys.toList.sorted


    var totalOffset = 0
    val textAndLoci = lineNums.map { lineNum =>
      val text = codec.lineMap(lineNum)._2
      val loci = codec.lineMap(lineNum)._1

      val currOffset = totalOffset
      totalOffset += text.length()

      Json.obj(
        "offset" := currOffset,
        "text" := text,
        "loci" := loci,
      )
    }

    // val bioLabelJson = LabeledSequenceCodecs.encodeBioLabels(
    //   textGraph.indexedCells().map(_._1)
    // )


    val labelDefs = Json.obj(
      "cellLabels" := Json.obj() // bioLabelJson
    )
    Json.obj(
      "stableId" := textGraph.stableId.unwrap,
      "rows" := textAndLoci,
      "labels" := labelDefs
    )
  }

}


protected class AccumulatingTextGraphCodecs(stableId: String@@DocumentID) {

  val lineMap = mutable.Map[Int, (Json, String)]()

  def decodeGraph(js: Json): TextGraph = {
    val cursor = js.hcursor

    // val rowsM = cursor
    //   .downField("rows").values.map { jsVals =>
    //     jsVals.toVector.map { js =>
    //       val lociM = js.hcursor.downField("loci").as[Seq[TextGraph.GridCell]]
    //       lociM.fold(fail => {
    //         sys.error(s"could not decode textgrid loci:${fail}: js=${js}")
    //       }, succ => {
    //         TextGraph.Row.fromCells(succ)
    //       })
    //     }
    //   }

    // val rows = rowsM.orDie(s"could not decode textgrid rows")

    // val textGraph = TextGraph.fromRows(stableId,  rows)
    // for {
    //   l <- cursor.downField("labels").success
    //   m <- l.downField("cellLabels").success
    //   focus <- m.focus
    // } yield {
    //   LabeledSequenceCodecs.decodeAndApplyBioLabels(focus, textGraph.cellLabels)
    // }
    // textGraph
    ???
  }

  private def nextLineNum: Int = if (lineMap.keySet.isEmpty) 0 else lineMap.keySet.max + 1

  private def decodeGlyphCells: Decoder[Seq[(String, Int, (Int, Int, Int, Int))]] = Decoder.instance { c =>
    c.as[(Seq[(String, Int, (Int, Int, Int, Int))])]
  }

  private def decodeGlyphCell: Decoder[(String, Int, (Int, Int, Int, Int))] = Decoder.instance { c =>
    c.as[(String, Int, (Int, Int, Int, Int))]
  }

  // protected[textgrid] def encodeRow(row: TextGraph.Row): Unit = {
  //   val rowAsJson = row.cells.map(c => c.asJson).asJson
  //   val lineNum = nextLineNum
  //   lineMap.put(lineNum, (rowAsJson, row.toText))
  // }


  // private def addBioPins(cell: TextGraph.GraphCell): TextGraph.GraphCell = {
  //   if (cellLabelBuffer.nonEmpty) {
  //     val cellPinRepr = cellLabelBuffer.remove(0)
  //     cellPinRepr.foreach { pinRep =>
  //       val Array(pinChar, labelId) = pinRep.split(":")
  //       val label = labelIdMap.get(labelId.toInt)
  //         .orDie(s"could not decode label ${labelId}")

  //       val pin = pinChar match {
  //         case "B" => label.B
  //         case "I" => label.I
  //         case "O" => label.O
  //         case "L" => label.L
  //         case "U" => label.U
  //         case _ =>
  //           sys.error(s"could not decode label pinChar ${pinChar}")
  //       }
  //       cell.addPin(pin)
  //     }
  //   }
  //   cell
  // }


  implicit def decodeGraphCell: Decoder[TextGraph.GridCell] = Decoder.instance { c =>

    c.keys.map(_.toVector) match {
      case Some(Vector("g")) =>

        val res = c.downField("g").focus.map{ json =>
          val dec = decodeGlyphCells.decodeJson(json).map { cells =>
            val atoms = cells.map{ case(char, page, (l, t, w, h)) =>
              val bbox = LTBounds.IntReps(l, t, w, h)
              PageItem.CharAtom(
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
            TextGraph.GlyphCell(atoms.head.char.head, atoms.head, atoms.tail)
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

              TextGraph.InsertCell(char.head)
            }
        }

        res.getOrElse { Left(DecodingFailure("insert grid cell decoding error", List.empty)) }

      case x => Left(DecodingFailure(s"unknown grid cell type ${x}", List.empty))
    }

  }

  val labelIdGen = utils.IdGenerator[LabelID]()

  implicit def GraphCellEncoder: Encoder[TextGraph.GridCell] = Encoder.instance[TextGraph.GridCell]{ _ match {
    case cell@ TextGraph.GlyphCell(char, headItem, tailItems) =>

      val items = (headItem +: tailItems).map{ pageItem =>
        val page = pageItem.pageRegion.page
        val pageNum = page.pageNum

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

    case cell@ TextGraph.InsertCell(char)     =>
      Json.obj(
        "i" := List(char.toString())
      )

  }}

}
