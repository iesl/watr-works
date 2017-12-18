package edu.umass.cs.iesl.watr
package textgrid


import corpora._
import TypeTags._
import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._
import textboxing.{TextBoxing => TB}, TB._

class TextGridRenderingTests extends TextGridTestUtil {
  override val docStore: DocumentZoningApi = new MemDocZoningApi

  val docs = List(
    List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "rst\nuvw\nxyz"
    ),
    List(
      "exit-\ning\n",
      "cellar\ndoor\n",
      "close-\nup\n"
    )
  )

  for { (doc, i) <- docs.zipWithIndex } {
    addDocument(DocumentID(s"doc#${i}"), doc)
  }

  val jsonLabelSpec = {
    Json.obj(
      "label" := "Affiliations",
      "children" := Seq(
        Json.obj(
          "label" := "Authors",
          "children" := Seq(
            Json.obj(
              "label" := "Author"
            )
          )
        )
      )
    )
  }

  val inlineSpec = {
    // j: Journal
    // s: Authors
    // n: Name
    // f/m/l: First/Middle/LastName

    """|
       |>     |1.
       |>snL  |Bishop-Clark
       |>║║   |,
       |>║╨F  |C.
       |>║    |and
       |>║nL  |Wheeler
       |>║║   |,
       |>║╨F  |D.
       |>║    |and
       |>╨N   |Boehm, B.W.
       |>J    |; Software Engineering Economics. Prentice-Hall
       |"""
  }






  // def makeRuler(str: String): String = {
  //   val height = str.length.toString.length
  //   val topRulerList = (height to 2 by -1).map(level => {
  //     (bb.startIndex until next).map(i => {
  //       if (i == bb.startIndex || (i % 10) == 0){
  //         val divisor = Math.pow(10,level).toInt
  //         val digit = (i % divisor)/(divisor/10)
  //         if (digit == 0 && level == height) " " else digit
  //       } else " "
  //     }).mkString("")+"|"
  //   })
  //   val bottomRuler = "| |" + (bb.startIndex until next).map(_ % 10).mkString("") + "|"
  //   val ruler = (topRulerList :+ bottomRuler).mkString("\n")
  //   "\n" + bb.annotationMap.values.toList.reverse.distinct.map(renderAnnotation(_, (next - bb.startIndex))).mkString("\n") + "\n" + ruler + "\n "
  // }



  val indentedRendering = {
    """|
       |1.
       |Bishop-Clark,  C. and Wheeler, D. and Boehm, B.W.; Software Engineering Economics. Prentice-Hall
       |    Bishop-Clark,  C.
       |        Bishop-Clark
       |        ,
       |        C.
       |    and
       |    Wheeler, D.
       |        Wheeler
       |        ,
       |        D.
       |    and
       |    Boehm, B.W.
       |; Software Engineering Economics. Prentice-Hall
       |""".stripMargin
  }


  case class LineRenderInfo(
    text: String,
    indent: Int,
    isHoverable: Boolean,
    canSplit: Boolean,
    canJoin: Boolean,
    canLabel: Boolean
  )
  import watrmarks.Label

  val Authors = Label.auto
  val Author = Label.auto
  val LastName = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val Journal = Label.auto
  val RefMarker = Label.auto

  val labelSpans = List(
    ((0, 3),   RefMarker),
    ((3, 53),  Authors),
    ((3, 20),  Author),
    ((3, 15),  LastName),
    ((18, 20), FirstName),
    ((25, 36), Author),
    ((41, 52), Author),
    ((54, 99), Journal)
  )
  val unlabeledText = {
    //"0         1         2         3         4         5         6         7         8         9
    // 0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789 """
    """1. Bishop-Clark,  C. and Wheeler, D. and Boehm, B.W.; Software Engineering Economics. Prentice-Hall"""

  }


  it should "render to a textgrid labeling widget" in {
    val stableId = DocumentID("docXX")

    val textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)

    println(textGrid.toText())


    val row0 = textGrid.rows.head.toCursor.get

    val labeledCursor = labelSpans.foldLeft(row0) {case (accCur, ((start, end), label)) =>
      val win = accCur.move(start)
        .get.toWindow
        .slurpRight({ case (window, next) => window.length < end-start })

      win.addLabel(label)
      win.toLastCursor.start
    }

    val labeledRow = labeledCursor.toRow
    println("row:")
    println(labeledRow.showRow())
    println("\n\n")
    val textGrid2 = TextGrid.fromRows(stableId, Seq(labeledRow))
    val splitGrid = textGrid2.splitOneLeafLabelPerLine()

    // splitGrid.rows.foreach{ row =>
    //   println("-------------------")
    //   println(row.showRow())
    //   println()
    // }

    val renderedWidgetBlock = splitGrid.rows
      .zipWithIndex.flatMap { case (row, rowNum) =>
        val rowText = row.toText()
        val rowPins = row.pins.toList
        val textIndent = rowPins.filterNot(_.isUnit).length
        val headerPins = rowPins.filter(_.isBegin)

        val thisRow = LineRenderInfo(
          rowText,
          textIndent,
          isHoverable = true,
          canSplit = false,
          canJoin = false,
          canLabel = false
        )

        val headerLines = headerPins.zipWithIndex.map{ case (pin, pinNum) =>
          val hdr = textGrid.rows.drop(rowNum)
            .takeWhile{ r => r.hasPin(pin.label.I) }
            .take(1)

          val text = hdr.map(_.toText()).mkString(" ")

          LineRenderInfo(
            text,
            pinNum,
            isHoverable = false,
            canSplit = false,
            canJoin = false,
            canLabel = false
          )
        }

        headerLines :+ thisRow
      }

    val lines = renderedWidgetBlock.map{ renderInfo =>
      indent(renderInfo.indent, renderInfo.text)
    }

    val block = vcat(left, lines)

    println(block.toString)

  }

}
