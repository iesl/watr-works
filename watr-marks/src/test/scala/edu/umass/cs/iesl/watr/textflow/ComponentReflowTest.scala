package edu.umass.cs.iesl.watr
package textflow

import spindex._


import watrmarks.{StandardLabels => LB}
import textboxing.{TextBoxing => TB}, TB._

object MITLabeling {

  case class Para(text: String)
  case class RawText(para: Para, start: Int, len: Int)

}


class ComponentReflowTest extends ConnectedComponentTestUtil {
  import ComponentReflow._
  import ComponentRendering._
  import TextFlow._



  it should "correctly compute string-match offsets" in {
    val ffi = 0xFB03.toChar

    s"""|
        |foo Eu1-xBixVO4 bar
        |      bbb  b  b
        |foo Eu_{1¿23;x}Bi_{x}VO_{4} bar
        |foo Fe^{+}_{x+1.0}O_{x}
        |foo ¿23;ﬂ bar
        |foo ﬂavor ﬆars e${ffi}cient bar
        |foo æon Æon bar
        |""".stripMargin

    s"""|
        |foo Eu_{1-x}Bi_{x}VO_{4} bar
        |foo Eu_{1¿23;x}Bi_{x}VO_{4} bar
        |foo Fe^{+}_{x+1.0}O_{x}
        |foo ¿23;ﬂ bar-
        |food ﬂavor ﬆars e${ffi}cient bar
        |foo æon Æon bar
        |""".stripMargin

    // val doc = toZoneIndex(...)
    // val lines = for {p <- doc.get(Page); l <- p.get(Lines) } yield l
    // val flowedLines = reflowGroupBy(lines){case (line1, line2) => dehyphenate or pad-with-space }

    // val match1 = flowedLines.findString("flavor stars")
    // val match2 = flowedLines.findString("barfood flavor")
    // val match3 = flowedLines.findRegex("[b]arfo.?d flavor")

    // val (preclipped, postclipped) = getContextForClipped(match3.get)
    // val fooMatch = preclipped.findString("foo")
    // val aeonMatch = postclipped.findString("aeon")


    // val match4 = flowedLines.findParse(ctr("bar foo Fe") ~ sup("\+").? ~ sub(".*"))
  }

  it should "join lines into single (virtual) line of text" in {
    // val (zoneIndex, lines) = createZoneIndexerAndLines(
    """|of LiFePO4 scan-
       |ning electron
       |""".stripMargin

    val (zoneIndex, visualLines) = tokenizeStringToZoneIndex(
      """|of LiFePO4 scan-
         |ning electron
         |""".stripMargin)

    val visualLineTextFlows = visualLines.map { visualLine =>
      val textFlow = VisualLine.render(visualLine).get
      val rendered = textFlow.text
      // println(VisualLine.renderRoleTree(visualLine))
      // println(s">${rendered}")
      textFlow
    }


    // // join ==> 'of LiFePO4 scanning electron'
    val line0 = foldMapTextFlow(
      visualLineTextFlows(0), {
        case (s:String, u: FlowUnit.Atom) =>
          if (s=="-") (s, FlowUnit.Rewrite(u, ""))
          else (s, u)

        case (s:String, u: FlowUnit) => (s, u)
      })


    // search for a string (regex/parse) in a reflow
    val cc0 = Content.CC(visualLines(0), line0)
    val cc1 = Content.CC(visualLines(1), visualLineTextFlows(1))
    val ccConcat = Content.Concat(Seq(cc0, cc1))

    val reflow = Reflow(ccConcat)

    val results = stringMatch("scanning", reflow)

    results.foreach { result =>
      val resultText = lineContentTextFlow(result)
      println("results======")
      println(resultText.flow.mkString("\n"))

      println("results text=======")
      println(resultText.text)
    }

    // ?? ==> 'of LiFePO4 (scanning) electron'
    //    MatchResult(rs: Seq[TargetRegion]) = Seq("scan-".targetRegion, "ning".targetRegion)
    // val m = stringMatch("scanning", joinedWithClip)



  }


  it should "join/break paragraph" in {}

  it should "grep-search virtual lines" in {}

  it should "define a repr for MIT-annots with context" in {}

}
