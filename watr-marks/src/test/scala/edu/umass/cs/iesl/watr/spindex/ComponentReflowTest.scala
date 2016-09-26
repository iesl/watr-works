package edu.umass.cs.iesl.watr
package spindex

import watrmarks.{StandardLabels => LB}
import textboxing.{TextBoxing => TB}, TB._

class ComponentReflowTest extends ConnectedComponentTestUtil {
  import ComponentReflow._

  /*

   Using this test (temporarily) as a code sketch for aliging the MIT-labeled corpus with watr.

   The problem: get these records into wworks:

     |"operations" : [{ "apparatuses" : [ ], "conditions" : [ ],
     | "order" : 1
     |  "raw_texts" : [{"end_char_index" : 75,
     |      "start_char_index" : 59,
     |      "raw_text" : "co-precipitation",
     |      "paragraph_id" : "56ab77b9a1515857d1664c36" }],
     |  "_id" : "570d90aaa1515842109298bc",
     |},


   Steps:


   - Reshape the MIT annotations by creating context windows around each raw_text, to help locate that span of text in the
     Watr-extracted text. Context windows are just a few words before and after the annotated text, to help use exact
     string matching to constrain the raw_text to a position within a docseg extraction

   - align MIT-annots w/docseg by using a combo of inferred text ordering, extact string match on contexts and raw_text

      >   E.g., MIT annotation of "0.5 mol%" in context:
      >   samples was set to 0.5 mol%. The nanopowder
      >   ------------------(        )---------------
      >   - try exact string match over entire context. if exactly 1 hit, take it.
      >   - try exact string using pre and/or post context. If both pre/post match uniquely, mark middle as "hit"
      >   - try progressively shorter versions of pre- context until something is matched, then
      >     use take the one with shorted edit distance to watr-derived target position


   - Reshape docseg into a stream of SemanticTextBlocks, which correspond to visual lines,
     flowed together then re-flowed at sentence and section boundaries. (actual reflow tbd)



   - Create a visualization of each aligned raw_text, including context windows, MIT-paragraph text, Watr-paragraph text,
     PDF image clip of Watr-derived visual lines in context, and yes/no confirmation for accuracy of alignment





   */

  behavior of "component reflowing"

  it should "join lines into single (virtual) line of text" in {
    val zoneIndex = createZoneIndexer(
      """|of LiFePO4 scan-
         |ning electron
         |""".stripMargin
    )

    val visualLines = (0 to 1).flatMap{ i => labelRow(zoneIndex, i, LB.VisualLine) }

    val lineReflows: Seq[Reflow] = visualLines.map{l =>
      Reflow(Content.CC(l, "of LiFePO4 scan-"))
    }

    val row = Content.Row(lineReflows)   // ==>  'of LiFePO4 scan-ning electron'

    // ==> 'of LiFePO4 scan- ning electron'
    val joinedWithSpace = Reflow(Content.Row(Seq(
      lineReflows(0),
      Reflow(Content.Space),
      lineReflows(1)
    )))

    // ?? ==> 'of LiFePO4 scanning electron'
    val joinedWithClip = Reflow(Content.Row(Seq(
      lineReflows(0),
      lineReflows(1)
    )))

    // search for a string (regex/parse) in a reflow
    def stringMatch(s: String, r: Reflow): Unit = {}

    // ?? ==> 'of LiFePO4 (scanning) electron'
    //    MatchResult(rs: Seq[TargetRegion]) = Seq("scan-".targetRegion, "ning".targetRegion)
    val m = stringMatch("scanning", joinedWithClip)



  }

  it should "break lines into multiple lines" in {}

  it should "join/break paragraph" in {}

  it should "grep-search virtual lines" in {}

  it should "define a repr for MIT-annots with context" in {}

}
