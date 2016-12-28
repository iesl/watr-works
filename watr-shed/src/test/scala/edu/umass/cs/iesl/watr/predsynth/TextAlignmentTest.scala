package edu.umass.cs.iesl.watr
package spindex


// import watrmarks.{StandardLabels => LB}
// import textboxing.{TextBoxing => TB}, TB._

class MITAlignLabelTask extends ConnectedComponentTestUtil {

  /*

   Using this test (temporarily) as a code sketch for aligning the MIT-labeled corpus with watr.


   Steps: (using agrep)

   - Create a one-line representation of the text of a pdf via watr, by joining all visual lines into one line


   - Create a table for each paper like so:
      paras: [
        {id: 0, text 'sdfj lsd'}
        {id: 0, text 'sdfj lsd'}
      ]
      rawTexts: [
        {grpId: 23, paraId: paragraph1, start:20, end:30, type:'entity'}
        {grpId: 23, paraId: paragraph1, start:20, end:30, type:'entity/amount'}
        {grpId: 23, paraId: paragraph1, start:20, end:30, type:'entity/entdescriptor'}

        {grpId: 23, paraId: paragraph1, start:20, end:30, type:'operation' order:3}
        {grpId: 23, paraId: paragraph1, start:20, end:30, type:'operation/condition'}
      ]

   - foreach rawText:
     - create a 'context window' of text + ~10 chars
     - minimize # of context window hits
       - agrep the context window through watr-created one-liner
       - if context window is found, agrep for rawText within window
       - if no context window is found, narrow context window and re-try
       - if multiple windows are found, increase context windo and re-try
     - use the agrep match index to get the corresponding text flow chars and bounds
     - label (at the char-level) the bounds corresponding to the rawText




   - align MIT-annots w/docseg by using a combo of inferred text ordering, extact string match on contexts and raw_text

      >   E.g., MIT annotation of "0.5 mol%" in context:
      >   samples was set to 0.5 mol%. The nanopowder
      >   ------------------(        )---------------
      >   - try exact string match over entire context. if exactly 1 hit, take it.
      >   - try exact string using pre and/or post context. If both pre/post match uniquely, mark middle as "hit"
      >   - try progressively shorter versions of pre- context until something is matched, then
      >     use take the one with shortest edit distance to watr-derived target position


   - Reshape docseg into a stream of semantically meaningful line, e.g., visual lines
     flowed together then re-flowed at sentence and section boundaries. (actual reflow tbd)



   - Create a visualization of each aligned raw_text, including context windows, MIT-paragraph text, Watr-paragraph text,
     PDF image clip of Watr-derived visual lines in context, and yes/no confirmation for accuracy of alignment


   */

  behavior of "mit annotation alignment"

  val sampleText =
  """|
     |          EXPERIMENTAL
     |
     |1. Sample Preparation and Characterization
     |
     |   The starting material of NaBiO3 ? nH2O (Nacalai Tesque
     |Inc.) was placed in a Teflon lined autoclave (70 ml) with
     |LiOH and H2O (30 ml) and was heated at 120–2008C
     |for 4 days.
     |""".stripMargin


  it should "align spans" in {
    // val mitPara = Para(sampleText)
    // val ent0 = RawText(para, para.find("NaBiO3"))
    // val _ = RawText(para, para.find("70 ml"))
    // val _ = RawText(para, para.find("Tesque Inc."))

    // val allRawTexts: Seq[RawText] = ...orderBy{ (rawText.para.order, rawText.start)}

    // val (mpageIndex, visualLines) = tokenizeStringToZoneIndex()
    /*

     val allVisLines = reflow.concat(visualLines) { _ => pad/dehyphenate }

     // Align 1 raw text w/ allVisLines:
     val rawText = allRawTexts.head
     val (preRaw, postRaw) = context(rawText, 20)
     val preWords = preRaw.split(" ")
     val postWords = postRaw.split(" ")
     allVisLines.



     */

  }


}
