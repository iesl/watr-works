package edu.umass.cs.iesl.watr
package ext

import org.scalatest._



object papers {
  def `6376.svg` = getClass().getResourceAsStream("/papers/6376.svg")
  def `6376.pdf` = getClass().getResourceAsStream("/papers/6376.pdf")
}


class ITextExtractorTest extends FlatSpec {


  it should "use custom char extractor" in {
    val is = papers.`6376.pdf`
    val _ = itextUtil.itextPdfToSvg(is)
  }


}
