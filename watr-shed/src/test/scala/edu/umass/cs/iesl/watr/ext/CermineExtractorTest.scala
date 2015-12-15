package edu.umass.cs.iesl.watr.ext


import org.scalatest._


class CermineExtractorSpec extends FlatSpec {

  object papers {
    def `6376.svg` = getClass().getResourceAsStream("/papers/6376.svg")
  }

  behavior of "svg -> cermine format"

  it should  "convert to bxdocument" in {

    val document = new CermineExtractor().extractCharacters(papers.`6376.svg`)

  }


}
