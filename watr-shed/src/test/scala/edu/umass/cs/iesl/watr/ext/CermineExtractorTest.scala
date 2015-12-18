package edu.umass.cs.iesl.watr
package ext


import java.io.InputStreamReader
import org.scalatest._


class CermineExtractorSpec extends FlatSpec {

  import watrmarks._
  import StandardLabels._

  object papers {
    def `6376.svg` = getClass().getResourceAsStream("/papers/6376.svg")
  }

  behavior of "svg -> cermine format"

  it should  "convert to bxdocument" in {

    val svg = watrmarks.dom.readWatrDom(new InputStreamReader(papers.`6376.svg`), bioDict)

    // watrdom -> BxDocument conversion
    val charCursor = svg.toCursor(CharLabel)

    println("char cursor")
    println(charCursor)

    // charCursor.foreach{ case cur =>
    //   println("char ="+cur.getText)

    // }

    // val document = new CermineExtractor().extractCharacters(papers.`6376.svg`)


  }


}
