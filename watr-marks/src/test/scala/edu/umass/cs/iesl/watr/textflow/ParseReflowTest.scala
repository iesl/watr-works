package edu.umass.cs.iesl.watr
package textflow

import spindex._


class ParseReflowTest extends ReflowTestUtil {
  import TextFlow._


  behavior of "text flow parsing"

  import textFlowParser._
  import GeneralizedReflow._


  it should "parse a basic flow of char atoms" in {
    // of LiFePO4 scan- ning electron
    val testReflow = makeTestSample()
    val parseInput = TextFlowParseInput(testReflow.unFix)
    println(prettyPrintTree(testReflow))
    println("parse elems")

    // NB: this is an infinite loop at the moment:
    println(parseInput.parseElems)


    // val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
    // val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
    // val parser0  = P{ 's' ~ 'c' ~ 'a' ~ 'n' }
    // val parser1  = P{ "scan" }
    // parser0.parse(parseInput)




  }
}
