package edu.umass.cs.iesl.watr
package textflow

import spindex._


class TextFlowParsingAdaptersTest extends ReflowTestUtil {
  import TextFlow._


  behavior of "text flow parsing"

  import textFlowParser._
  import GeneralizedReflow._

  it should "flatten a text flow into a searchable structure, isomorphically preserving structure" in {


  }

  it should "search/match within flow of adapted chars" in {
    val input =  "foo3 bar baz quxx"
    val inputElems = input.map(CharToElem(_)).toArray

    val parseInput = TextFlowParseInput(inputElems)
    val parser1  = P{ "foo" ~ "d".? } ~ P{ " " } ~ P{ "bar" | "baz" }

    // parser1.parse(parseInput, index: Int, instrument: Function3)
    val pRes = parser1.parse(parseInput)
    pRes match {
      case f:Parsed.Failure =>
        println(s"Fail = ${f}")
        val parsedExpected = f.extra.traced.expected
        val parsedFound = input.slice(f.index, f.index + 10)
        val stack = f.extra.traced.trace
        println(s"""expected: ${parsedExpected} """)
        println(s"""found: ${parsedFound} """)
        println(s"""stack: ${stack} """)

      case s:Parsed.Success[_] =>
        println(s"Succ = ${s}")


    }

    // val testReflow = makeTestSample()
    // val parseInput = TextFlowParseInput(testReflow.unFix)
    // println(prettyPrintTree(testReflow))
    // println("parse elems")

    // NB: this is an infinite loop at the moment:
    // println(parseInput.parseElems)
    // val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
    // val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
    // val parser0  = P{ 's' ~ 'c' ~ 'a' ~ 'n' }
    // parser0.parse(parseInput)




  }
}
