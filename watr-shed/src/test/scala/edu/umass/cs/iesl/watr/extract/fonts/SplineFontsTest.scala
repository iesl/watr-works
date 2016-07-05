package edu.umass.cs.iesl.watr
package extract

import org.scalatest._

class SplineFontsTest extends FlatSpec with Matchers {

  // import fastparse.all._

  // import fastparse.core.Parsed.Failure

  // def outputFailure(input: String, failExtra: Failure.Extra): Unit = {
  //   val failedLine = input.split("\n").drop(failExtra.line-1).take(1).head
  //   val marker = ("_"*failExtra.col) + "^"
  //   println(s"""failed: ${failExtra.traced.trace}""")
  //   println(failedLine)
  //   println(marker)
  // }

  // def checkParse[T](input: String, p: P[T]): Unit = {
  //   p.parse(input) match {
  //     case Parsed.Success(value, successIndex) =>
  //       println(s"""success: ${value}""")
  //     case Parsed.Failure(parser, failIndex, failExtra) =>
  //       outputFailure(input, failExtra)
  //   }
  // }

  // import SplineFontParsers._

  // behavior of "parse charblocks"


  // it should "parse SplineSet -> EndSplineSet stanza" in {
  //   checkParse(
  //     """|SplineSet
  //        |970 250 m 0
  //        |100 17 286 -173 514 -173 c 0
  //        |EndSplineSet
  //        |""".stripMargin,
  //     SplineSetParser)

  // }


  // it should "verify component parsers" in {

  //   checkParse("MyKW", Keyword("MyKW"))
  //   checkParse("someIdent", Identifier)
  //   checkParse("s3omexIdent", Identifier)
  //   checkParse("Encoding: 1 2 3", EncodingLine)
  //   checkParse("Blah:1 2 3<2890s>..", KeyValLine)

  // }


  // it should "parse spline lines" in {

  //   """|970 250 m 0
  //      |970 -9 762 -216 514 -216 c 0
  //      |57 507 262 716 514 716 c 0
  //      |514 -173 m 0
  //      |  294 353 l 2
  //      |294 353 l 2xe8
  //      |  294 412 272 439 208 439 c 0xb4
  //      |739 -173 927 15 927 250 c 0
  //      |""".stripMargin.split("\n")
  //     .map(_.trim)
  //     .foreach({l =>
  //       checkParse(l, splineLine)
  //     })
  // }

  it should "parse StartChar -> EndChar stanza" in {

    // checkParse(
    //   """|StartChar: circlecopyrt
    //      |Encoding: 13 169 0
    //      |Fore
    //      |EndChar
    //      |""".stripMargin,
    //   CharBlockParser)

    // checkParse(
    //   """|StartChar: circlecopyrt
    //      |Encoding: 13 169 0
    //      |SplineSet
    //      |970 250 m 0
    //      | 100 17 286 -173 514 -173 c 0
    //      |514 -173 m 0
    //      |  294 353 l 2
    //      |294 353 l 2xe8
    //      |EndSplineSet
    //      |EndChar
    //      |""".stripMargin,
    //   CharBlockParser)



    // checkParse(
    //   """|StartChar: circlecopyrt
    //      |Encoding: 13 169 0
    //      |Width: 1027
    //      |Flags: MW
    //      |HStem: -216 43<400 626.5 400 638> 673 43<400.5 627>
    //      |  VStem: 57 43<133.5 367.5 133.5 378.5> 927 43<132.5 366.5>
    //      |  LayerCount: 2
    //      |Fore
    //      |SplineSet
    //      |970 250 m 0
    //      |970 -9 762 -216 514 -216 c 0
    //      |262 -216 57 -7 57 250 c 0
    //      |57 507 262 716 514 716 c 0
    //      |763 716 970 509 970 250 c 0
    //      |514 -173 m 0
    //      |739 -173 927 15 927 250 c 0
    //      |927 483 741 673 513 673 c 0
    //      |288 673 100 485 100 250 c 0
    //      |100 17 286 -173 514 -173 c 0
    //      |EndSplineSet
    //      |EndChar
    //      |""".stripMargin,
    //   CharBlockParser)

    // parseCharData(
    //   """|
    //      |StartChar: comma
    //      |Encoding: 44 44 32
    //      |Width: 250
    //      |Flags: W
    //      |LayerCount: 2
    //      |Fore
    //      |SplineSet
    //      |5 -129 m 1
    //      | -4 -112 l 1
    //      | 48 -77 71 -50 71 -23 c 0
    //      | 71 -13 66 -5 51 9 c 0
    //      | 32 26 26 37 26 54 c 0
    //      | 26 81 48 101 77 101 c 0
    //      | 110 101 135 70 135 30 c 0
    //      | 135 -26 89 -82 5 -129 c 1
    //      |EndSplineSet
    //      |EndChar
    //      |""".stripMargin)
  }


}
