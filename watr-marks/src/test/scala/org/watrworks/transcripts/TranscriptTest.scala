package org.watrworks
package transcripts

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.circe, circe._
import circe.parser._
import TypeTags._
import utils.ExactFloats._
import circe.syntax._

import geometry._

class TranscriptionFormatTest extends AnyFlatSpec with Matchers {

  val JsonPrettyPrinter = circe.Printer(
    dropNullValues = false,
    indent = "  ",
    lbraceRight = "\n",
    rbraceLeft = "\n",
    lbracketRight = "",
    rbracketLeft = "",
    lrbracketsEmpty = "",
    arrayCommaRight = " ",
    objectCommaRight = "\n",
    colonLeft = " ",
    colonRight = " "
  )

  def isIsomorphic[A: Encoder: Decoder](strrep: String, verbose: Boolean=false): Boolean = {
    parse(strrep) match {
      case Right(jsRep) =>
        val jsPretty = jsRep.printWith(JsonPrettyPrinter)
        val decoded = jsRep.as[A]
        decoded match {
          case Right(value) =>
            val encoded = value.asJson
            val encodedPretty = encoded.printWith(JsonPrettyPrinter)

            if (verbose) {
              println(s"js:input ${jsPretty}")
              pprint.pprintln(value)
              println(s"re-encoded ${encodedPretty}")
            }

            jsRep === encoded

          case Left(value) =>
            println(s"Failed: ${value} from: ${jsPretty}")
            false
        }
      case Left(err) =>
        println(s"parsing error ${err}")
        false
    }
  }


  it should "ser/desr glyphs" in {
    val examples = List(
      "[1, 2, 3, 4]",
      """|[[3, 2, 3, 4], {
         |  "gs": [
         |    [[1, 2, 3, 4], { "g": "A" }],
         |    [[1, 2, 3, 4], { "g": "~" }]
         |  ]
         |}]
         |""".stripMargin,
      """[[3, 2, 3, 4], { "os": [1, 2, 3, 4] } ]""",
      """[[3, 2, 3, 4], { "o": 1 } ]"""

    )

    examples.foreach(example => {
      assert(isIsomorphic[Transcript.Glyph](example))
    })
  }

  it should "ser/desr ranges" in {
    val examples = List(
      """{ "unit": "text:line", "page": 1, "at": [10, 20] }""",
      """{ "unit": "text:char", "page": 2, "at": [1, 2] }""",
      """{ "unit": "shape:rect", "page": 1, "at": [123, 234, 345, 456] }""",
      """{ "unit": "document" }""",
      """{ "unit": "page", "at": [0, 3] }""",
      """{ "unit": "label" }""",
    )

    examples.foreach(example => {
      assert(isIsomorphic[Transcript.Range](example))
    })
  }

  it should "ser/desr labels" in {
    val examples = List(
      """{ "name": "Paragraph",
           "id": "0", "range": [ { "unit": "text:line", "page": 1, "at": [10, 20] } ],
           "props": { "key": "value" } }""",
      """{ "name": "Foo",
           "id": "0",
           "range": [
              { "unit": "text:line", "page": 1, "at": [10, 20] },
              { "unit": "shape:rect", "page": 1, "at": [123, 234, 345, 456] },
              { "unit": "document" },
              { "unit": "page", "at": [0, 3] }
           ],
           "props": { "key": "value" } }""",
      """{ "name": "NoProps", "id": "0", "range": [ { "unit": "document" } ] }""",
    )
    examples.foreach(example => {
      assert(isIsomorphic[Transcript.Label](example))
    })
  }

  it should "ser/desr transcripts" in {
    val sampleTranscript = (
      """|
         | {
         |   "description": "desc",
         |   "documentId": "doc-25-id",
         |   "pages": [{
         |     "pdfPageBounds": [0, 0, 61200, 79200],
         |     "lines": [{
         |       "text": "I Ã ffi",
         |       "glyphs": [
         |         [1, 2, 3, 4],
         |         [[59, 2, 3, 4], {}],
         |         [[3, 2, 3, 4], {
         |           "gs": [
         |             [[1, 2, 3, 4], { "g": "A" }],
         |             [[1, 2, 3, 4], { "g": "~" }]
         |           ]
         |         }]
         |       ]
         |     }, {
         |       "text": "Fe_{3}",
         |       "glyphs": [
         |         [11, 2, 3, 4],
         |         [22, 2, 3, 4],
         |         [[53, 2, 3, 4], { "os": [-2, -1, 1] }],
         |         [[54, 2, 3, 4], { "o": 1 }]
         |       ]
         |     }]
         |   }],
         |   "labels": [
         |     { "name": "HasRefs", "id": "L#2", "range": [{ "unit": "page", "at": [7, 2] }] },
         |     { "name": "IsGoldLabled", "id": "L#3", "range": [{ "unit": "document" }] }
         |   ]
         | }
         |""".stripMargin)

      assert(isIsomorphic[Transcript](sampleTranscript))
  }
}
