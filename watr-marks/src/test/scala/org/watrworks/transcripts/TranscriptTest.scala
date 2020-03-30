package org.watrworks
package transcripts

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.circe, circe._
import circe.parser._
import TypeTags._
import utils.ExactFloats._
// import circe.generic.semiauto._
import circe.syntax._

import geometry._

class TranscriptionFormatTest extends AnyFlatSpec with Matchers {
  // import TranscriptCodecs._
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

  it should "ser/desr glyphs" in {
    val examples = List(
      "[1, 2, 3, 4]",
      """|[[3, 2, 3, 4], {
         |  "gs": [
         |    [[1, 2, 3, 4], { "g": "A" }],
         |    [[1, 2, 3, 4], { "g": "~" }]
         |  ]
         |}]
         |""".stripMargin
    )

    examples.foreach(example => {
      val js = parse(example).getOrElse(Json.Null)
      val jsPretty = js.printWith(JsonPrettyPrinter)
      val decoded = js.as[Transcript.Glyph]
      decoded match {
        case Right(glyph) =>
          val encoded = glyph.asJson
          val encodedPretty = encoded.printWith(JsonPrettyPrinter)
          println(s"input ${example}")
          println(s"js:input ${jsPretty}")
          println(s"decoded ${glyph}")
          println(s"re-encoded ${encodedPretty}")
        case Left(value) =>
          println(s"Failed: ${value}")
      }
    })

    // val rect = LTBounds.FromInts(100, 200, 300, 400)
    // val props = Transcript.GlyphProps()
    // val glyphWithProps = Transcript.Glyph(rect,Some(props))
    // val glyphWithoutProps = Transcript.Glyph(rect,None)

    // val gwop = glyphWithoutProps.asJson
    // println(s"glyph w/o props: ${gwop}")

    // val gwp = glyphWithProps.asJson
    // println(s"glyph w/ props: ${gwp}")

    // val gwopGlyph = gwop.as[Transcript.Glyph]
    // println(s"(rt) glyph w/o props: ${gwopGlyph}")

    // val gwpGlyph = gwp.as[Transcript.Glyph]
    // println(s"(rt) glyph w/ props: ${gwpGlyph}")

    // val examples = parse(jsonExamples).getOrElse(Json.Null)

  }
  it should "ser/desr ranges" in {}
  it should "ser/desr labels" in {}

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
         |   }]
         | }
         |""".stripMargin)

         // |   "labels": [
         // |     { "name": "HasRefs", "id": "L#2", "range": [{ "unit": "page", "at": [7, 2] }] }
         // |   ]
         // |     { name: "IsGoldLabled", id: "L#3", range: [{ unit: "document" }] },

    // val jsonSrc = parse(sampleTranscript).getOrElse(Json.Null)
    // val decoded = jsonSrc.as[Transcript]
    // val prettySrc = jsonSrc.printWith(JsonPrettyPrinter)
    // println(s"json: ${prettySrc}")
    // println(s"decoded: ${decoded}")
    // val encoded = decoded.asJson

  }

}
