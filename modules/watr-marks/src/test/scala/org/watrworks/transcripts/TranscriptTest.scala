package org.watrworks
package transcripts

import io.circe, circe._
import circe.parser._
import circe.syntax._

class TranscriptionFormatTest extends WatrSpec {

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

  val verbose = false

  it should "ser/desr glyphs" in {

    val examples = List(
      """["a", 1, [1, 2, 3, 4]]""",
      """|["ffi", 23, [3, 2, 3, 4], {
         |  "kind": "rewrite",
         |  "gs": [
         |    ["ﬃ", 24, [1, 2, 3, 4]]
         |   ]
         |  }
         |]""".stripMargin,
    )

    examples.foreach(example => {
      assert(isIsomorphic[Transcript.Glyph](example, verbose))
    })
  }


  it should "ser/desr ranges" in {
    val examples = List(
      """{ "unit": "text:line", "at": [10, 20] }""",
      """{ "unit": "text:char", "at": [1, 2] }""",
      """{ "unit": "shape", "at": [123, 234, 345, 456] }""",
      """{ "unit": "document", "at": "docId#32" }""",
      """{ "unit": "page", "at": 0 }""",
      """{ "unit": "label", "at": 23 }""",
      """{ "unit": "stanza", "at": 32 }""",
    )

    examples.foreach(example => {
      assert(isIsomorphic[Transcript.Range](example, verbose))
    })
  }

  it should "ser/desr labels" in {
    val examples = List(
      """{ "name": "Paragraph",
           "id": 0, "range": [ { "unit": "text:line", "at": [10, 20] } ],
           "props": { "key": "value" } }""",
      """{ "name": "Author",
           "range": [ { "unit": "text:line", "at": [10, 20] } ],
           "children": [
              { "name": "FirstName", "range": [ { "unit": "text:char", "at": [1, 3] } ] },
              { "name": "LastName", "range": [ { "unit": "text:char", "at": [1, 3] } ] }
           ]}""",
      """{ "name": "Foo",
           "id": 1,
           "range": [
              { "unit": "text:line",  "at": [10, 20] },
              { "unit": "shape", "at": [123, 234, 345, 456] },
              { "unit": "document", "at": "d3" },
              { "unit": "page", "at": 0 }
           ],
           "props": { "key": "value" } }""",
    )


    examples.foreach(example => {
      assert(isIsomorphic[Transcript.Label](example, verbose))
    })
  }


  it should "ser/desr glyphrefs" in {
    val examples = List(
      "23",
      """ "abc" """
    )

    import Transcript.GlyphRef._

    examples.foreach(example => {
      assert(isIsomorphic[Transcript.GlyphRef](example, verbose))
    })
  }

  it should "ser/desr stanzas" in {
    val examples = List(
      """|{
         |  "id": 1,
         |  "lines": [
         |     { "text": "ffi", "glyphs": [10, 10, 10] },
         |     { "text": "_{j}", "glyphs": ["_{", 10, "}"] }
         |  ],
         |  "labels": [
         |    { "name": "Paragraph", "range": [ { "unit": "text:line", "at": [10, 20] } ]  }
         |  ]
         |}""".stripMargin
    )


    examples.foreach(example => {
      assert(isIsomorphic[Transcript.Stanza](example, verbose))
    })
  }

  it should "ser/desr transcripts" in {
    val sampleTranscript = (
      """| {
         |   "documentId": "doc-25-id",
         |   "pages": [{
         |     "page": 1,
         |     "bounds": [0, 0, 61200, 79200],
         |     "glyphs": [
         |       ["e", 1, [1, 2, 3, 4]],
         |       ["ffi", 2, [3, 2, 3, 4], { "kind": "rewrite", "gs": [["ﬃ", 3, [1, 2, 3, 4]]] } ]
         |     ]
         |   }],
         |   "labels": [
         |     { "name": "HasRefs", "id": 2, "range": [{ "unit": "page", "at": 7 }] },
         |     { "name": "IsGoldLabled", "id": 3, "range": [{ "unit": "document", "at": "self" }] }
         |   ],
         |  "stanzas": [
         |      { "id": 20, "lines": [], "labels": [] },
         |      { "id": 20,
         |        "lines": [
         |          {"text": "abc", "glyphs": []}
         |        ],
         |        "labels": [{"name": "Foo", "range": []}]
         |      }
         |  ]
         | }
         |""".stripMargin)


      assert(isIsomorphic[Transcript](sampleTranscript, verbose))
  }
}
