package edu.umass.cs.iesl.watr
package textgraph

import geometry._

import TypeTags._
import LabeledSequenceTreeTransforms._

import _root_.io.circe, circe._
import circe.syntax._

import utils.DoOrDieHandlers._
import LabeledSequenceCodecs._
import TextGridLabelWidget._
import textboxing.{TextBoxing => TB}, TB._

class TextGraphCodecTests extends TextGraphSpec {

  val dummyPageRegion = PageRegion(
    StablePage(DocumentID("docX"), PageNum(0)), LTBounds.empty
  )

  "representation of labels as JSON" in {
    /** Old version: ['label', begin, len, [children]]
      *  [
      *      ["Author", 0, 15], [
      *          [["FirstName", 0, 4], []],
      *          [["LastName", 4, 4], []],
      *          [["NoteMarker", 8, 2], []],
      *          [["NoteMarker", 10, 2], []],
      *          [["NoteMarker", 12, 3], []]
      *      ]
      *  ],
      *
      *
      *
      *
      **/

    // New Version:
    Json.obj(
      "LineStarts" := List(
        1, 24
      ),
      "Labeling" := Json.obj(
        "Label" := "Author",
        "Span" := List(1, 12),
        "Children" := List[Json](
          Json.obj(
            "Label" := "FirstName",
            "Span" := List(1, 2),
          ),
          Json.obj(
            "Label" := "LastName",
            "Span" := List(4, 2),
          )
        )
      )
    )


}

  "Behavior of labeled TextGraph serialization to/from Json" in {
    import TextGraphJvm._

    val textGraph = makeBishopClarkSample()
    val asJson = textGraph.asJson

    println(asJson.toString())

    // val roundTripGraph = asJson.decodeOrDie[TextGraphJvm]()
    // val rtJson = roundTripGraph.asJson
    // println(asJson)

    // val cmpare = asJson.toString().box besideS rtJson.toString().box
    // println("\n\n\n----------------------------------------------")
    // println(cmpare)
    // println("========================================================")

    // assert(asJson.toString() === rtJson.toString())
  }

}
