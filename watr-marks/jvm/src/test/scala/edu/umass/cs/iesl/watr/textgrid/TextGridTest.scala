package edu.umass.cs.iesl.watr
package textgrid

import org.scalatest._

import corpora._
import TypeTags._

trait TextGridTestUtil extends FlatSpec with Matchers with TextGridBuilder

class TextGridTests extends TextGridTestUtil {

  override val docStore: DocumentZoningApi = new MemDocZoningApi

  behavior of "TextGrid Rows"

  it should "support plaintext rows" in {


    val docs = List(
      List(
        "abc\ndef\nghi",
        "012\n345\n678"
      )
    )

    for { (doc, i) <- docs.zipWithIndex } {
      addDocument(DocumentID(s"doc#${i}"), doc)
    }
  }


  it should "support alphabet soup regions" in {}
  it should "join two rows into one, with optional space or dehyphenation" in {}

  it should "clip single row to target region(s)" in {}
  it should "clip multiple rows to target region(s)" in {}

  it should "serialize/unserialize" in {}

}
