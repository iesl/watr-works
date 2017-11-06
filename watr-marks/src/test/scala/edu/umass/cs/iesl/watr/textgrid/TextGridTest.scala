package edu.umass.cs.iesl.watr
package textgrid

import org.scalatest._

import corpora._
import TypeTags._

trait TextGridTestUtil extends FlatSpec with Matchers with TextGridBuilder

class TextGridTests extends TextGridTestUtil {

  override val docStore: DocumentZoningApi = new MemDocZoningApi

  val docs = List(
    List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "rst\nuvw\nxyz"
    ),
    List(
      "exit-\ning\n",
      "cellar\ndoor\n",
      "close-\nup\n"
    )
  )
  for { (doc, i) <- docs.zipWithIndex } {
    addDocument(DocumentID(s"doc#${i}"), doc)
  }

  behavior of "TextGrid Rows"

  it should "support plaintext rows" in {
    visualizeDocStore()
  }

  it should "join two rows into one, with optional space or dehyphenation" in {

    // val textGrid = stringToPageTextGrid("exit-\ning\n", PageNum(0), None)

  }

  it should "support alphabet soup regions" in {}
  it should "clip single row to target region(s)" in {}
  it should "clip multiple rows to target region(s)" in {}
  it should "serialize/unserialize" in {}

  behavior of "TextGrid Cursors"

  it should "slurp/barf" in {}

  behavior of "windows"

}
