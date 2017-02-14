package edu.umass.cs.iesl.watr
package corpora

import org.scalatest._

// import spindex._
// import databasics._
// import textreflow._
import corpora._

import textboxing.{TextBoxing => TB}, TB._

class DocstoreTest extends FlatSpec with Matchers with DocstoreTestUtil {

  behavior of "In-memory Tables"

  it should "insert basic relation types" in new FreshDocstore(pageCount=1) {

    println(
      reportDocument(stableId)
    )

  }


}
