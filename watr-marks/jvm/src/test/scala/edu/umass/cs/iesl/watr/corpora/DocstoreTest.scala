package edu.umass.cs.iesl.watr
package docstore

import spindex._
import databasics._
import textreflow._

import textboxing.{TextBoxing => TB}, TB._

class DocstoreTest extends DocstoreTestUtil  {

  behavior of "In-memory Tables"

  it should "insert basic relation types" in new FreshDocstore(pageCount=1) {

    println(
      reportDocument(stableId)
    )

  }


}
