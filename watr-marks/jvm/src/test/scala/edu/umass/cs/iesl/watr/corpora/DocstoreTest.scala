package edu.umass.cs.iesl.watr
package corpora

import org.scalatest._

class DocstoreTest extends FlatSpec with Matchers with CorpusTestingUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "Document storage"

  it should "correctly add single document" in new CleanDocstore {
    test1()

    println(visualizeDocuments())
  }


}
