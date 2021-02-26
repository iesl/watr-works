package org.watrworks
package corpora

import TypeTags._
import textgrid._

trait CorpusTestingUtil extends TextGridBuilder {
  def createEmptyDocumentZoningApi(): DocumentZoningApi
  val regionIdGen = utils.IdGenerator[RegionID]()

  var freshDocstore: Option[DocumentZoningApi] = None

  def docStore: DocumentZoningApi = freshDocstore
    .getOrElse(sys.error("Uninitialized DocumentZoningApi; Use EmptyDatabase() class"))

  def initEmpty(): Unit = {
    try {
      regionIdGen.reset()
      freshDocstore = Some(createEmptyDocumentZoningApi())
    } catch {
      case t: Throwable =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        println(s"ERROR: ${message}")
        t.printStackTrace()
    }
  }

  trait EmptyDatabase {
    initEmpty()
  }


  def putStrLn(s: String): Seq[Unit] = Seq({
    println(s)
  })

  def addSampleDocs(docs: List[List[String]]): Seq[String@@DocumentID] = {
    val numDocs = docStore.getDocumentCount()
    for { (doc, i) <- docs.zipWithIndex } yield {
      val d =  i + numDocs
      val stableId = DocumentID(s"doc#${d}")
      addDocument(stableId, doc)
      stableId
    }
  }
  def addSampleDoc(doc: List[String]): String@@DocumentID = {
    addSampleDocs(List(doc)).head
  }

  def addDummyDocs(n: Int): Seq[String@@DocumentID] = {
    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr"
    );
    (0 until n).map{ i =>
      val stableId = DocumentID(s"doc#${i}")
      addDocument(stableId, doc)
      stableId
    }
  }

}

