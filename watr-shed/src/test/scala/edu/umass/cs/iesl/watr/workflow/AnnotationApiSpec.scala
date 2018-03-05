package edu.umass.cs.iesl.watr
package workflow

import corpora.database.DatabaseTest

class AnnotationApiSpec extends DatabaseTest with UserbaseTestHelpers {

  it should "create label schemas" in new EmptyDatabase {

  }

  it should "annotate a document" in new EmptyDatabase {
    val corpusSize = 2
    val stableIds = addDummyDocs(corpusSize)
    // val userIds = initUsers(1)
    // val stableId0 = docStore.getDocuments(1, 0).head
    val docId0 = docStore.getDocument(stableIds.head).get
    val annotId = annotApi.createAnnotation(docId0)

    {
      val annot = annotApi.getAnnotationRecord(annotId)
      println(s"Annot = ${annot}")
    }
    annotApi.updateBody(annotId, "{}")
    // corpusAccessDB.updateAnnotationStatus(annotId, StatusCode("NewStatus"))

    {
      val annot = annotApi.getAnnotationRecord(annotId)
      println(s"Annot = ${annot}")
    }

    // val annot1 = annotApi.getAnnotation(annotId)
    // println(s"Annot 1 = ${annot1}")
  }
}
