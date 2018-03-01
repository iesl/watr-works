package edu.umass.cs.iesl.watr
package workflow

import corpora.database.DatabaseTest

class AnnotationApiSpec extends DatabaseTest with UserbaseTestHelpers {

  it should "annotate a locked document" in new EmptyDatabase {
    val corpusSize = 2
    addDummyDocs(corpusSize)
    val userIds = initUsers(1)
    // val workflowId = initWorkflows(1).head
    val stableId0 = docStore.getDocuments(1, 0).head
    val docId0 = docStore.getDocument(stableId0).get
    // val annotId = annotApi.createAnnotation(userIds(0),  docId0, workflowId)
    // val annot = annotApi.getAnnotation(annotId)
    // corpusAccessDB.updateAnnotationJson(annotId, )
    // corpusAccessDB.updateAnnotationStatus(annotId, StatusCode("NewStatus"))

    // println(s"Annot = ${annot}")

    // val annot1 = annotApi.getAnnotation(annotId)
    // println(s"Annot 1 = ${annot1}")
  }
}
