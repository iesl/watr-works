package edu.umass.cs.iesl.watr
package workflow

import corpora.database.DatabaseTest
import watrmarks._

class AnnotationApiSpec extends DatabaseTest with UserbaseTestHelpers {

  import _root_.io.circe, circe.syntax._
  import utils.DoOrDieHandlers._

  it should "serialize/unserialize schemas" in {
    {
      val schema = ExampleLabelSchemas.authorNamesSchema
      val strRep = schema.asJson.noSpaces
      val rtrip  = strRep.decodeOrDie[LabelSchema]()
      schema shouldEqual rtrip
    }
    {
      val schema = ExampleLabelSchemas.headerLabelSchema
      val strRep = schema.asJson.spaces2
      val rtrip = strRep.decodeOrDie[LabelSchemas]()

      schema shouldEqual rtrip
    }

  }

  it should "create label schemas" in new EmptyDatabase {
    val schema = ExampleLabelSchemas.headerLabelSchema
    val schemaId = annotApi.createLabelSchema(schema)
    val rtSchema = annotApi.getLabelSchema(schemaId)

    rtSchema shouldEqual schema

    //  Should throw on duplicate named schema
    intercept[Throwable] {
      annotApi.createLabelSchema(schema)
    }
    annotApi.deleteLabelSchema(schemaId)
    // Throw on invalid get
    intercept[Throwable] {
      annotApi.getLabelSchema(schemaId)
    }

  }

  it should "create/delete annotations on a document" in new EmptyDatabase {
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

    {
      val annot = annotApi.getAnnotationRecord(annotId)
      println(s"Annot = ${annot}")
    }

  }
  it should "assign annotations" in new EmptyDatabase {
    val corpusSize = 2
    val stableIds = addDummyDocs(corpusSize)
    val userIds = initUsers(1)
    val user0 = userIds.head
    val docId0 = docStore.getDocument(stableIds.head).get
    val annotId = annotApi.createAnnotation(docId0)
    annotApi.assignOwnership(annotId, user0)
    val annots = annotApi.listAnnotations(user0, None)

    annots.length shouldBe 1
    annots.head shouldBe annotId
  }

  import TypeTags._

  it should "set annotation paths and filter queries on paths" in new EmptyDatabase {
    val corpusSize = 2
    val stableIds = addDummyDocs(corpusSize)
    val userIds = initUsers(3)
    val user0 = userIds.head
    val user1 = userIds.drop(1).head
    val docId0 = docStore.getDocument(stableIds.head).get
    val annotId = annotApi.createAnnotation(docId0)

    val testPath = CorpusPath("A.B.C")

    annotApi.setCorpusPath(annotId, testPath)

    val queryWithExpectedResultCount = List[(String@@CorpusPathQuery, Int)](
      (CorpusPathQuery("*.B.C"), 1),
      (CorpusPathQuery("A.*.C"), 1),
      (CorpusPathQuery("A.B.*"), 1),
      (CorpusPathQuery("A.B.C"), 1),
      (CorpusPathQuery("A"), 0),
    )

    queryWithExpectedResultCount.foreach{case (q, i) =>
      // println(s"testing ${q}")
      annotApi.listAnnotations(q).length shouldBe i
    }

    annotApi.assignOwnership(annotId, user0)

    queryWithExpectedResultCount.foreach{case (q, i) =>
      annotApi.listAnnotations(user0, Some(q)).length shouldBe i
    }

    queryWithExpectedResultCount.foreach{case (q, i) =>
      annotApi.listAnnotations(user1, Some(q)).length shouldBe 0
    }
  }



}
