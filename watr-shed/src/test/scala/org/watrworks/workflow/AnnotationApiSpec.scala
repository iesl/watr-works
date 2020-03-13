package edu.umass.cs.iesl.watr
package workflow

import corpora.database.DatabaseTest
import watrmarks._
import geometry._
import TypeTags._
import corpora._

class AnnotationApiSpec extends DatabaseTest with UserbaseTestHelpers {

  import _root_.io.circe, circe.syntax._
  import utils.DoOrDieHandlers._
  val Math: Label = Label.auto

  it should "serialize/unserialize schemas" in {
    {
      val schema = HeaderLabelSchemas.authorNamesSchema
      val strRep = schema.asJson.noSpaces
      val rtrip  = strRep.decodeOrDie[LabelSchema]()
      schema shouldEqual rtrip
    }
    {
      val schema = HeaderLabelSchemas.headerLabelSchema
      val strRep = schema.asJson.spaces2
      val rtrip = strRep.decodeOrDie[LabelSchemas]()

      schema shouldEqual rtrip
    }

  }

  it should "create label schemas" in new EmptyDatabase {
    val schema = HeaderLabelSchemas.headerLabelSchema
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

    val stableId0 = stableIds.head
    val docId0 = docStore.getDocument(stableId0).get
    val pageId0 = docStore.getPage(docId0, PageNum(0)).get
    val stablePage0 = docStore.getPageIdentifier(pageId0)

    {
      val loc = AnnotatedLocation.Location(StableDocument(stableId0))
      val annotId = annotApi.createAnnotation(Math, loc)
      val annot = annotApi.getAnnotationRecord(annotId)
      annot.location shouldBe loc
      // println(s"Annot = ${annot}")
    }
    {
      val loc = AnnotatedLocation.Location(StableDocument(stableId0))
      val annotId = annotApi.createAnnotation(Math, loc)
      val annot = annotApi.getAnnotationRecord(annotId)
      // println(s"Annot = ${annot}")

      annot.location shouldBe loc

      val loc2 = AnnotatedLocation.Zone(List(
        PageRegion(stablePage0, LTBounds.Ints(10, 20, 30, 40))
      ))

      // val jsonStr = loc2.asJson.spaces2


      annotApi.updateLocation(annotId, loc2)

      val annot2 = annotApi.getAnnotationRecord(annotId)

      val jsonStr = annot2.asJson.spaces2
      println(jsonStr)
      // println(s"Annot2 = ${annot2}")

      annot2.location shouldBe loc2
    }

    {
      val bbox = LTBounds.Ints(10, 20, 30, 40)
      val loc = AnnotatedLocation.Zone(List(
        PageRegion(stablePage0, bbox)
      ))
      val annotId = annotApi.createAnnotation(Math, loc)
      val annot = annotApi.getAnnotationRecord(annotId)
      annot.location shouldBe loc
      // println(s"Annot = ${annot}")
    }
  }
  it should "assign annotations" in new EmptyDatabase {
    val corpusSize = 2
    val stableIds = addDummyDocs(corpusSize)
    val userIds = initUsers(1)
    val user0 = userIds.head
    val stableId0 = stableIds.head
    val loc = AnnotatedLocation.Location(StableDocument(stableId0))
    val annotId = annotApi.createAnnotation(Math, loc)
    annotApi.assignOwnership(annotId, user0)
    val annots = annotApi.listUserAnnotations(user0, None)

    annots.length shouldBe 1
    annots.head shouldBe annotId


    val rec = annotApi.getAnnotationRecord(annotId)
    // println(rec)
  }


  it should "set annotation paths and filter queries on paths" in new EmptyDatabase {
    val corpusSize = 2
    val stableIds = addDummyDocs(corpusSize)
    val userIds = initUsers(3)
    val user0 = userIds.head
    val user1 = userIds.drop(1).head

    val stableId0 = stableIds.head
    val loc = AnnotatedLocation.Location(StableDocument(stableId0))
    val annotId = annotApi.createAnnotation(Math, loc)

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
      annotApi.listPathAnnotations(q).length shouldBe i
    }

    annotApi.assignOwnership(annotId, user0)

    queryWithExpectedResultCount.foreach{case (q, i) =>
      annotApi.listUserAnnotations(user0, Some(q)).length shouldBe i
    }

    queryWithExpectedResultCount.foreach{case (q, i) =>
      annotApi.listUserAnnotations(user1, Some(q)).length shouldBe 0
    }
  }



}
