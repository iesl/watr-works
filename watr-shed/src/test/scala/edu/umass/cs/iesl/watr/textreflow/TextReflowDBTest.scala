package edu.umass.cs.iesl.watr
package textreflow

import scalaz.concurrent.Task
import doobie.imports._
import spindex._
import segment.DocumentSegmentation
import extract.images.PageImages

// import geometry._
// import PageComponentImplicits._

class FreshTextReflowDBTables(xa: Transactor[Task]) {
  val tables = new TextReflowDBTables(xa)
  tables.dropAndCreate.unsafePerformSync
  val reflowDB = new TextReflowDB(tables)
}


class TextReflowDBTest extends ConnectedComponentTestUtil {

  val doLogging = false
  val loggingProp = if (doLogging) "?loglevel=2" else ""

  val xa = DriverManagerTransactor[Task](
    s"org.postgresql.Driver",
    s"jdbc:postgresql:watrdev${loggingProp}",
    "watrworker", "watrpasswd"
  )


  behavior of "DDL create/drop"

  // it should "drop/recreate table" in new FreshTextReflowDBTables(xa){}
  val pageStrs = List(

    """|            The Title of the Paper
       |^{a}Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan
       |""".stripMargin,

    """|   EXPERIMENTAL
       |1. Sample Preparation and Characterization
       |
       |   The starting material of NaBiO_{3} ? nH2O (Nacalai Tesque
       |Inc.) was placed in a Teflon lined autoclave (70 ml) with
       |LiOH and H2O (30 ml) and was heated at 120–2008C
       |for 4 days.
       |
       |""".stripMargin
  )

  it should "add page indexes" in new FreshTextReflowDBTables(xa) {

    val docId = DocumentID("doc-0")

    val pages = stringsToTextReflows(docId, pageStrs)
    pages.foreach { page =>
      println("tree==============")
      println(
        prettyPrintTree(page)
      )
    }

    val docAtomsAndGeometry = textReflowsToAtoms(docId, pages)

    val images = pages.map(textReflowToImage(_))

    val mpageIndex = MultiPageIndex.loadSpatialIndices(
      docId, docAtomsAndGeometry
    )

    val ds = DocumentSegmentation(
      mpageIndex, PageImages(images)
    )

    reflowDB.addSegmentation(ds)


    // re-read DocumentSegmentation from DB and verify that it is the same?

    // targetregions
    // zones: ordered list of targetregion, linked to labels
    // retrieve all textreflows belonging to doc/page (or else zone???)
  }

}
