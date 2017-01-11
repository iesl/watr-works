package edu.umass.cs.iesl.watr
package textreflow

import scalaz.concurrent.Task
import doobie.imports._
import spindex._
import segment.DocumentSegmentation
import extract.images.PageImages

class FreshTextReflowDBTables(xa: Transactor[Task]) {
  val tables = new TextReflowDBTables(xa)
  tables.dropAndCreate.unsafePerformSync
  val reflowDB = new TextReflowDB(tables)
}


class TextReflowDBTest extends ConnectedComponentTestUtil {
  val xa = DriverManagerTransactor[Task](
    "org.postgresql.Driver",
    "jdbc:postgresql:watrdev",
    "watrworker", "watrpasswd"
  )

  behavior of "DDL create/drop"

  // it should "drop/recreate table" in new FreshTextReflowDBTables(xa){}

  it should "add page indexes" in new FreshTextReflowDBTables(xa) {

    val page0 = (
      """|
         |^{a}Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan
         |""".stripMargin)

    val page1 = (
      """|
         |   EXPERIMENTAL
         |1. Sample Preparation and Characterization
         |
         |   The starting material of NaBiO_{3} ? nH2O (Nacalai Tesque
         |Inc.) was placed in a Teflon lined autoclave (70 ml) with
         |LiOH and H2O (30 ml) and was heated at 120–2008C
         |for 4 days.
         |
         |""".stripMargin)

    val tr0 = stringToTextReflow(page0)
    // tr0
    // construct a page index from this text reflow

    val docId = DocumentID("doc-0")
    val (mpageIndex, images) = createMultiPageIndexWithImages(docId, page0, page1)
    val ds = DocumentSegmentation(
      mpageIndex, PageImages(images)
    )

    reflowDB.addMultiPageIndex(ds)
  }

}
