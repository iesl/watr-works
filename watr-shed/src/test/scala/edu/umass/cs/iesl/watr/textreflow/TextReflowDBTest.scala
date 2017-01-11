package edu.umass.cs.iesl.watr
package databasics

// import scalaz._, Scalaz._
import scalaz.concurrent.Task
import doobie.imports._
// import doobie.contrib.postgresql.pgtypes._
import spindex._

class FreshTextReflowDBTables(
  xa: Transactor[Task]
) {
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

    val docId = DocumentID("doc-0")
    val mpageIndex = createMultiPageIndex(docId, page0, page1)

    reflowDB.addMultiPageIndex(mpageIndex)
  }


}
