package edu.umass.cs.iesl.watr
package docstore

import org.scalatest._
// import watrmarks.{StandardLabels => LB}
import corpora._
// import geometry._
// import TypeTags._

class DBDocumentCorpusTest extends FlatSpec with Matchers with CorpusTestingUtil {

  def createEmptyDocumentCorpus(): DocumentCorpus = {
    val tables = new TextReflowDBTables()

    val reflowDB = new TextReflowDB(
      tables,
      dbname="watrdev",
      dbuser="watrworker",
      dbpass="watrpasswd"
    )


    reflowDB.runq {
      reflowDB.veryUnsafeDropDatabase().run
    }

    reflowDB.dropAndRecreate
    reflowDB.docStore
  }

  behavior of "database-backed corpus"


  it should "run default tests" in new CleanDocstore {
    test1()

    println(visualizeDocuments())
  }



}
