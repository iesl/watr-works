package edu.umass.cs.iesl.watr
package corpora

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
// import TypeTags._


class Tables {
  // val Rel = Relations

  // object documents extends DBRelation[DocumentID, Rel.Document] {
    // val stableIds = mutable.HashMap[String@@DocumentID, Int@@DocumentID]()

    // def forStableId(stableId: String@@DocumentID): Option[Int@@DocumentID] = {
    //   stableIds.get(stableId)
    // }

    // def add(stableId: String@@DocumentID): Rel.Document= {
    //   val rec = Rel.Document(nextId(), stableId)
    //   insert(rec.prKey, rec)
    //   stableIds.put(stableId, rec.prKey)
    //   rec
    // }
  

}

class MemTablesTest extends AnyFlatSpec with Matchers {

  behavior of "In-memory Tables"

  it should "insert/delete/update" in new Tables {}
  it should "declare primary key" in new Tables {}
  it should "declare foreign key constraint" in new Tables {}
  it should "declare secondary indexes" in new Tables {}


}
