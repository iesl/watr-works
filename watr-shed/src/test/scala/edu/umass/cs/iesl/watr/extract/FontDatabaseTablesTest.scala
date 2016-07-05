package edu.umass.cs.iesl.watr
package extract

import org.scalatest._

// import DatabaseApi._
import ammonite.ops._



class FontDatabaseTablesTest extends FlatSpec {
  // sequential

  behavior of "font database"
  val db = new FontDatabase(cwd / "fontdb")

  it should "create tables" in {
    db.commit()
    db.shutdown()

  }


}
