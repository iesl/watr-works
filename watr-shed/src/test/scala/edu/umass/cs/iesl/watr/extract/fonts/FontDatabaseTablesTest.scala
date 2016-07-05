package edu.umass.cs.iesl.watr
package extract

import org.scalatest._

// import DatabaseApi._
import ammonite.ops._



class FontDatabaseTablesTest extends FlatSpec {
  // sequential

  behavior of "font database"
  val db = new FontDatabase(cwd / "fontdb")

  val fontPath = Path(getClass.getResource("/fontdb/gulliver-sfdirs").getPath)

  it should "create tables" in {
    db.commit()
    db.shutdown()
  }

  it should "load font dirs into db" in {

    // println(s"path = ${fontPath} exists= ${exists(fontPath)} ")

  }


}
