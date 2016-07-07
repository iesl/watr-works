package edu.umass.cs.iesl.watr
package extract
package fonts

import org.scalatest._

// import DatabaseApi._
import ammonite.ops._



class FontDatabaseTablesTest extends FlatSpec with SequentialNestedSuiteExecution with BeforeAndAfter {

  val db = new FontDatabaseApi(cwd / "fontdb~")
  val fontPath = Path(getClass.getResource("/fontdb/gulliver-sfdirs").getPath)

  before {
    db.dropAndRecreateDatabase()
  }

  after {
  }


  behavior of "font database"


  // it should "load a unique font dir into db" in {
  //   db.addFontDir(SplineFonts.loadSfdir(fontPath / "font-0.sfdir"))

  //   // db.reportAll()
  // }

  // it should "correctly deal with space glyphs" in {}

  it should "load an overlapping font dir into db" in {

    db.addFontDir(SplineFonts.loadSfdir(fontPath / "font-2.sfdir"))
    db.addFontDir(SplineFonts.loadSfdir(fontPath / "font-3a.sfdir"))
    db.addFontDir(SplineFonts.loadSfdir(fontPath / "font-3b.sfdir"))

    db.addFontDir(SplineFonts.loadSfdir(fontPath / "font-0.sfdir"))

    db.reportAll()
  }


}
