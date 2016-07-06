package edu.umass.cs.iesl.watr
package extract
package fonts

import org.scalatest._

// import DatabaseApi._
import ammonite.ops._



class FontDatabaseTablesTest extends FlatSpec with SequentialNestedSuiteExecution with BeforeAndAfterEach {

  val db = new FontDatabase(cwd / "fontdb~")
  val fontPath = Path(getClass.getResource("/fontdb/gulliver-sfdirs").getPath)

  override def beforeEach(): Unit = {
    db.dropAndRecreateDatabase()
  }

  override def afterEach(): Unit = {
  }


  behavior of "font database"


  it should "load a unique font dir into db" in {
    db.addFontDir(SplineFonts.loadSfdir(fontPath / "font-0.sfdir"))

    db.reportAll()

  }

  // it should "load an overlapping font dir into db" in {

  //   db.addFontDir(SplineFonts.loadSfdir(fontPath / "font-0.sfdir"))
  //   db.addFontDir(SplineFonts.loadSfdir(fontPath / "font-1.sfdir"))


  //   db.reportAll()
  // }


}
