package edu.umass.cs.iesl.watr
package db

import org.scalatest._

import watrmarks._

import java.io.InputStreamReader

class SpatialDBTest extends FlatSpec {

  it should "report all rects in svg" in { 

    val doc = dom.readWatrDom(
      new InputStreamReader(ext.papers.`6376.svg`),
      StandardLabels.bioDict
    )

    doc
  }

  //   behavior of "mixed spatial index/relational db"

  //   it should "connect to db" in {

  //     val xa = DriverManagerTransactor[Task](
  //       "org.h2.Driver", "jdbc:h2:world", "h2", ""
  //     )
  //   }

}
