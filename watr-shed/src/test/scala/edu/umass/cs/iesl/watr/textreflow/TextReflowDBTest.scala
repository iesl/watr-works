package edu.umass.cs.iesl.watr
package textreflow

import scalaz.concurrent.Task
import doobie.imports._
import spindex._
import segment.DocumentSegmentation
import extract.images.PageImages
// import doobie.free.{ connection => C }
import com.sksamuel.scrimage._
import TypeTags._
import watrmarks.{StandardLabels => LB}

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

    // val docAtomsAndGeometry = textReflowsToAtoms(docId, pages)

    val images = pages.map(textReflowToImage(_))

    val mpageIndex = MultiPageIndex.loadTextReflows(
      docId, pages
    )


    // mpageIndex.pageIndexes.foreach({case (pageId, pageIndex) =>
    //   println(s"children for page ${pageId}")
    //   val cc = pageIndex.componentToChildren
    //     .foreach({case (cid, childIds) =>
    //       val parentCC = mpageIndex.getComponent(cid, pageId)
    //       val s = parentCC.toString() + childIds
    //         .map({case (chlbl, chids) =>
    //           chlbl +": " + chids.map(mpageIndex.getComponent(_, pageId).roleLabel).mkString(", ")
    //         })
    //         .mkString("\n  ", "\n  ", "\n")
    //       println(s)
    //     })
    //     // .mkString("\n  ", "\n  ", "\n")
    //   // println(cc)
    // })

    // val mpageIndex = MultiPageIndex.loadSpatialIndices(
    //   docId, docAtomsAndGeometry
    // )

    val ds = DocumentSegmentation(
      mpageIndex, PageImages(images)
    )

    reflowDB.addSegmentation(ds)

    val targetRegions = reflowDB.selectTargetRegions(docId, PageID(0))
    println("targetRegions")
    println(targetRegions)

    println("Zones page 0")
    val vlineZones0 = reflowDB.selectZones(docId, PageID(0), LB.VisualLine)
    println(vlineZones0.mkString("\n"))

    val vlineZones = reflowDB.selectZones(docId, PageID(1), LB.VisualLine)
    println("Zones page 1")
    println(vlineZones.mkString("\n"))

    // val blank = Image.filled(w, h, Color.Transparent)

    // vlineZones.foreach{zone =>
    //   zone.regions.foreach {tr =>
    //     reflowDB.overwriteTargetRegionImage(tr, blank)
    //   }

    // }

    // re-read DocumentSegmentation from DB and verify that it is the same?

    // targetregions
    // zones: ordered list of targetregion, linked to labels
    // retrieve all textreflows belonging to doc/page (or else zone???)
  }

}



// import doobie.free.{ drivermanager => DM }
// import geometry._
// import PageComponentImplicits._

// import doobie.contrib.hikari.hikaritransactor._

// object HikariExample {

//   def tmain: Task[Unit] =
//     for {
//       xa <- HikariTransactor[Task]("org.h2.Driver", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "")
//       _  <- FirstExample.examples.transact(xa)
//       _  <- xa.shutdown
//     } yield ()

//   def main(args: Array[String]): Unit =
//     tmain.unsafePerformSync

// }

// case class FreshTables(
//   ex: TextReflowDB => Unit
// ) {
//   val doLogging = false
//   val loggingProp = if (doLogging) "?loglevel=2" else ""

//   val xa = DriverManagerTransactor[Task](
//     s"org.postgresql.Driver",
//     s"jdbc:postgresql:watrdev${loggingProp}",
//     "watrworker", "watrpasswd"
//   )

//   val tables = new TextReflowDBTables(xa)
//   val reflowDB = new TextReflowDB(tables)


//   def run: ConnectionIO[String] = for {
//     _ <- C.delay(println("Running example"))
//     _ <- tables.dropAndCreateAll
//     _  = ex(reflowDB) // .except(t => t.toString.point[ConnectionIO])
//     _ <- C.close
//     // _ <- xa.shutdown
//   } yield "Ok"

//   run.transact(xa).unsafePerformSync
// }
