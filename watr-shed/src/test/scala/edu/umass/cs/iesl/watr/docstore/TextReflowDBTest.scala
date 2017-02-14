package edu.umass.cs.iesl.watr
package docstore

import scalaz.concurrent.Task
import doobie.imports._
import spindex._
import segment.DocumentSegmentation
import extract.images.PageImages
import TypeTags._
import watrmarks.{StandardLabels => LB}
import textreflow._

class FreshTextReflowDBTables(
  override val docStore:DocumentCorpus
) extends ImageTextReflow {

  def loadSampleDoc0(): String@@DocumentID = {

    val pageStrs = List(

      """|            The Title of the Paper
         |^{a}Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan
         |""".stripMargin

    )
    val docId = DocumentID("doc-0")
    ???
    // stringsToTextReflows(docId, pageStrs)
    docId
  }

  def loadSampleDoc(): String@@DocumentID = {

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

    val docId = DocumentID("doc-0")

    ???
    // val pages = stringsToTextReflows(docId, pageStrs)

    // val images = pages.map(textReflowToImage(_))


    // val mpageIndex = MultiPageIndex.loadTextReflows(
    //   docId, pages, docStore
    // )

    docId
  }

}

class TextReflowDBTest extends ConnectedComponentTestUtil {

  val doLogging = false
  val loggingProp = if (doLogging) "?loglevel=2" else ""

  val xa = DriverManagerTransactor[Task](
    s"org.postgresql.Driver",
    s"jdbc:postgresql:watrdev${loggingProp}",
    "watrworker", "watrpasswd"
  )
  val tables = new TextReflowDBTables(xa)
  val reflowDB = new TextReflowDB(tables)

  lazy val docStore: DocumentCorpus = reflowDB.docstorage

  behavior of "database-backed corpus"

  it should "add zones" in new FreshTextReflowDBTables(docStore) {
    try {

    tables.dropAndCreate.unsafePerformSync

    val stableId = loadSampleDoc()
    val pageNum = PageNum(1)

    val docId = docStore.getDocument(stableId).get
    val pageId = docStore.getPage(docId, pageNum).get

    val targetRegionsPg1 = docStore.getTargetRegions(pageId)


    val newZoneId = docStore.createZone(docId)
    docStore.addZoneLabel(newZoneId, LB.PageLines)
    val newZone = docStore.getZone(newZoneId)
    println(s"newZone: ${newZone}")

    targetRegionsPg1.foreach { regionId =>
      val zones = docStore.getZonesForTargetRegion(regionId)
      println(s"Got zones ${zones}")
    }
    } catch {
      case t: Throwable =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        println(s"ERROR: ${message}")
        t.printStackTrace()
    }
  }


  // it should "add page indexes" in new FreshTextReflowDBTables(xa) {
  //   val docId = loadSampleDoc()

  //   val targetRegionsPg1 = reflowDB.selectTargetRegions(docId, PageNum(1))


  //   println("Zones page 0")
  //   val vlineZones0 = reflowDB.selectZones(docId, PageNum(0), LB.VisualLine)
  //   println(vlineZones0.mkString("\n"))

  //   val vlineZones = reflowDB.selectZones(docId, PageNum(1), LB.VisualLine)
  //   println("Zones page 1")
  //   println(vlineZones.mkString("\n"))


  //   println("Zones for target regions")
  //   targetRegionsPg1.foreach { tr =>
  //     val zonesAndReflows = reflowDB.getTextReflowsForTargetRegion(tr)
  //     zonesAndReflows.foreach({case (zone, reflow) =>
  //       println(s"for ${zone}")
  //       println(s"=> ${reflow.toText()}")
  //     })
  //   }
  // }

}
