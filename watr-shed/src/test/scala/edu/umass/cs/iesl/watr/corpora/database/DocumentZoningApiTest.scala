package edu.umass.cs.iesl.watr
package corpora
package database

import TypeTags._
import watrmarks.{StandardLabels => LB}

class DocumentZoningApiTest extends DatabaseTest {

  behavior of "database-backed corpus"

  // it should "run default tests" in new CleanDocstore {
  //   addSampleDoc(MockPapers.sample_4pg_3x3_doc)

  //   val stableId = DocumentID("doc#0")
  //   // val stableId = DocumentID("10.1101-090498.d")
  //   val docId = docStore.getDocument(stableId).get
  //   val pageId = docStore.getPage(docId, PageNum(0)).get
  //   val lineZones = docStore.getPageZones(pageId, LB.VisualLine)
  //   val lineRegions = lineZones.flatMap(_.regions)

  //   docStore.labelRegions(LB.TextBlock, lineRegions.map(_.toPageRegion()))

  //   println(s"========================================")
  //   val label = LB.Title
  //   val labelId = docStore.ensureLabel(label)
  //   for {
  //     zone <- docStore.getDocumentZones(docId, label)
  //   } {
  //     println(s"Zone: $zone (label= ${label})")
  //     zone.regions.foreach { r =>
  //       println(s"  region: ${r}")
  //     }
  //   }

  //   for {
  //     pageId <- docStore.getPages(docId)
  //     pageDef <- docStore.getPageDef(pageId)
  //     labelId <- docStore.getZoneLabelsForDocument(docId)
  //     zoneId <- docStore.getZonesForDocument(docId, labelId)
  //   } {
  //     println(visualizeDocuments())
  //     println(s"deleting zone ${zoneId}")
  //     docStore.deleteZone(zoneId)
  //   }

  // }

  it should "export and reimport zones" in new CleanDocstore {
    addSampleDoc(MockPapers.sample_4pg_3x3_doc)

    val stableId = DocumentID("doc#0")
    val docId = docStore.getDocument(stableId).get
    val pageId = docStore.getPage(docId, PageNum(0)).get
    val lineZones = docStore.getPageZones(pageId, LB.VisualLine)
    val lineRegions = lineZones.flatMap(_.regions)

    docStore.labelRegions(LB.TextBlock, lineRegions) // .map(_.toPageRegion()))

    val zoneExport = docStore.exportDocumentZones(Seq(LB.TextBlock))

    // println(s"========================================")
    // val label = LB.TextBlock
    // // val labelId = docStore.ensureLabel(label)

    // for {
    //   zone <- docStore.getDocumentZones(docId, label)
    // } {
    //   println(s"Zone: $zone (label= ${label})")
    //   zone.regions.foreach { r =>
    //     println(s"  region: ${r}")
    //   }
    // }
  }

}
