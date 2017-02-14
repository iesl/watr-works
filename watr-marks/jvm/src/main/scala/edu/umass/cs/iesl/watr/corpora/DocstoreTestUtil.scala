package edu.umass.cs.iesl.watr
package docstore

import org.scalatest._

import textreflow._
import textboxing.{TextBoxing => TB}, TB._
import TypeTags._
import databasics._

object TextPageSamples {
  val samples = List(
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

}

trait DocstoreTestUtil extends FlatSpec with Matchers with PlainTextReflow {
  var freshDocstore = new MemDocstore
  def docStore: ReflowDocstore = freshDocstore

  class FreshDocstore(pageCount: Int = 1) {
    freshDocstore = new MemDocstore
    loadSampleDoc(pageCount)
  }

  val stableId = DocumentID("stable-id#23")

  def loadSampleDoc(pageCount: Int): Unit = {
    val pages = TextPageSamples.samples
      .take(pageCount)

    createDocumentPagesFromStrings(stableId, pages)
  }

  def reportDocument(stableId: String@@DocumentID): TB.Box = {
    val docBoxes = for {
      docId <- docStore.getDocument(stableId).toSeq
    } yield {
      val pagesBox = for {
        pageId <- docStore.getPages(docId)
      } yield {
        val pageGeometry = docStore.getPageGeometry(pageId)

        val regionBoxes = for {
          regionId <- docStore.getTargetRegions(pageId)
        } yield {
          val targetRegion = docStore.getTargetRegion(regionId)
          // val imageBytes = getTargetRegionImage(regionId)

          val zoneBox = for {
            zoneId <- docStore.getZonesForTargetRegion(regionId)
          } yield {
            docStore.getZone(zoneId).toString().box
          }

          "t: ".box + targetRegion.toString.box
        }

        val pageZoneBoxes = for {
          zoneId <- docStore.getZonesForPage(pageId)
          textReflow <- docStore.getTextReflowForZone(zoneId)
        } yield {
          docStore.getZone(zoneId).toString().box
        }

        (
          indent(2)("PageGeometry")
            % indent(4)(pageGeometry.toString.box)
            % indent(2)("TargetRegions + zones/regions")
            % indent(4)(vcat(regionBoxes))
            % indent(2)("Page Zones")
            % indent(4)(vcat(pageZoneBoxes))
        )
      }

      (s"Document ${docId} (${stableId}) report"
         % vcat(pagesBox)
      )
    }
    vcat(docBoxes)
  }


}
