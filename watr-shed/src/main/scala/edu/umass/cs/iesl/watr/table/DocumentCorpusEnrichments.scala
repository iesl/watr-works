package edu.umass.cs.iesl.watr
package table

import corpora._

import textboxing.{TextBoxing => TB}, TB._
import textgrid._
import _root_.io.circe, circe._, circe.syntax._
import geometry._
import ammonite.{ops => fs}

trait DocumentZoningApiEnrichments extends GeometricFigureCodecs {

  implicit class RicherDocumentZoningApi(val theDocumentZoningApi: DocumentZoningApi) {

    def documents(n: Int=0, skip: Int=0): Seq[String@@DocumentID] = {
      val allEntries = theDocumentZoningApi.getDocuments()
      val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
      val entries = if (n > 0) skipped.take(n) else skipped
      entries
    }
  }


  implicit class RicherStableID(val thisStableId: String@@DocumentID) {
    def getDocument()(implicit docStore: DocumentZoningApi): Int@@DocumentID = {
      docStore.getDocument(thisStableId).getOrElse { sys.error(s"no document ${thisStableId}") }
    }

    def getPages()(implicit docStore: DocumentZoningApi): Seq[Int@@PageID] = {
      docStore.getDocument(thisStableId).toSeq
        .flatMap(docId => docStore.getPages(docId))
    }


    import TextGridLabelWidget._
    import TextGridFunctions._

    def reportDocument()(implicit docStore: DocumentZoningApi): TB.Box = {
      val docBoxes = for {
        docId <- docStore.getDocument(thisStableId).toSeq
      } yield {
        val pagesBox = for {
          pageId <- docStore.getPages(docId)
        } yield {
          val pageGeometry = docStore.getPageGeometry(pageId)

          val allTargetRegions = docStore.getTargetRegions(pageId)

          val regionCount =  s"TargetRegions for page ${pageId}: ${allTargetRegions.length} ".box

          (
            indent(2, "PageGeometry")
              % indent(4, pageGeometry.toString.box)
              % indent(2, regionCount)
              % indent(2, "Page Zones")
          )
        }

        val zoneBoxes = for {
          labelId <- docStore.getZoneLabelsForDocument(docId)
          zoneId <- docStore.getZonesForDocument(docId, labelId)
        } yield {
          val zone = docStore.getZone(zoneId)

          val maybeTextGrid = zone.glyphDefs.map{ str =>
            val textGrid = TextGrid.fromJsonStr(str)
            val indentedBlock = textGridToIndentedBox(textGrid)
            val labelTree = textGridToLabelTree(textGrid)

            val expMarginals = labelTreeToMarginals(labelTree, compactMarginals=false)
            val emarginBlock = marginalGlossToTextBlock(expMarginals)
            val expBlock = emarginBlock + indentedBlock
            expBlock
          } getOrElse { "<no glyphs>".box }

          s"Zone: ${zone.label} # ${zone.id}, @ ".box atop indent(4, maybeTextGrid)
        }
        (s"Document ${docId} (${thisStableId}) report"
          % indent(4, vcat(left,pagesBox))
          % indent(2, "Zones")
          % indent(4, vcat(left,zoneBoxes))
        )
      }
      vcat(left,docBoxes)
    }


    import circe.generic.auto._

    def exportDocumentLabels()(implicit docStore: DocumentZoningApi): Option[Json] = {
      for {
        docId <- docStore.getDocument(thisStableId)
      } yield {
        val pageGeometries = for {
          pageId <- docStore.getPages(docId)
        } yield {
          docStore.getPageGeometry(pageId)
        }

        val zoneJsons = for {
          labelId <- docStore.getZoneLabelsForDocument(docId)
          if ! docStore.getLabel(labelId).fqn.startsWith("seg:")
          zoneId <- docStore.getZonesForDocument(docId, labelId)
        } yield {
          val zone = docStore.getZone(zoneId)
          zone.asJson
        }

        Json.obj(
          "stableId" := thisStableId,
          "pageGeometries" := pageGeometries,
          "zones" := zoneJsons
        )
      }
    }

  }
  def exportAllDocumentLabels()(implicit docStore: DocumentZoningApi): Unit = {
    val totalDocs = docStore.getDocumentCount()
    println(s"Total doc count = ${totalDocs}")
    val outputRoot = fs.pwd / "json-exports.d"
    if (!fs.exists(outputRoot)) {
      fs.mkdir(outputRoot)
    }
    val allDocJsons = docStore.getDocuments().zipWithIndex.flatMap { case (stableId, i) =>
      if (i % 100 == 0) {
        println(s"Output 100 documents...")
      }
      stableId.exportDocumentLabels()
    }
    val uberJson = allDocJsons.asJson

    fs.write(outputRoot / "uber.json", uberJson.noSpaces)
  }

}
