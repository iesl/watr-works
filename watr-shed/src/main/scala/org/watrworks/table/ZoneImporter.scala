package org.watrworks
package table


import TypeTags._
import watrmarks._
import corpora._
import utils.DoOrDieHandlers._

import ammonite.{ops => fs}
import _root_.io.circe, circe._
import circe.generic.auto._
import circe.generic._
import geometry._
import GeometryCodecs._

object OldZoneFormats extends TypeTagCodecs  {

  case class DocZones(
    stableId: String,
    pageGeometries: Seq[LTBounds],
    zones: Seq[Zone]
  )

  @JsonCodec
  case class Zone(
    id: Int,
    regions: Seq[PageRegion],
    label: String,
    order: Int,
    glyphDefs: Option[String] = None
  )

}

object ImportZoneFormatToAnnot {
  import OldZoneFormats._
  val R = RelationModel

  import utils.PathUtils._

  def checkZoneImportsForMissingCorpusEntries(jsonFilename: String)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val docStore = corpusAccessApi.docStore

    val jsonStr = fs.read(jsonFilename.toPath())
    val jsonSeq = jsonStr.decodeOrDie[List[Json]]()

    val oldDocZones = jsonSeq.map{ _.decodeOrDie[DocZones]("Error decoding doc zone") }

    println(s"decoded ${jsonSeq.length} old zone sets")

    oldDocZones.foreach { docZones =>
      print(s"checking ${docZones.stableId}..  ")
      val maybeDocs = docZones.zones.map{ docZone =>
        val newZone = AnnotatedLocation.Zone(docZone.regions)
        val stableId = newZone.stableId
        docStore.getDocument(stableId)
      }
      if (maybeDocs.exists(_.isEmpty)) {
        println(s"ERROR: missing pdf")
      } else {
        println(s"ok.")
      }
    }
  }


  def importZoneUberJson(jsonFilename: String, pathSuffix: String)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore

    val jsonStr = fs.read(jsonFilename.toPath())
    val jsonSeq = jsonStr.decodeOrDie[List[Json]]()

    val oldDocZones = jsonSeq.map{ _.decodeOrDie[DocZones]("Error decoding doc zone") }
    println(s"decoded ${jsonSeq.length} old zone sets")

    oldDocZones.foreach { docZones =>
      val stableId = DocumentID(docZones.stableId)
      println(s"importing ${stableId}")
      if (docStore.getDocument(stableId).isEmpty) {
        println(s"Adding document ${stableId} with ${docZones.pageGeometries.length} pages")
        val docId = docStore.addDocument(stableId)
        docZones.pageGeometries.zipWithIndex.foreach {  case (pageBounds, n) =>
          val pageNum = PageNum(n)
          val pageId = docStore.addPage(docId, pageNum)
          docStore.setPageGeometry(pageId, pageBounds)
        }
      }
      docZones.zones.foreach{ docZone =>
        val label = Labels.fromString(docZone.label)
        val newZone = AnnotatedLocation.Zone(docZone.regions)
        val docId = docStore.getDocument(stableId).orDie("Document should have been created by this point")

        val annotId = annotApi.createAnnotation(label, newZone)
        annotApi.setCorpusPath(annotId, CorpusPath(s"Bioarxiv.UmaCzi2018.${pathSuffix}"))

        docZone.glyphDefs.foreach { glyphs =>
          annotApi.updateBody(annotId, AnnotationBody.TextGrid(glyphs))
        }

      }
    }
  }
}
