package edu.umass.cs.iesl
package watr
package extract


import watrmarks._
import play.api.libs.json._
// import scala.collection.JavaConversions._
import java.io.InputStream


import _root_.pl.edu.icm.cermine
import cermine.ComponentConfiguration
import cermine.ExtractionUtils
// import cermine.structure.model._
// import cermine.structure.model.BxBounds
// import scala.collection.JavaConversions._
import com.itextpdf.text.pdf.DocumentFont

import play.api.libs.json._

// import scalaz.@@

import Bounds._

object CermineExtractor extends SpatialJsonFormat {
  // import CermineEnrichments._



  def extractChars(
    pdfis: InputStream,
    charsToDebug: Set[Int] = Set()
  ): Seq[(PageChars, PageGeometry)] = {

    val conf = new ComponentConfiguration()
    val charExtractor = new XITextCharacterExtractor(
      charsToDebug,
      IdGenerator[CharID] //, IdGenerator[PageID]
    )
    conf.setCharacterExtractor(charExtractor)

    val _ = ExtractionUtils.extractCharacters(conf, pdfis)

    val pageInfos = charExtractor.pagesInfo

    // Use computed page bounds (based on char bounds) rather than reported page bounds
    pageInfos.map({ case (pchars, pgeom) =>

      val computedBounds =  charBoxesBounds(pchars.chars)

      (pchars,
        pgeom.copy(bounds = computedBounds)
      )

    })

    // charExtractor.pagesInfo
    // val zoneRecs = charExtractor.pagesInfo

    // zoneRecs.chars.zip(zoneRecs.pageGeometries)

  }

  def modifyZoneLabelName(name: String): Label = {
    val Array(pre, post0) = name.toLowerCase.split("_", 2)
    val post = post0.replace("_", "-")

    Label("bx", s"${pre}:${post}")
  }

  def getFontID(fullFontName: String, fontDict: Map[String, (Long, DocumentFont)]): Long = {
    val maybeFont = fontDict.getOrElse(fullFontName, sys.error(s"no font found with fullname =${fullFontName}"))
    maybeFont._1
  }

  // import scala.collection.mutable

  // def cermineZonesToJson(pdfis: InputStream): String = {

  //   val (bxDoc, zoneRecords) = extractCermineZones(pdfis)


  //   def formatBounds(bounds: BxBounds): String = {
  //     val x = bounds.getX
  //     val y = bounds.getY
  //     val w = bounds.getWidth
  //     val h = bounds.getHeight
  //     def fmt = (d: Double) => f"${d}%1.2f"
  //     s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
  //   }

  //   val zoneIds = IdGenerator[ZoneID]
  //   val labelIds = IdGenerator[LabelID]
  //   val regionIds = IdGenerator[RegionID]

  //   val zones = mutable.ListMap[Int@@ZoneID, Zone]()
  //   val zoneAndLabels = mutable.ArrayBuffer[ZoneAndLabel]()

  //   var zpageNum = -1
  //   var page0Zones = 0
  //   var page0Lines = 0
  //   var page0Tokens = 0
  //   var page0Chars = 0

  //   def addZone(label: Label, target: Int, bboxes: LTBounds*): Zone = {

  //     val zone = Zone(
  //       zoneIds.nextId,
  //       bboxes.toList.map(TargetedBounds(regionIds.nextId, PageID(target), _))
  //     )

  //     zones.put(zone.id, zone)
  //     zoneAndLabels.append(zone.withLabel(label))

  //     zone
  //   }



  //   bxDoc.asPages.zip(zoneRecords.pageGeometries).zipWithIndex.foreach {
  //     case ((page, pageGeometry), pageNum) =>
  //       zpageNum = pageNum

  //       page.iterator.foreach { zone =>

  //         // Adding generic Zone label (output of Document segmentation)
  //         addZone(LB.Block, pageNum, zone.getBounds.toLTBounds)
  //         //  ... as well as adding Cermine's classification of the zone
  //         addZone(modifyZoneLabelName(zone.getLabel.name), pageNum, zone.getBounds.toLTBounds)

  //         if (pageNum==0) {
  //           page0Zones += 1
  //         }

  //         zone.iterator().toList.foreach { line =>
  //           addZone(LB.Line, pageNum, line.getBounds.toLTBounds)
  //           if (pageNum==0) {
  //             page0Lines += 1
  //           }

  //           line.iterator().toList.foreach { token =>
  //             addZone(LB.Token(token.toText), pageNum, token.getBounds.toLTBounds)
  //             if (pageNum==0) {
  //               page0Tokens += 1
  //             }

  //             // token.iterator().toList.foreach { chunk =>
  //             //   if (pageNum==0) {
  //             //     page0Chars += 1
  //             //   }
  //             //   addZone(Label("bx", "char"), pageNum, chunk.getBounds.toLTBounds)
  //             // }
  //           }
  //         }
  //       }
  //   }

  //   // if (zpageNum >= 0) {
  //   //   println(s"page ${zpageNum}: tokens=${page0Tokens}, chars=${page0Chars} lines=${page0Lines}")
  //   // }

  //   val z0 = zoneRecords.copy(
  //     zones = zones.values.toList,
  //     labels = zoneAndLabels.toList
  //   )

  //   val zoneObj = Json.toJson(z0)

  //   Json.prettyPrint(zoneObj)
  // }

  def loadSpatialIndices(jsvalue: JsValue): ZoneIndexer = {
    jsvalue.validate[ZoneRecords] match {
      case JsSuccess(zoneRecords, path) =>
        ZoneIndexer.loadSpatialIndices(zoneRecords)
      case JsError(err) =>
        sys.error(s"error validating zone records: ${err}")
    }
  }




}
