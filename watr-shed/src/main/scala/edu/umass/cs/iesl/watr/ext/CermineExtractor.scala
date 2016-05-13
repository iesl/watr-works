package edu.umass.cs.iesl.watr
package ext

import com.itextpdf.text.pdf.DocumentFont
import watrmarks._
import play.api.libs.json._
import scala.collection.JavaConversions._
import java.io.InputStream

import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.structure.model._
import scala.collection.JavaConversions._
import com.itextpdf.text.pdf.DocumentFont
import pl.edu.icm.cermine.structure.model.BxBounds

import play.api.libs.json._

import scalaz.@@
import StandardLabels._

object CermineExtractor extends SpatialJsonFormat {
  import cermineZoneUtil._


  def extractCermineZones(pdfis: InputStream): (BxDocument, ZoneRecords) = {
    val conf = new ComponentConfiguration()
    val charExtractor = new XITextCharacterExtractor()
    conf.setCharacterExtractor(charExtractor)

    val d0 = ExtractionUtils.extractCharacters(conf, pdfis)
    val d1 = ExtractionUtils.segmentPages(conf, d0)
    val d2 = ExtractionUtils.resolveReadingOrder(conf, d1);
    val d3 = ExtractionUtils.classifyInitially(conf, d2);
    val d4 = ExtractionUtils.classifyMetadata(conf, d3);


    val fontDict = charExtractor.fontDict
    (d4, charExtractor.getZoneRecords)
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

  import scala.collection.mutable

  def cermineZonesToJson(pdfis: InputStream): String = {

    val (bxDoc, zoneRecords) = extractCermineZones(pdfis)


    def formatBounds(bounds: BxBounds): String = {
      val x = bounds.getX
      val y = bounds.getY
      val w = bounds.getWidth
      val h = bounds.getHeight
      def fmt = (d: Double) => f"${d}%1.2f"
      s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
    }

    val zoneIds = IdGenerator[ZoneID]
    val labelIds = IdGenerator[LabelID]
    val regionIds = IdGenerator[RegionID]

    val zones = mutable.ListMap[Int@@ZoneID, Zone]()
    val zoneAndLabels = mutable.ArrayBuffer[ZoneAndLabel]()

    var zpageNum = -1
    var page0Zones = 0
    var page0Lines = 0
    var page0Tokens = 0
    var page0Chars = 0

    def addZone(label: Label, target: Int, bboxes: LTBounds*): Zone = {

      val zone = Zone(
        zoneIds.nextId,
        bboxes.toList.map(TargetedBounds(regionIds.nextId, PageID(target), _))
      )

      zones.put(zone.id, zone)
      zoneAndLabels.append(zone.withLabel(label))

      zone
    }



    // Serialize zones to json
    bxDoc.asPages.zip(zoneRecords.pageGeometries).zipWithIndex.foreach {
      case ((page, pageGeometry), pageNum) =>
        zpageNum = pageNum

        page.iterator.foreach { zone =>
          val zlabel = modifyZoneLabelName(zone.getLabel.name)
          if (pageNum==0) {
            page0Zones += 1
          }

          addZone(zlabel, pageNum, zone.getBounds.toLTBounds)

          zone.iterator().toList.foreach { line =>
            addZone(Label("bx", "line"), pageNum, line.getBounds.toLTBounds)
            if (pageNum==0) {
              page0Lines += 1
            }

            line.iterator().toList.foreach { token =>
              addZone(Token(token.toText), pageNum, token.getBounds.toLTBounds)
              if (pageNum==0) {
                page0Tokens += 1
              }

              // token.iterator().toList.foreach { chunk =>
              //   if (pageNum==0) {
              //     page0Chars += 1
              //   }
              //   addZone(Label("bx", "char"), pageNum, chunk.getBounds.toLTBounds)
              // }
            }
          }
        }
    }

    // if (zpageNum >= 0) {
    //   println(s"page ${zpageNum}: tokens=${page0Tokens}, chars=${page0Chars} lines=${page0Lines}")
    // }

    val z0 = zoneRecords.copy(
      zones = zones.values.toList,
      labels = zoneAndLabels.toList
    )

    val zoneObj = Json.toJson(z0)

    Json.prettyPrint(zoneObj)
  }

  def loadSpatialIndices(jsvalue: JsValue): ZoneIndexer = {
    jsvalue.validate[ZoneRecords] match {
      case JsSuccess(zoneRecords, path) =>
        ZoneIndexer.loadSpatialIndices(zoneRecords)
      case JsError(err) =>
        sys.error(s"error validating zone records: ${err}")
    }
  }




}
