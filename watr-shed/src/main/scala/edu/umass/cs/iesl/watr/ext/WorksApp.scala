package edu.umass.cs.iesl.watr
package ext


import edu.umass.cs.iesl.watr.watrmarks._
import java.io.InputStream

import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.structure.model._
import scala.collection.JavaConversions._
import com.itextpdf.text.pdf.DocumentFont
import pl.edu.icm.cermine.structure.model.BxBounds

import better.files._
import play.api.libs.json._

import scalaz.@@

object Works extends App {

  import java.io.{File => JFile}

  case class AppConfig(
    file: JFile = new JFile(""),
    force: Boolean = false
  )

  val parser = new scopt.OptionParser[AppConfig]("scopt") {
    head("itext zone extractor", "0.1")

    note("(add description)")

    opt[Unit]('x', "overwrite") action { (v, opts) =>
      opts.copy(force = true) } text("force overwrite of existing files")

    opt[JFile]('f', "file") action { (v, opts) =>
      opts.copy(file = v) } text("input file or directory")

  }


  val config = parser.parse(args, AppConfig()).getOrElse{
    sys.error(parser.usage)
  }

  val f = File(config.file.getPath)

  if (f.isDirectory) {
    f.glob("**/*.pdf").foreach { pdf =>
      val artifactPath = s"${pdf.path}.d".toFile
      if (!artifactPath.exists) {
        Cmds.mkdir(artifactPath)
      }

      val output = s"${artifactPath}/cermine-zones.json".toFile
      val fileSha1 = DigestUtils.shaHex(pdf.byteArray)

      pdf.inputStream.map { is =>
        if (!output.isReadable || config.force) {
          println(s"processing ${pdf}, force=${config.force}")
          output < cermineZoneUtil.cermineZonesToJson(fileSha1, is)
        } else {
          println(s"skipping $pdf")
        }
      }
    }
  } else if (f.isReadable) {

    val artifactPath = s"${f.path}.d".toFile
    val fileSha1 = DigestUtils.shaHex(f.byteArray)

    println("file sha1: "+fileSha1)

    f.inputStream.map { is =>
      val output = s"${artifactPath}/cermine-zones.json".toFile
      if (!output.isReadable || config.force) {
        println(s"processing ${f}, force=${config.force}")
        output < cermineZoneUtil.cermineZonesToJson(fileSha1, is)
      } else {
        println(s"skipping $f")
      }
    }
  } else {
    sys.error("please specify a file or directory")
  }
}

object cermineZoneUtil extends SpatialJsonFormat {

  implicit class RicherLTBounds(val bb: LTBounds) extends AnyVal {
    def toBxBounds: BxBounds = {
     new BxBounds(bb.left, bb.top, bb.width, bb.height)
    }

  }

  implicit class RicherBxBounds(val tb: BxBounds) extends AnyVal {
    def toLTBounds: LTBounds = {
      LTBounds(
        left = tb.getX,
        top =  tb.getY,
        width = tb.getWidth,
        height = tb.getHeight
      )

    }
  }


  def formatLabel(l: BxZoneLabel): String = {
    val cat = l.getCategory
    val gen = l.getGeneralLabel
    s"""label=$l, category = $cat, general = $gen """
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

  def extractCermineZones(fileSha1: String, pdfis: InputStream): (BxDocument, ZoneRecords) = {
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

  def cermineZonesToJson(fileSha1: String, pdfis: InputStream): String = {

    val (bxDoc, zoneRecords) = extractCermineZones(fileSha1, pdfis)


    def formatBounds(bounds: BxBounds): String = {
      val x = bounds.getX
      val y = bounds.getY
      val w = bounds.getWidth
      val h = bounds.getHeight
      def fmt = (d: Double) => f"${d}%1.2f"
      s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
    }



    import scala.collection.mutable
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
      if (zpageNum==0) {
        println(s"""adding zone ${label}, bboxes = ${bboxes.mkString(", ")}""")
      }
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
              addZone(Label("bx", "token"), pageNum, token.getBounds.toLTBounds)
              if (pageNum==0) {
                page0Tokens += 1
              }

              token.iterator().toList.foreach { chunk =>
                if (pageNum==0) {
                  page0Chars += 1
                }
                addZone(Label("bx", "char"), pageNum, chunk.getBounds.toLTBounds)
              }
            }
          }
        }
    }

    println(s"added to page 0: tokens=${page0Tokens}, chars=${page0Chars} lines=${page0Lines}")

    val z0 = zoneRecords.copy(
      zones = zones.values.toList,
      labels = zoneAndLabels.toList
    )

    val zoneObj = Json.toJson(z0)

    Json.prettyPrint(zoneObj)
  }

  import scala.sys.process._
  // val pdfToSVGPath = cwd/up/"iesl-pdf-to-text"
  // val pdfToSVGExe = pdfToSVGPath/"bin"/"run.sh"
  val pdf2svg = "ext/iesl-pdf-to-text/bin/run.sh"

  def pdfToSVG(pdfInput: File, outputPath: File): Unit = {
    println("running: " + pdf2svg)

    // "ls" #| "grep .scala" #&& Seq("sh", "-c", "scalac *.scala") #|| "echo nothing found" lines
    println(s"running: ${pdf2svg} on ${pdfInput} -> ${outputPath}")

    val result = List(pdf2svg, "-i", pdfInput.toString, "-o", outputPath.toString()).!

  }


}
