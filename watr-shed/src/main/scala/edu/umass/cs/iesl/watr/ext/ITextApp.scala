package edu.umass.cs.iesl.watr
package ext


import edu.umass.cs.iesl.watr.watrmarks._
import java.io.InputStream

import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.structure.model._
import scala.collection.JavaConversions._
import com.itextpdf.text.pdf.DocumentFont

import better.files._
import play.api.libs.json._

import scalaz.@@

object ITextPdfToSvg extends App {

  import java.io.{File => JFile}

  case class AppConfig(
    file: JFile = new JFile(""),
    force: Boolean = false
  )

  val parser = new scopt.OptionParser[AppConfig]("scopt") {
    head("itext zone extractor", "0.1")


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
          output < cermineZoneUtil.cermineZonesToSVG(fileSha1, is)
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
        output < cermineZoneUtil.cermineZonesToSVG(fileSha1, is)
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

  import pl.edu.icm.cermine.structure.model.BxBounds
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


  // def bxBoundsToSpatialBounds(bounds: BxBounds): spatialindex.Bounds = {
  //   spatialindex.regions.bbox(
  //     bounds.getX,
  //     bounds.getY,
  //     bounds.getWidth,
  //     bounds.getHeight
  //   )
  // }

  def reportFontInfo(font: DocumentFont): Unit = {
    val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
    val allNameEntries = font.getAllNameEntries.map(_.mkString("[", ",", "]")).mkString(", ")
    val fontDictionary = font.getFontDictionary
    val fontDictionaryKeys = fontDictionary.getKeys.map(_.toString()).mkString(",")
    val unicodeDiffs = font.getUnicodeDifferences.dropWhile(_ == "").mkString(",")
    val diffs = font.getDifferences.dropWhile(_ == null).mkString(",")

    debugReport(
      allNameEntries,
      // font.getCharBBox,
      // font.getFontDictionary,
      fontDictionaryKeys,
      // font.getFontMatrix,
      fontFullname,
      font.getFullFontStream,
      // font.getKerning,
      font.getPostscriptFontName,
      // font.getWidth,
      // font.getWidth,
      font.hasKernPairs,
      font.isVertical,
      // font.getAscent,
      // font.getAscentPoint,
      // font.getCidCode,
      // font.getCodePagesSupported.mkString(", "),
      // font.getCompressionLevel,
      // font.getDescent,
      // font.getDescentPoint,
      font.getEncoding,
      font.getFontType,
      // font.getSubfamily,
      unicodeDiffs,
      diffs,
      // font.getUnicodeEquivalent,
      // font.getWidthPoint,
      // font.getWidthPoint,
      // font.getWidthPointKerned,
      // font.getWidths.mkString(", "),
      font.isDirectTextToByte,
      font.isEmbedded,
      font.isFontSpecific,
      font.isForceWidthsOutput,
      font.isSubset
    )

    // charExists            (Int)           => Boolean
    // convertToBytes        (String)        => Array[Byte]
    // getAllNameEntries     ()              => Array[Array[String]]
    // getCharBBox           (Int)           => Array[Int]
    // getFamilyFontName     ()              => Array[Array[String]]
    // getFontDescriptor     (Int, Float)    => Float
    // getFontDictionary     ()              => PdfDictionary
    // getFontMatrix         ()              => Array[Double]
    // getFullFontName       ()              => Array[Array[String]]
    // getFullFontStream     ()              => PdfStream
    // getKerning            (Int, Int)      => Int
    // getPostscriptFontName ()              => String
    // getWidth              (String)        => Int
    // getWidth              (Int)           => Int
    // hasKernPairs          ()              => Boolean
    // isVertical            ()              => Boolean
    // setKerning            (Int, Int, Int) => Boolean
    // setPostscriptFontName (String)        => Unit

    // com.itextpdf.text.pdf.BaseFont
    // ---------------------------
    // addSubsetRange        (Array[Int])    => Unit
    // correctArabicAdvance  ()              => Unit
    // getAscent             (String)        => Int
    // getAscentPoint        (String, Float) => Float
    // getCidCode            (Int)           => Int
    // getCodePagesSupported ()              => Array[String]
    // getCompressionLevel   ()              => Int
    // getDescent            (String)        => Int
    // getDescentPoint       (String, Float) => Float
    // getDifferences        ()              => Array[String]
    // getEncoding           ()              => String
    // getFontType           ()              => Int
    // getSubfamily          ()              => String
    // getUnicodeDifferences ()              => Array[Char]
    // getUnicodeEquivalent  (Int)           => Int
    // getWidthPoint         (String, Float) => Float
    // getWidthPoint         (Int, Float)    => Float
    // getWidthPointKerned   (String, Float) => Float
    // getWidths             ()              => Array[Int]
    // isDirectTextToByte    ()              => Boolean
    // isEmbedded            ()              => Boolean
    // isFontSpecific        ()              => Boolean
    // isForceWidthsOutput   ()              => Boolean
    // isSubset              ()              => Boolean
    // setCharAdvance        (Int, Int)      => Boolean
    // setCompressionLevel   (Int)           => Unit
    // setDirectTextToByte   (Boolean)       => Unit
    // setFontDescriptor     (Int, Float)    => Unit
    // setForceWidthsOutput  (Boolean)       => Unit
    // setSubset             (Boolean)       => Unit
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


    // val pageSpatialInfo = charExtractor
    val fontDict = charExtractor.fontDict
    (d4, charExtractor.getZoneRecords)
  }

  def cermineZonesToSVG(fileSha1: String, pdfis: InputStream): String = {

    val (bxDoc, zoneRecords) = extractCermineZones(fileSha1, pdfis)


    // val pageSpatialInfo = charExtractor.spatialInfo
    // val fontDict = charExtractor.fontDict

    // fontDict.foreach {
    //   case (key, font) =>
    //     reportFontInfo(font)
    // }

    def formatBounds(bounds: BxBounds): String = {
      val x = bounds.getX
      val y = bounds.getY
      val w = bounds.getWidth
      val h = bounds.getHeight
      def fmt = (d: Double) => f"${d}%1.2f"
      s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
    }


    object annotationTags extends ScalatagsDefs {

      def zoneExtents(zone: BxZone) = Seq(
        zone.getBounds.getX.attrX,
        zone.getBounds.getY.attrY,
        zone.getBounds.getWidth.attrWidth,
        zone.getBounds.getHeight.attrHeight)

      def zoneExtents(tb: LBBounds) = Seq(
        tb.left.attrX,
        tb.bottom.attrY,
        tb.width.attrWidth,
        tb.height.attrHeight)

    }


    import scala.collection.mutable
    val zoneIds = IdGenerator[ZoneID]
    val labelIds = IdGenerator[LabelID]




    val zones = mutable.HashMap[Int@@ZoneID, Zone]()
    val zoneAndLabels = mutable.ArrayBuffer[ZoneAndLabel]()

    def addZone(label: Label, target: Int, bboxes: LTBounds*): Zone = {
      val zone = Zone(
        zoneIds.nextId,
        bboxes.toList.map(TargetedBounds(PageID(target), _))
      )

      zones.put(zone.id, zone)
      zoneAndLabels.append(zone.withLabel(label))

      zone
    }


    // Serialize zones to json
    bxDoc.asPages.zip(zoneRecords.pageGeometries).zipWithIndex.foreach {
      case ((page, pageGeometry), pageNum) =>

        page.iterator.foreach { zone =>
          val zlabel = modifyZoneLabelName(zone.getLabel.name)

          addZone(zlabel, pageNum, zone.getBounds.toLTBounds)

          zone.iterator().toList.foreach { line =>
            addZone(Label("bx", "line"), pageNum, line.getBounds.toLTBounds)

            line.iterator().toList.foreach { token =>
              addZone(Label("bx", "token"), pageNum, token.getBounds.toLTBounds)

              token.iterator().toList.foreach { chunk =>
                addZone(Label("bx", "char"), pageNum, chunk.getBounds.toLTBounds)
              }
            }
          }
        }
    }

    // zoneRecords.copy(zones = zones.toList)

    // emit zone/label records
    val zoneAndLabelJson = zoneAndLabels.map { case ZoneAndLabel(zoneID, label) =>
      // val zoneId = ZoneID.unwrap(zoneID)
      val zone = zones(zoneID)

      val zoneBboxes = zone.bboxes.map({b =>
        val pageTargetId = Json.toJson(PageID.unwrap(b.target))
        val bboxList: JsArray = Json.toJson(util.listBounds(b.bbox.toBxBounds).map(JsNumber(_))).asInstanceOf[JsArray]
        pageTargetId +: bboxList
      })

      val zoneJson = Json.obj(
        "id" -> ZoneID.unwrap(zoneID),
        "bboxes" -> zoneBboxes
      )

      val labelId = LabelID.unwrap(labelIds.nextId)

      val labelJson = Json.toJson(label)

      (zoneJson, labelJson)
    }

    val zonesJson = zoneAndLabelJson.map(_._1)
    val labelsJson = zoneAndLabelJson.map(_._2)

    val pageGeom = zoneRecords.pageGeometries.map{ geometry =>
      Json.toJson(geometry)
      // Json.obj(
      //   "id" -> geometry.id,
      //   "bounds" -> Json.obj(
      //     "left"   -> geometry.bounds.left,
      //     "bottom" -> geometry.bounds.bottom,
      //     "width"  -> geometry.bounds.width,
      //     "height" -> geometry.bounds.height
      //   ),
      //   "borders" -> geometry.borders.map{borders =>
      //     Json.obj(
      //       "left"   ->borders.bleft ,
      //       "right"  ->borders.bright ,
      //       "top"    ->borders.btop ,
      //       "bottom" ->borders.bbottom
      //     )
      //   }
      // )
    }

    val zoneObj = Json.obj(
      // "id" -> "todo",
      // "target" -> "todo",
      "pageGeometries" -> pageGeom,
      "zones" -> zonesJson,
      "labels" -> labelsJson
    )

    Json.prettyPrint(zoneObj)
  }

}
