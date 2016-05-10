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

}
object CermineUtils extends SpatialJsonFormat {
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
              addZone(Label("bx", "token"), pageNum, token.getBounds.toLTBounds)
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

    println(s"added to page 0: tokens=${page0Tokens}, chars=${page0Chars} lines=${page0Lines}")

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

  def reportFontInfo(font: DocumentFont): Unit = {
    val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
    val allNameEntries = font.getAllNameEntries.map(_.mkString("[", ",", "]")).mkString(", ")
    val fontDictionary = font.getFontDictionary
    val fontDictionaryKeys = fontDictionary.getKeys.map(_.toString()).mkString(",")
    val unicodeDiffs = font.getUnicodeDifferences.dropWhile(_ == "").mkString(",")
    val diffs = font.getDifferences.dropWhile(_ == null).mkString(",")

    // debugReport(
    //   allNameEntries,
    //   // font.getCharBBox,
    //   // font.getFontDictionary,
    //   fontDictionaryKeys,
    //   // font.getFontMatrix,
    //   fontFullname,
    //   font.getFullFontStream,
    //   // font.getKerning,
    //   font.getPostscriptFontName,
    //   // font.getWidth,
    //   // font.getWidth,
    //   font.hasKernPairs,
    //   font.isVertical,
    //   // font.getAscent,
    //   // font.getAscentPoint,
    //   // font.getCidCode,
    //   // font.getCodePagesSupported.mkString(", "),
    //   // font.getCompressionLevel,
    //   // font.getDescent,
    //   // font.getDescentPoint,
    //   font.getEncoding,
    //   font.getFontType,
    //   // font.getSubfamily,
    //   unicodeDiffs,
    //   diffs,
    //   // font.getUnicodeEquivalent,
    //   // font.getWidthPoint,
    //   // font.getWidthPoint,
    //   // font.getWidthPointKerned,
    //   // font.getWidths.mkString(", "),
    //   font.isDirectTextToByte,
    //   font.isEmbedded,
    //   font.isFontSpecific,
    //   font.isForceWidthsOutput,
    //   font.isSubset
    // )

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


  import scala.collection.JavaConversions._

  import com.itextpdf.text.pdf._
  import com.itextpdf.text.pdf.parser.{Vector => PVector, _}

  def outputCharInfo(tri: TextRenderInfo, reader: PdfReader): Unit = {
    val font = tri.getFont()
    // val text2 = font.getUnicodeEquivalent(tri.getMcid)
    val pdfstring = tri.getPdfString
    val ffs = font.getFullFontStream

    // val dictKvs = "Type,Subtype,Name/BaseFont".split(",").map{key =>
    //   font.getFontDictionary.get(new PdfName(s"/$key"))
    // }.mkString("\n")

    val asString = tri.getText.toCharArray().map{c =>
      Char.char2int(c).toString
    }.mkString

    val bs = pdfstring.getBytes.map(Byte.byte2int(_)).mkString(",")
    val obs = pdfstring.getOriginalBytes.map(Byte.byte2int(_)).mkString(",")

    val pdfstrInf = s"""|${tri.getText}:  '${asString}'
                        |    getBytes         ${bs} => Array[Byte]
                        |    getEncoding      ${pdfstring.getEncoding} => String
                        |    getOriginalBytes ${obs} => Array[Byte]
                        |    isHexWriting     ${pdfstring.isHexWriting        } => Boolean
                        |    toString         ${pdfstring.toString            } => String
                        |    toUnicodeString  ${pdfstring.toUnicodeString     } => String
                        |    canBeInObjStm    ${pdfstring.canBeInObjStm       } => Boolean
                        |    getIndRef        ${pdfstring.getIndRef           } => PRIndirectReference
                        |    isArray          ${pdfstring.isArray             } => Boolean
                        |    isBoolean        ${pdfstring.isBoolean           } => Boolean
                        |    isDictionary     ${pdfstring.isDictionary        } => Boolean
                        |    isIndirect       ${pdfstring.isIndirect          } => Boolean
                        |    isName           ${pdfstring.isName              } => Boolean
                        |    isNull           ${pdfstring.isNull              } => Boolean
                        |    isNumber         ${pdfstring.isNumber            } => Boolean
                        |    isStream         ${pdfstring.isStream            } => Boolean
                        |    isString         ${pdfstring.isString            } => Boolean
                        |    length           ${pdfstring.length              } => Int
                        |    type             ${pdfstring.`type`              } => Int
                        |
                        |""".stripMargin

    val d0 = font.getDifferences
    val d1 = font.getUnicodeDifferences
    val dict  = font.getFontDictionary

    val dictKvs = font.getFontDictionary.getKeys.map{ key =>
      "    " +key.toString() + ": " + font.getFontDictionary.get(key)
    }.mkString("\n")


    val diffStr = d0.mkString(",")
    val udiffStr = d1.mkString(",")

    val unicodeEquiv = font.getUnicodeEquivalent(0)
      // |    diffs             ${diffStr}
      // |    unidiffs          ${udiffStr}

    object formatting {
      import watrmarks.TB._

      def formatPdfObject(obj: PdfObject): String = {
        renderBox(formatObject(obj)).mkString("\n")
      }

      def formatObject(obj: PdfObject): Box = {
        if (obj.isName) {
          s"name:$obj".box
        } else if (obj.isIndirect()) {
          val xo = obj.asInstanceOf[PdfIndirectReference]
          val xo2 = obj.asInstanceOf[PdfIndirectObject]
          val sub = reader.getPdfObject(xo.getNumber)
          "indirect:" beside formatObject(sub)
        } else if (obj.isStream()) {
          val xo = obj.asInstanceOf[PdfStream]
          // new java.io.OutputStreamWriter(new java.io.StringWriter())
          val sw = new java.io.StringWriter()

          val baos = new java.io.ByteArrayOutputStream()
          xo.writeContent(baos)
          val s = baos.toByteArray().mkString("[", ",", "]")
          // val rlen = xo.getRawLength
          // val bytes = if (rlen>0 && xo.getBytes != null) {
          //   xo.getBytes.mkString("")
          // } else "[]"

          val other = vcat(center2)(List(
            "bytes" besideS s
          ))

          "stream:" atop indent(3)(
            other atop vcat(center1)(
              xo.getKeys.toList.map{ key =>
                val dval = xo.get(key)

                val valBox = if (dval.isIndirect()) {
                  val xo = dval.asInstanceOf[PdfIndirectReference]
                  val sub = reader.getPdfObject(xo.getNumber)
                  formatObject(sub)
                } else if (dval.isStream()) {
                  formatObject(dval)
                } else {
                  formatObject(dval)
                }
                s"${key}:".box beside valBox
              }
            ))
        } else if (obj.isDictionary) {
          val xo = obj.asInstanceOf[PdfDictionary]

          "dict:" atop indent(3)(
            vcat(center1)(
              xo.getKeys.toList.map{ key =>
                val dval = xo.get(key)

                val valBox = if (dval.isIndirect()) {
                  val xo = dval.asInstanceOf[PdfIndirectReference]
                  val sub = reader.getPdfObject(xo.getNumber)
                  formatObject(sub)
                } else if (dval.isStream()) {
                  formatObject(dval)
                } else {
                  formatObject(dval)
                }
                s"${key}:".box beside valBox
              }
            )
          )
        } else {
          s"<${obj}>".box
        }
      }
    }

    val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
    val charProcs = dict.getAsDict(new PdfName("CharProcs"))
    val charProcInf = if (charProcs != null) {
      formatting.formatPdfObject(charProcs).toString()
    } else "<no CharProcs>"


    val fontinf = s"""|${tri.getText}
                      |    Font              ${fontFullname}      => DocumentFont
                      |       Unic.Equiv     ${unicodeEquiv}      => Int
                      |    Mcid              ${tri.getMcid}              => Integer
                      |    PdfString         ${tri.getPdfString}              => PdfString
                      |    TextRenderMode    ${tri.getTextRenderMode}              => Int
                      | ${dictKvs}
                      | ${charProcInf}
                      |""".stripMargin

    def formatLineVector(ls: PVector): String = {
      s"""[${ls.get(0)} ${ls.get(1)}, ${ls.get(2)}}}]"""
    }

    def formatLineSegment(ls: LineSegment): String = {
      s""" ${formatLineVector(ls.getStartPoint)} -> ${formatLineVector(ls.getEndPoint)} ${ls.getLength}}"""

    }



    // val bbinf = s"""|${tri.getText}:  '${asString}'
    //                 |    Rise              ${tri.getRise}                                => Float
    //                 |    AscentLine        ${formatLineSegment(tri.getAscentLine)}       => LineSegment
    //                 |    Baseline          ${formatLineSegment(tri.getBaseline)}         => LineSegment
    //                 |    Baseline (uns)    ${formatLineSegment(tri.getUnscaledBaseline)} => LineSegment
    //                 |    DescentLine       ${formatLineSegment(tri.getDescentLine)}      => LineSegment
    //                 |    Mcid              ${tri.getMcid}              => Integer
    //                 |    PdfString         ${tri.getPdfString}              => PdfString
    //                 |""".stripMargin

    // println(bbinf)


    // if (chars.length > 20) {
    //   output << chars.mkString
    //   chars.clear()
    // }

    // chars.append(tri.getText)

    // val _ = output << pdfstrInf
    // output << fontinf

    // | SingleSpaceWidth  ${tri.getSingleSpaceWidth }                 => Float
    // | StrokeColor       ${tri.getStrokeColor      }              => BaseColor
    // | FillColor         ${tri.getFillColor        }              => BaseColor
  }



}
