package edu.umass.cs.iesl.watr
package extract
package fonts


import _root_.com.itextpdf
import com.itextpdf.io.font.{ FontProgramFactory, _ }
import com.itextpdf.io.font.otf.Glyph
import com.itextpdf.kernel.pdf.PdfName
// import com.itextpdf.io.font.{ FontIdentification, FontNames, FontProgram }
import com.itextpdf.kernel.font.PdfFont
import com.itextpdf.kernel.geom.LineSegment
import com.itextpdf.kernel.pdf.{ PdfArray, PdfIndirectReference, PdfStream, PdfString }
import itextpdf.kernel.pdf.PdfReader
import com.itextpdf.kernel.pdf.PdfDictionary;
import com.itextpdf.kernel.pdf.PdfObject
import itextpdf.kernel.pdf.canvas.parser.data._


import com.itextpdf.kernel.geom.{Vector => PVector}

import scala.collection.JavaConversions._
import textboxing.{TextBoxing => TB}

object DocumentFontInfo {

  import TB._

  def getPdfStringInfo(pdfFont: PdfFont, pdfString: PdfString, reader: PdfReader): Box = {

    val bs = pdfString.getValueBytes.map(Byte.byte2int(_))
    val b0 = bs(0)
    val fprogram = pdfFont.getFontProgram
    // val pglyph = fprogram.getGlyph(b0)
    val pglyphByCode = fprogram.getGlyphByCode(b0)

    val decoded = pdfFont.decode(pdfString)


    val bsstr = bs.mkString("bytes:[", ", ", "]")

    s"""|PdfString:
        |   decoded: ${decoded}
        |   getEncoding     ${pdfString.getEncoding      }   () => String
        |   getType         ${pdfString.getType          }   () => Byte
        |   getValue        ${pdfString.getValue         }   () => String
        |   getValueBytes   ${bsstr}                         () => Array[Byte]
        |   isHexWriting    ${pdfString.isHexWriting     }   () => Boolean
        |   toString        ${pdfString.toString         }   () => String
        |   toUnicodeString ${pdfString.toUnicodeString  }   () => String
        |""".stripMargin.mbox
  }

  def getCharTriInfo(tri: TextRenderInfo, reader: PdfReader, recurse: Boolean = false): Box = {
    val textRenderInfoStr =
    s"""|TextRenderInfo =>
        |     getActualText           ${tri.getActualText             }
        |     getAscentLine           ${outputLineSegmentInfo(tri.getAscentLine)}
        |     getBaseline             ${outputLineSegmentInfo(tri.getBaseline)}
        |     getCanvasTagHierarchy   ${tri.getCanvasTagHierarchy     }
        |     getCharSpacing          ${tri.getCharSpacing            }
        |     getDescentLine          ${outputLineSegmentInfo(tri.getDescentLine)}
        |     getFillColor            ${tri.getFillColor.getColorValue.mkString(", ")}
        |     getFont                 (below)
        |     getFontSize             ${tri.getFontSize               }
        |     getHorizontalScaling    ${tri.getHorizontalScaling      }
        |     getLeading              ${tri.getLeading                }
        |     getMcid                 ${tri.getMcid                   }
        |     getPdfString            ${tri.getPdfString              }
        |     getRise                 ${tri.getRise                   }
        |     getSingleSpaceWidth     ${tri.getSingleSpaceWidth       }
        |     getStrokeColor          ${tri.getStrokeColor            }
        |     getText                 ${tri.getText                   }
        |     getTextRenderMode       ${tri.getTextRenderMode         }
        |     getUnscaledBaseline     ${outputLineSegmentInfo(tri.getUnscaledBaseline)}
        |     getUnscaledWidth        ${tri.getUnscaledWidth          }
        |     getWordSpacing          ${tri.getWordSpacing            }
        |     getCharacterRenderInfos:
        |""".stripMargin.mbox

    val pdfString = tri.getPdfString

    val bs = pdfString.getValueBytes.map(Byte.byte2int(_))
    val b0 = bs(0)
    val font = tri.getFont
    val fprogram = font.getFontProgram
    // val pglyph = fprogram.getGlyph(b0)
    val pglyphByCode = fprogram.getGlyphByCode(b0)


    val pdfStrInfo = getPdfStringInfo(tri.getFont, tri.getPdfString, reader)

    (textRenderInfoStr atop
      indent(2)(outputFontInfo(tri.getFont, reader, None)) atop
      indent(2)(pdfStrInfo)
    )
  }

  def outputVectorInfo(vec: PVector): String = {
    s"(${vec.get(0)}, ${vec.get(1)})"
  }

  def outputLineSegmentInfo(ls: LineSegment): String = {
    val sv = ls.getStartPoint
    val ev = ls.getEndPoint
    val l = ls.getLength

    s"ln[${outputVectorInfo(sv)}, ${outputVectorInfo(ev)} (l:${l})]"
  }

  import scala.reflect.runtime.universe._

  def typesOf[T : TypeTag](v: T): List[Type] = typeOf[T].baseClasses.map(typeOf[T].baseType)



  def outputFontInfo(pdfFont: PdfFont, reader: PdfReader, maybeGlyph:Option[Glyph] = None): Box = {

    val glyphBytes = maybeGlyph.map(pdfFont.convertToBytes(_)).map(_.mkString(",")).getOrElse("none")

    val registered = FontProgramFactory.getRegisteredFonts.mkString(", ")

    val fmtPdfObject = formatting.formatPdfObject(
      pdfFont.getPdfObject, reader
    )

    // |    reg: ${registered}

    val fontHdr =
    s"""|Font:
        |    Glyph bytes   ${glyphBytes}
        |    getFontMatrix ${pdfFont.getFontMatrix }      () => Array[Double]
        |    isEmbedded    ${pdfFont.isEmbedded    }      () => Boolean
        |    isSubset      ${pdfFont.isSubset      }      () => Boolean
        |    getFontProgram =>
        |""".stripMargin.mbox

    val fontPrgm = indent(6)(outputFontProgramInfo(pdfFont.getFontProgram, reader))

    (fontHdr atop
      fontPrgm atop
      "Font Object" atop
      indent(8)(fmtPdfObject)
    )




  }

  def outputFontNameInfo(fontNames: FontNames): Box = {
    s"""|FontName:
        |   allowEmbedding   ${fontNames.allowEmbedding }     () => Boolean
        |   getCidFontName   ${fontNames.getCidFontName }     () => String
        |   getFamilyName    ${fontNames.getFamilyName  }     () => Array[Array[String]]
        |   getFontName      ${fontNames.getFontName    }     () => String
        |   getFontWeight    ${fontNames.getFontWeight  }     () => Int
        |   getFontWidth     ${fontNames.getFontWidth   }     () => Int
        |   getFullName      ${fontNames.getFullName    }     () => Array[Array[String]]
        |   getStyle         ${fontNames.getStyle       }     () => String
        |   getSubfamily     ${fontNames.getSubfamily   }     () => String
        |   isBold           ${fontNames.isBold         }     () => Boolean
        |   isCondensed      ${fontNames.isCondensed    }     () => Boolean
        |   isExtended       ${fontNames.isExtended     }     () => Boolean
        |   isItalic         ${fontNames.isItalic       }     () => Boolean
        |   isOutline        ${fontNames.isOutline      }     () => Boolean
        |   isShadow         ${fontNames.isShadow       }     () => Boolean
        |   isUnderline      ${fontNames.isUnderline    }     () => Boolean
        |""".stripMargin.mbox
  }

  def outputFontIdentificationInfo(fontId: FontIdentification, reader: PdfReader): Box = {

    s"""|FontIdentification:
        |    getPanose       ${fontId.getPanose       }
        |    getTtfUniqueId  ${fontId.getTtfUniqueId  }
        |    getTtfVersion   ${fontId.getTtfVersion   }
        |    getType1Xuid    ${fontId.getType1Xuid    }
        |""".stripMargin.mbox
  }



  def outputGlyphInfo(glyph: Glyph, reader: PdfReader): Box = {
    s"""|Glyph:
        |    getAnchorDelta    ${glyph.getAnchorDelta   }   () => Byte
        |    getBbox           ${glyph.getBbox }   () => Array[Int]
        |    getChars          ${glyph.getChars }   () => Array[Char]
        |    getCode           ${glyph.getCode          }   () => Int
        |    getUnicode        ${glyph.getUnicode       }   () => Integer
        |    getWidth          ${glyph.getWidth         }   () => Int
        |    getXAdvance       ${glyph.getXAdvance      }   () => Short
        |    getXPlacement     ${glyph.getXPlacement    }   () => Short
        |    getYAdvance       ${glyph.getYAdvance      }   () => Short
        |    getYPlacement     ${glyph.getYPlacement    }   () => Short
        |    hasAdvance        ${glyph.hasAdvance       }   () => Boolean
        |    hasOffsets        ${glyph.hasOffsets       }   () => Boolean
        |    hasPlacement      ${glyph.hasPlacement     }   () => Boolean
        |    hasValidUnicode   ${glyph.hasValidUnicode  }   () => Boolean
        |    hashCode          ${glyph.hashCode         }   () => Int
        |    isMark            ${glyph.isMark           }   () => Boolean
        |""".stripMargin.mbox
  }




  def outputType1FontProgramInfo(fp: Type1Font, reader: PdfReader): Box = {
    // |    getGlyph             ${fp.getGlyph            }(String) => Glyph
    // |    getKerning           ${fp.getKerning          }(Glyph, Glyph) => Int
    // |    setKerning           ${fp.setKerning          }(Int, Int, Int) => Boolean
    val fstrm = if (fp.isBuiltInFont) fp.getFontStreamBytes else "(!built-in, bytes unavailabe)"


    s"""|Type1Font:
        |    getCharacterSet      ${fp.getCharacterSet     }
        |    isBuiltInFont        ${fp.isBuiltInFont       }
        |    getFontStreamBytes   ${fstrm}
        |    getFontStreamLengths ${fp.getFontStreamLengths}
        |    getPdfFontFlags      ${fp.getPdfFontFlags     }
        |    hasKernPairs         ${fp.hasKernPairs        }
        |""".stripMargin.mbox
  }


  def outputFontProgramInfo(fontProgram: FontProgram, reader: PdfReader): Box = {
    val fontTypes = typesOf(fontProgram).mkString("["," ", "]" )

    val fontIdBox = outputFontIdentificationInfo(fontProgram.getFontIdentification, reader)

    val fontPBox = if (fontProgram.isInstanceOf[Type1Font]) {
      val fp = fontProgram.asInstanceOf[Type1Font]
      outputType1FontProgramInfo(fp, reader)
    } else if (fontProgram.isInstanceOf[CidFont]) {
      val fp = fontProgram.asInstanceOf[CidFont]
      "(TODO no specific font info)".box
    } else if (fontProgram.isInstanceOf[TrueTypeFont]) {
      val fp = fontProgram.asInstanceOf[TrueTypeFont]
      "(TODO no specific font info)".box
    } else if (fontProgram.isInstanceOf[TrueTypeFont]) {
      val fp = fontProgram.asInstanceOf[TrueTypeFont]
      "(TODO no specific font info)".box
    } else {
      "(no specific font info)".box
    }


    val fontProgramInfo =
      s"""|FontProgram:
          |    T: ${fontProgram.getClass}
          |    countOfGlyphs       () => Int                 ${fontProgram.countOfGlyphs          }
          |    getAvgWidth         () => Int                 ${fontProgram.getAvgWidth            }
          |    getFontMetrics      () => FontMetrics         ${fontProgram.getFontMetrics         }
          |    getPdfFontFlags     () => Int                 ${fontProgram.getPdfFontFlags        }
          |    getRegistry         () => String              ${fontProgram.getRegistry            }
          |    hasKernPairs        () => Boolean             ${fontProgram.hasKernPairs           }
          |    isFontSpecific      () => Boolean             ${fontProgram.isFontSpecific         }
          |    getFontNames =>
          |""".stripMargin.mbox

    val fnInfor = outputFontNameInfo(fontProgram.getFontNames)

    (fontIdBox atop
      fontProgramInfo atop
      indent(7)(fontPBox) atop
      indent(7)(fnInfor)
      )
  }

  def outputPdfDecoding(tri: TextRenderInfo, reader: PdfReader, prefix: String): Unit = {
    val pdfstring = tri.getPdfString

    val valueBytes = pdfstring.getValueBytes.map(Byte.byte2int(_))
    val font = tri.getFont()
    val fontProgram = font.getFontProgram
    val fontNames = fontProgram.getFontNames
    val fontName = fontNames.getFontName

    println(
      s"""${prefix} '${tri.getText}': vbytes:[${valueBytes.mkString(",")}] ${fontName}"""
    )

  }

  def outputCharInfo(tri: TextRenderInfo, reader: PdfReader, force: Boolean = false): Unit = {
    val doDebugOutput = tri.getText == "⇑"

    if (doDebugOutput || force) {
      println(getCharTriInfo(tri, reader))

      val font = tri.getFont()


      // val text2 = font.getUnicodeEquivalent(tri.getMcid)
      val pdfstring = tri.getPdfString

      // val ffs = font.getFullFontStream
      // val baos = new java.io.ByteArrayOutputStream()
      // val directObj = reader.getPdfObject(ffs.getIndRef.getNumber)
      // val byteStr = directObj.getBytes.mkString("[", ",", "]")
      // ffs.writeContent(baos)
      // val byteStr = baos.getBytes.mkString("[", ",", "]")
      // val byteStr = baos.toByteArray().mkString("[", ",", "]")

      // val dictKvs = "Type,Subtype,Name/BaseFont".split(",").map{key =>
      //   font.getFontDictionary.get(new PdfName(s"/$key"))
      // }.mkString("\n")

      // println("885")
      // println(formatting.formatObject(reader.getPdfObject(885) , reader))
      // println("886")
      // println(formatting.formatObject(reader.getPdfObject(886) , reader))
      // println("887")
      // println(formatting.formatObject(reader.getPdfObject(887) , reader))
      // println("888")
      // println(formatting.formatObject(reader.getPdfObject(888) , reader))

      val asString = tri.getText.toCharArray().map{c =>
        Char.char2int(c).toString
      }.mkString




      val bs = pdfstring.getValueBytes.map(Byte.byte2int(_)).mkString(",")

      // val obs = pdfstring.getOriginalBytes.map(Byte.byte2int(_)).mkString(",")
      // pdfstring.getEncoding



      // val d0 = font.getDifferences
      // val d1 = font.getUnicodeDifferences
      // val dict  = font.getFontDictionary

      // val dictKvs = font.getFontDictionary.getKeys.map{ key =>
      //   "    " +key.toString() + ": " + font.getFontDictionary.get(key)
      // }.mkString("\n")


      // val diffStr = d0.mkString(",")
      // val udiffStr = d1.mkString(",")

      // val unicodeEquiv = font.getUnicodeEquivalent(0)
      // |    diffs             ${diffStr}
      // |    unidiffs          ${udiffStr}


      // val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
      // val charProcs = dict.getAsDict(new PdfName("CharProcs"))
      // val charProcInf = if (charProcs != null) {
      //   formatting.formatPdfObject(charProcs, reader).toString()
      // } else "<no CharProcs>"


      // def formatLineVector(ls: PVector): String = {
      //   s"""[${ls.get(0)} ${ls.get(1)}, ${ls.get(2)}}}]"""
      // }

      // def formatLineSegment(ls: LineSegment): String = {
      //   s""" ${formatLineVector(ls.getStartPoint)} -> ${formatLineVector(ls.getEndPoint)} ${ls.getLength}}"""

      // }
      val fontProgram = font.getFontProgram
      val fontProgramInfo =
        s"""|  ${fontProgram.getFontIdentification} getFontIdentification
            |  countOfGlyphs       () => Int                 ${fontProgram.countOfGlyphs          }
            |  getAvgWidth         () => Int                 ${fontProgram.getAvgWidth            }
            |  getFontMetrics      () => FontMetrics         ${fontProgram.getFontMetrics         }
            |  getFontNames        () => FontNames           ${fontProgram.getFontNames           }
            |  getPdfFontFlags     () => Int                 ${fontProgram.getPdfFontFlags        }
            |  getRegistry         () => String              ${fontProgram.getRegistry            }
            |  hasKernPairs        () => Boolean             ${fontProgram.hasKernPairs           }
            |  isFontSpecific      () => Boolean             ${fontProgram.isFontSpecific         }
            |""".stripMargin


      val pdfstrInf = s"""| tri.text: ${tri.getText}:  As c->int: '${asString}'
                          |        isEmbedded       ${font.isEmbedded()}        => Boolean
                          |        getBytes         ${bs} => Array[Byte]
                          |        getEncoding      ${pdfstring.getEncoding} => String
                          |        isHexWriting     ${pdfstring.isHexWriting        } => Boolean
                          |        toString         ${pdfstring.toString            } => String
                          |        toUnicodeString  ${pdfstring.toUnicodeString     } => String
                          |        isArray          ${pdfstring.isArray             } => Boolean
                          |        isBoolean        ${pdfstring.isBoolean           } => Boolean
                          |        isDictionary     ${pdfstring.isDictionary        } => Boolean
                          |        isIndirect       ${pdfstring.isIndirectReference()} => Boolean
                          |        isName           ${pdfstring.isName              } => Boolean
                          |        isNull           ${pdfstring.isNull              } => Boolean
                          |        isNumber         ${pdfstring.isNumber            } => Boolean
                          |        isStream         ${pdfstring.isStream            } => Boolean
                          |        isString         ${pdfstring.isString            } => Boolean
                          |        Rise              ${tri.getRise}                                => Float
                          |        Mcid              ${tri.getMcid}              => Integer
                          |        PdfString         ${tri.getPdfString}              => PdfString
                          |        Mcid              ${tri.getMcid}              => Integer
                          |        PdfString         ${tri.getPdfString}              => PdfString
                          |        TextRenderMode    ${tri.getTextRenderMode}              => Int
                          |""".stripMargin

      println(pdfstrInf)


    }
  }

}





class MyCFFFont(bytes: Array[Byte]) extends CFFFont(bytes) {
  def getFonts() = fonts
  def getRange(i: Int) = getEntireIndexRange(i)
}

object formatting {
  import TB._


  def formatPdfObject(obj: PdfObject, reader: PdfReader): String = {
    formatObject(obj, reader).toString()
  }


  var followIndirect: Boolean = true

  def formatIndirectObject(obj: PdfObject, reader: PdfReader): Box = {
    val iref: PdfIndirectReference = obj.asInstanceOf[PdfIndirectReference]
    val refNum =iref.getObjNumber
    val refGen =iref.getGenNumber
    val refStr = s"[${refNum} $refGen R]"
    if (followIndirect)  {
      val objDirect = iref.getDocument.getPdfObject(refNum)
      refStr.box + formatObject(objDirect, reader)
    } else {
      refStr
    }
  }



  def formatCFFFont(cf: MyCFFFont): Box = {
    val fontname = cf.getNames()(0)

    val cidFontByteArray = cf.getCID(fontname)

    val bas = cidFontByteArray.take(10).mkString(", ")

    val cidBytesStr = s""" fontByteArray.len = ${cidFontByteArray.length}= [${bas}]"""

    val fontBoxes = cf.getFonts.toList.map({font =>

      val bytesAtOffset = font.charstringsOffsets.take(4).map({offset =>
        val cidOff = cidFontByteArray(offset)
        val cidOff1 = cidFontByteArray(offset+1)
        val cidOff2 = cidFontByteArray(offset+2)
        s"""bytes@${offset}=${cidOff}, ${cidOff1}, ${cidOff2}"""
      }).mkString(" :: ")


      s"""|font
          |    CharsetLength           ${font.CharsetLength       }  Int
          |    CharstringType          ${font.CharstringType      }  Int
          |    FDArrayCount            ${font.FDArrayCount        }  Int
          |    FDArrayOffsets          ${font.FDArrayOffsets      }  Array[Int]
          |    FDArrayOffsize          ${font.FDArrayOffsize      }  Int
          |    FDSelect                ${font.FDSelect            }  Array[Int]
          |    FDSelectFormat          ${font.FDSelectFormat      }  Int
          |    FDSelectLength          ${font.FDSelectLength      }  Int
          |    PrivateSubrsOffset      ${font.PrivateSubrsOffset   }  Array[Int]
          |    SubrsOffsets            ${font.SubrsOffsets       }  Array[Int]
          |    charset                 ${font.charset             }  Array[Int]
          |    charsetOffset           ${font.charsetOffset       }  Int
          |    charstringsOffset       ${font.charstringsOffset   }  Int
          |    charstringsOffsets      ${font.charstringsOffsets.mkString(",")}  Array[Int]
          |      BytesAtOffset         ${bytesAtOffset}
          |    encodingOffset          ${font.encodingOffset      }  Int
          |    fdarrayOffset           ${font.fdarrayOffset       }  Int
          |    fdprivateLengths        ${font.fdprivateLengths   }  Array[Int]
          |    fdprivateOffsets        ${font.fdprivateOffsets   }  Array[Int]
          |    fdprivateSubrs          ${font.fdprivateSubrs   }  Array[Int]
          |    fdselectOffset          ${font.fdselectOffset      }  Int
          |    fullName                ${font.fullName            }  String
          |    isCID                   ${font.isCID               }  Boolean
          |    name                    ${font.name                }  String
          |    nglyphs                 ${font.nglyphs             }  Int
          |    nstrings                ${font.nstrings            }  Int
          |    privateLength           ${font.privateLength       }  Int
          |    privateOffset           ${font.privateOffset       }  Int
          |    privateSubrs            ${font.privateSubrs        }  Int
          |""".stripMargin.mbox
    })

    val allFontBoxes = vcat(left)(fontBoxes)

    val hdr =
    s"""|CFFFont:
        |    getNames   ${cf.getNames.mkString(", ")  }          () => Array[String]
        |""".stripMargin.mbox

    hdr atop cidBytesStr.box atop allFontBoxes
  }

  def formatStreamObject(obj: PdfObject, reader: PdfReader): Box = {
    val strm = obj.asInstanceOf[PdfStream]

    val subtype = strm.getAsName(new PdfName("Subtype"))
    val isType1C = subtype != null && subtype.getValue == "Type1C"


    val byteBox = if (strm.getBytes != null && isType1C) {
      val cffFont = new MyCFFFont(strm.getBytes())
      formatCFFFont(cffFont)
    } else "[empty]".box

    val strmAsDict = indent(3)(formatDictionary(strm, reader))

    strmAsDict atop "bytes:" atop byteBox

  }

  def objectType(obj: PdfObject, reader: PdfReader): String = {
    if (obj.isArray()) {
      "arr"
    } else if (obj.isBoolean()) {
      "bool"
    } else if (obj.isNull()) {
      "null"
    } else if (obj.isNumber()) {
      "num"
    } else if (obj.isString()) {
      "str"
    } else if (obj.isName) {
      "name"
    } else if (obj.isDictionary) {
      "dict"
    } else if (obj.isIndirectReference()) {
      val iref: PdfIndirectReference = obj.asInstanceOf[PdfIndirectReference]
      val refNum =iref.getObjNumber
      val refGen =iref.getGenNumber
      val objDirect = iref.getDocument.getPdfObject(refNum)
      val dtype = objectType(objDirect, reader)
      s"${dtype}*${refNum}"
    } else if (obj.isStream()) {
      "stream"
    } else {
      "T?"
    }
  }
  def formatDictionary(obj: PdfObject, reader: PdfReader): Box = {
    val xo = obj.asInstanceOf[PdfDictionary]
    // val cbi = obj.canBeInObjStm()
    // val objNumber = obj.getIndRef.getNumber
    vcat(left)(
      xo.keySet().toList.map{ key =>
        val pdfVal = xo.get(key)
        val valBox = formatObject(pdfVal, reader)
        if( valBox.rows == 1 ) {
          s"${key} :: ${objectType(pdfVal, reader)} => ".box beside valBox
        } else {
          s"${key} :: ${objectType(pdfVal, reader)} =>".box atop indent(2)(valBox)
        }
      }
    )
  }

  def formatObject(obj: PdfObject, reader: PdfReader): Box = {

    if (obj.isArray()) {
      val arr = obj.asInstanceOf[PdfArray]
      val alen = arr.size()
      val fmt = arr.map{ formatObject(_, reader) }.toSeq
      val isMultiLine = fmt.exists { _.rows > 1 }
      val fbox = if (isMultiLine) {
        vcat(fmt)
      } else {
        s"(${fmt.size}) [".box + hjoins(center1, ", ")(fmt.toSeq.take(15)) + "]".box
      }
      fbox
    } else if (obj.isDictionary) {
      val xo = obj.asInstanceOf[PdfDictionary]
      indent(4)(formatDictionary(obj, reader))
    } else if (obj.isBoolean()) {
      s"${obj.toString().trim()}"
    } else if (obj.isNull()) {
      "<null>"
    } else if (obj.isNumber()) {
      s"${obj.toString()}"
    } else if (obj.isString()) {
      s"${obj.toString().trim()}"
    } else if (obj.isName) {
      s"${obj.toString().trim()}"
    } else if (obj.isIndirectReference()) {
      formatIndirectObject(obj, reader)
    } else if (obj.isStream()) {
      formatStreamObject(obj, reader)
    } else {
      s"??unknown-pdf-obj??".box
    }
  }

}
