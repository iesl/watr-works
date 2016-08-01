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
import com.itextpdf.kernel.geom
import com.itextpdf.kernel.pdf.{ PdfArray, PdfIndirectReference, PdfStream, PdfString }
import itextpdf.kernel.pdf.PdfReader
import com.itextpdf.kernel.pdf.PdfDictionary;
import com.itextpdf.kernel.pdf.PdfObject
import itextpdf.kernel.pdf.canvas.parser.data._


import com.itextpdf.kernel.geom.{Vector => PVector}

import scala.collection.JavaConversions._
import textboxing.{TextBoxing => TB}

import utils.ShowNumerics._

object DocumentFontInfo {

  import TB._

  def getPdfStringInfo(pdfFont: PdfFont, pdfString: PdfString, reader: PdfReader): Box = {

    val bs = pdfString.getValueBytes.map(Byte.byte2int(_))
    val b0 = bs(0)
    val fprogram = pdfFont.getFontProgram
    // val pglyph = fprogram.getGlyph(b0)
    val pglyphByCode = fprogram.getGlyphByCode(b0 & 0xFF)

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

  def formatMatrix(m: geom.Matrix, label: Option[String]): Box = {
    formatMatrixArr(Array(m.get(0).toDouble, m.get(1).toDouble,
                          m.get(2).toDouble, m.get(3).toDouble,
                          m.get(4).toDouble, m.get(5).toDouble),
                    label)
  }

  def formatMatrixArr(m: Array[Double], label: Option[String]): Box = {
    val i11 = m(0)
    val i12 = m(1)
    val i13 = m(2)
    val i21 = m(3)
    val i22 = m(4)
    val i23 = m(5)

    val col1 = vcat(right)(Seq(i11.pp, i21.pp))
    val col2 = vcat(right)(Seq(i12.pp, i22.pp))
    val col3 = vcat(right)(Seq(i13.pp, i23.pp))
    val mbox = col1 besideS col2 besideS col3

    label.map{ l =>
      borderInlineTop(l atop mbox)
    } getOrElse {
      borderInlineTop(mbox)
    }
  }
  import com.itextpdf.kernel.pdf.canvas.CanvasGraphicsState

  def outputGraphicsState(gs: CanvasGraphicsState): Box = {
    s"""|CanvasGraphicsState:
        |   getAlphaIsShape               () => ${gs.getAlphaIsShape               } :: Boolean
        |   getAutomaticStrokeAdjustment  () => ${gs.getAutomaticStrokeAdjustment  } :: Boolean
        |   getCharSpacing                () => ${gs.getCharSpacing                } :: Float
        |   getFillColor                  () => ${gs.getFillColor                  } :: Color
        |   getFillOpacity                () => ${gs.getFillOpacity                } :: Float
        |   getFillOverprint              () => ${gs.getFillOverprint              } :: Boolean
        |   getFlatnessTolerance          () => ${gs.getFlatnessTolerance          } :: Float
        |   getFontSize                   () => ${gs.getFontSize                   } :: Float
        |   getHorizontalScaling          () => ${gs.getHorizontalScaling          } :: Float
        |   getLeading                    () => ${gs.getLeading                    } :: Float
        |   getLineCapStyle               () => ${gs.getLineCapStyle               } :: Int
        |   getLineJoinStyle              () => ${gs.getLineJoinStyle              } :: Int
        |   getLineWidth                  () => ${gs.getLineWidth                  } :: Float
        |   getMiterLimit                 () => ${gs.getMiterLimit                 } :: Float
        |   getOverprintMode              () => ${gs.getOverprintMode              } :: Int
        |   getRenderingIntent            () => ${gs.getRenderingIntent            } :: PdfName
        |   getSmoothnessTolerance        () => ${gs.getSmoothnessTolerance        } :: Float
        |   getStrokeColor                () => ${gs.getStrokeColor                } :: Color
        |   getStrokeOpacity              () => ${gs.getStrokeOpacity              } :: Float
        |   getStrokeOverprint            () => ${gs.getStrokeOverprint            } :: Boolean
        |   getTextKnockout               () => ${gs.getTextKnockout               } :: Boolean
        |   getTextRenderingMode          () => ${gs.getTextRenderingMode          } :: Int
        |   getTextRise                   () => ${gs.getTextRise                   } :: Float
        |   getCtm                        () => ${gs.getCtm                        } :: Matrix
        |   getHTP                        () => ${gs.getHTP                        } :: PdfObject
        |   getHalftone                   () => ${gs.getHalftone                   } :: PdfObject
        |   getSoftMask                   () => ${gs.getSoftMask                   } :: PdfObject
        |   getFont                       () => ${gs.getFont                       } :: PdfFont
        |   getDashPattern                () => ${gs.getDashPattern                } :: PdfArray
        |   getTransferFunction           () => ${gs.getTransferFunction           } :: PdfObject
        |   getTransferFunction2          () => ${gs.getTransferFunction2          } :: PdfObject
        |   getUnderColorRemovalFunction  () => ${gs.getUnderColorRemovalFunction  } :: PdfObject
        |   getUnderColorRemovalFunction2 () => ${gs.getUnderColorRemovalFunction2 } :: PdfObject
        |   getWordSpacing                () => ${gs.getWordSpacing                } :: Float
        |   getBlackGenerationFunction    () => ${gs.getBlackGenerationFunction    } :: PdfObject
        |   getBlackGenerationFunction2   () => ${gs.getBlackGenerationFunction2   } :: PdfObject
        |   getBlendMode                  () => ${gs.getBlendMode                  } :: PdfObject
        |""".stripMargin.mbox

  }


  def getCharTriInfo(tri: TextRenderInfo, reader: PdfReader): Box = {
    val graphicState = tri.gs
    val fontMatrix = tri.fontMatrix
    val ctm = formatMatrix(graphicState.getCtm, Some("CTM"))
    val fontMatrixBox = formatMatrixArr(fontMatrix, Some("FontMatrix"))
    val t2u = formatMatrix(tri.textToUserSpaceTransformMatrix, Some("Txt->User"))
    // CTM = FontMatrix * TextSpace
    // CTM maps UserSpace => Output-Device-Coords
    // When the glyph description begins execution, the current transformation matrix (CTM) shall be the
    // concatenation of the font matrix (FontMatrix in the current font dictionary) and the text space that was in effect
    // at the time the text-showing operator was invoked (see 9.4.4, "Text Space Details").

    val matrices = hcat(Seq(ctm, " = ".box, fontMatrixBox, t2u), center1)

    val stringWidth = tri.getPdfStringWidth(tri.getPdfString, true)

    val textRenderInfoStr =
      s"""|TextRenderInfo =>
          |     getPdfString            ${tri.getPdfString              }
          |     getStringWidth(pdfStr)  ${stringWidth}
          |     getActualText           ${tri.getActualText             }
          |     getAscentLine           ${outputLineSegmentInfo(tri.getAscentLine)}
          |     getDescentLine          ${outputLineSegmentInfo(tri.getDescentLine)}
          |     getBaseline             ${outputLineSegmentInfo(tri.getBaseline)}
          |     getCanvasTagHierarchy   ${tri.getCanvasTagHierarchy     }
          |     getCharSpacing          ${tri.getCharSpacing            }
          |     getFillColor            ${tri.getFillColor.getColorValue.mkString(", ")}
          |     getFont                 (below)
          |     getFontSize             ${tri.getFontSize               }
          |     getHorizontalScaling    ${tri.getHorizontalScaling      }
          |     getLeading              ${tri.getLeading                }
          |     getMcid                 ${tri.getMcid                   }
          |     getRise                 ${tri.getRise                   }
          |     getSingleSpaceWidth     ${tri.getSingleSpaceWidth       }
          |     getStrokeColor          ${tri.getStrokeColor            }
          |     getText                 ${tri.getText                   }
          |     getTextRenderMode       ${tri.getTextRenderMode         }
          |     getUnscaledBaseline     ${outputLineSegmentInfo(tri.getUnscaledBaseline)}
          |     getUnscaledWidth        ${tri.getUnscaledWidth          }
          |     getWordSpacing          ${tri.getWordSpacing            }
          |""".stripMargin.mbox

    val pdfString = tri.getPdfString

    val bs = pdfString.getValueBytes.map(Byte.byte2int(_))
    val b0 = bs(0)
    val font = tri.getFont
    val fprogram = font.getFontProgram
    val pglyph = fprogram.getGlyph(b0)
    val pglyphByCode = fprogram.getGlyphByCode(b0 & 0xFF)

    var glyphInf =  "Glyph Info".box

    if (pglyph != null) {
      glyphInf = glyphInf atop "pglyph" atop outputGlyphInfo(pglyph, reader)
    }
    if (pglyphByCode != null) {
      glyphInf = glyphInf atop "pglyph-by-code" atop outputGlyphInfo(pglyphByCode, reader)
    }


    val fmetrics = outputFontMetrics(fprogram.getFontMetrics)

    val pdfStrInfo = getPdfStringInfo(tri.getFont, tri.getPdfString, reader)
    val gstate = outputGraphicsState(graphicState)

    (textRenderInfoStr atop
      matrices atop
      gstate atop
      indent(2)(outputFontInfo(tri.getFont, reader, None)) atop
      fmetrics atop
      indent(2)(pdfStrInfo) atop
      glyphInf
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

    val fontHdr =
    s"""|Font:
        |    Glyph bytes   ${glyphBytes}
        |    getFontMatrix ${pdfFont.getFontMatrix }      () => Array[Double]
        |    isEmbedded    ${pdfFont.isEmbedded    }      () => Boolean
        |    isSubset      ${pdfFont.isSubset      }      () => Boolean
        |    getFontProgram =>
        |""".stripMargin.mbox

    val fontPrgm = indent(6)(outputFontProgramInfo(pdfFont.getFontProgram))

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

  def outputFontIdentificationInfo(fontId: FontIdentification): Box = {

    s"""|FontIdentification:
        |    getPanose       ${fontId.getPanose       }
        |    getTtfUniqueId  ${fontId.getTtfUniqueId  }
        |    getTtfVersion   ${fontId.getTtfVersion   }
        |    getType1Xuid    ${fontId.getType1Xuid    }
        |""".stripMargin.mbox
  }



  def outputGlyphInfo(glyph: Glyph, reader: PdfReader): Box = {
    val getBbox = if (glyph.getBbox!=null) {glyph.getBbox.mkString(", ")} else "null"

    s"""|Glyph:
        |    getAnchorDelta    ${glyph.getAnchorDelta   }   () => Byte
        |    getBbox           ${getBbox }   () => Array[Int]
        |    getChars          [${glyph.getChars.mkString(", ") }]   () => Array[Char]
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




  def outputType1FontProgramInfo(fp: Type1Font): Box = {
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


  def outputFontProgramInfo(fontProgram: FontProgram): Box = {
    val fontTypes = typesOf(fontProgram).mkString("["," ", "]" )

    val fontIdBox = outputFontIdentificationInfo(fontProgram.getFontIdentification)

    val fontPBox = if (fontProgram.isInstanceOf[Type1Font]) {
      val fp = fontProgram.asInstanceOf[Type1Font]
      outputType1FontProgramInfo(fp)
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

  def outputFontMetrics(fontMetrics: FontMetrics): Box = {
    val  getGlyphWidths = if (fontMetrics.getGlyphWidths() != null) {
      fontMetrics.getGlyphWidths.mkString(", ")
    } else "null"

    s"""|Font Metrics:
        |   getAdvanceWidthMax    => ${fontMetrics.getAdvanceWidthMax    } Int
        |   getAscender           => ${fontMetrics.getAscender           } Int
        |   getBbox               => ${fontMetrics.getBbox.mkString(", ")} Array[Int]
        |   getCapHeight          => ${fontMetrics.getCapHeight          } Int
        |   getDescender          => ${fontMetrics.getDescender          } Int
        |   getGlyphWidths        => ${getGlyphWidths.take(4).mkString(",")}... Array[Int]
        |   getItalicAngle        => ${fontMetrics.getItalicAngle        } Float
        |   getLineGap            => ${fontMetrics.getLineGap            } Int
        |   getMaxGlyphId         => ${fontMetrics.getMaxGlyphId         } Int
        |   getStemH              => ${fontMetrics.getStemH              } Int
        |   getStemV              => ${fontMetrics.getStemV              } Int
        |   getStrikeoutPosition  => ${fontMetrics.getStrikeoutPosition  } Int
        |   getStrikeoutSize      => ${fontMetrics.getStrikeoutSize      } Int
        |   getSubscriptOffset    => ${fontMetrics.getSubscriptOffset    } Int
        |   getSubscriptSize      => ${fontMetrics.getSubscriptSize      } Int
        |   getSuperscriptOffset  => ${fontMetrics.getSuperscriptOffset  } Int
        |   getSuperscriptSize    => ${fontMetrics.getSuperscriptSize    } Int
        |   getTypoAscender       => ${fontMetrics.getTypoAscender       } Int
        |   getTypoDescender      => ${fontMetrics.getTypoDescender      } Int
        |   getUnderlinePosition  => ${fontMetrics.getUnderlinePosition  } Int
        |   getUnderlineThickness => ${fontMetrics.getUnderlineThickness } Int
        |   getUnitsPerEm         => ${fontMetrics.getUnitsPerEm         } Int
        |   getWinAscender        => ${fontMetrics.getWinAscender        } Int
        |   getWinDescender       => ${fontMetrics.getWinDescender       } Int
        |   getXHeight            => ${fontMetrics.getXHeight            } Int
        |   isFixedPitch          => ${fontMetrics.isFixedPitch          } Boolean
        |""".stripMargin.mbox

  }

  def outputCharInfo(tri: TextRenderInfo, reader: PdfReader, force: Boolean = false): Unit = {
    val doDebugOutput = tri.getText == "⇑"

    if (doDebugOutput || force) {
      println(getCharTriInfo(tri, reader))

      val font = tri.getFont()


      // val text2 = font.getUnicodeEquivalent(tri.getMcid)
      val pdfstring = tri.getPdfString


      val asString = tri.getText.toCharArray().map{c =>
        Char.char2int(c).toString
      }.mkString




      val bs = pdfstring.getValueBytes.map(Byte.byte2int(_)).mkString(",")

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
