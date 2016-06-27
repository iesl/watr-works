package edu.umass.cs.iesl.watr
package extract



import com.itextpdf.text.pdf._
import com.itextpdf.text.pdf.parser.{Vector => PVector, _}
import scala.collection.JavaConversions._
import textboxing.{TextBoxing => TB}

object DocumentFontInfo {
  def addFontInfo(font: DocumentFont): Unit = {
    // val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
    // val allNameEntries = font.getAllNameEntries.map(_.mkString("[", ",", "]")).mkString(", ")
    // val fontDictionary = font.getFontDictionary

    // val fontDictionaryKeys = fontDictionary.getKeys.map(_.toString()).mkString(",")
    val fontNameKey = font.getFullFontName.map(_.mkString("").trim).mkString("_").trim
    // val _ = fontDict.put(fontNameKey, font)


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
    //   // font.getDifferences.mkString(", "),
    //   font.getEncoding,
    //   font.getFontType,
    //   font.getSubfamily,
    //   // font.getUnicodeDifferences.mkString(", "),
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
      // font.getWidths.take(3).mkString(", "),
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



  def outputCharInfo(tri: TextRenderInfo, reader: PdfReader): Unit = {
    val font = tri.getFont()
    val doDebugOutput = tri.getText == "⇑"

    if (doDebugOutput) {
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

      println("885")
      println(formatting.formatObject(reader.getPdfObject(885) , reader))
      println("886")
      println(formatting.formatObject(reader.getPdfObject(886) , reader))
      println("887")
      println(formatting.formatObject(reader.getPdfObject(887) , reader))
      println("888")
      println(formatting.formatObject(reader.getPdfObject(888) , reader))

      val asString = tri.getText.toCharArray().map{c =>
        Char.char2int(c).toString
      }.mkString

      val bs = pdfstring.getBytes.map(Byte.byte2int(_)).mkString(",")
      val obs = pdfstring.getOriginalBytes.map(Byte.byte2int(_)).mkString(",")
      pdfstring.getEncoding

      // |        bytes            ${byteStr} => Array[Byte]
      //   |        bytes            ${byteStr} => Array[Byte]


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


      val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
      val charProcs = dict.getAsDict(new PdfName("CharProcs"))
      val charProcInf = if (charProcs != null) {
        formatting.formatPdfObject(charProcs, reader).toString()
      } else "<no CharProcs>"


      def formatLineVector(ls: PVector): String = {
        s"""[${ls.get(0)} ${ls.get(1)}, ${ls.get(2)}}}]"""
      }

      def formatLineSegment(ls: LineSegment): String = {
        s""" ${formatLineVector(ls.getStartPoint)} -> ${formatLineVector(ls.getEndPoint)} ${ls.getLength}}"""

      }

      val pdfstrInf = s"""| tri.text: ${tri.getText}:  As c->int: '${asString}'
                          |        isEmbedded       ${font.isEmbedded()}        => Boolean
                          |        getPostscriptFontName ${font.getPostscriptFontName}
                          |        getBytes         ${bs} => Array[Byte]
                          |        getEncoding      ${pdfstring.getEncoding} => String
                          |        getOriginalBytes ${obs} => Array[Byte]
                          |        isHexWriting     ${pdfstring.isHexWriting        } => Boolean
                          |        toString         ${pdfstring.toString            } => String
                          |        toUnicodeString  ${pdfstring.toUnicodeString     } => String
                          |        canBeInObjStm    ${pdfstring.canBeInObjStm       } => Boolean
                          |        getIndRef        ${pdfstring.getIndRef           } => PRIndirectReference
                          |        isArray          ${pdfstring.isArray             } => Boolean
                          |        isBoolean        ${pdfstring.isBoolean           } => Boolean
                          |        isDictionary     ${pdfstring.isDictionary        } => Boolean
                          |        isIndirect       ${pdfstring.isIndirect          } => Boolean
                          |        isName           ${pdfstring.isName              } => Boolean
                          |        isNull           ${pdfstring.isNull              } => Boolean
                          |        isNumber         ${pdfstring.isNumber            } => Boolean
                          |        isStream         ${pdfstring.isStream            } => Boolean
                          |        isString         ${pdfstring.isString            } => Boolean
                          |        length           ${pdfstring.length              } => Int
                          |        type             ${pdfstring.`type`              } => Int
                          |        Rise              ${tri.getRise}                                => Float
                          |        AscentLine        ${formatLineSegment(tri.getAscentLine)}       => LineSegment
                          |        Baseline          ${formatLineSegment(tri.getBaseline)}         => LineSegment
                          |        Baseline (uns)    ${formatLineSegment(tri.getUnscaledBaseline)} => LineSegment
                          |        DescentLine       ${formatLineSegment(tri.getDescentLine)}      => LineSegment
                          |        Mcid              ${tri.getMcid}              => Integer
                          |        PdfString         ${tri.getPdfString}              => PdfString
                          |        Font              ${fontFullname}      => DocumentFont
                          |        Unic.Equiv        ${unicodeEquiv}      => Int
                          |        Mcid              ${tri.getMcid}              => Integer
                          |        PdfString         ${tri.getPdfString}              => PdfString
                          |        TextRenderMode    ${tri.getTextRenderMode}              => Int
                          |        ${dictKvs}
                          |        ${charProcInf}
                          |""".stripMargin

      println(pdfstrInf)


    }
  }

}





object formatting {
  import TB._


  def formatPdfObject(obj: PdfObject, reader: PdfReader): String = {
    formatObject(obj, reader).toString()
  }

  var followIndirect: Boolean = true

  def formatIndirectObject(obj: PdfObject, reader: PdfReader): Box = {
    val iref: PdfIndirectReference = obj.asInstanceOf[PdfIndirectReference]
    val refNum =iref.getNumber
    val refGen =iref.getGeneration
    if (followIndirect)  {
      val objDirect = reader.getPdfObject(refNum)
      formatObject(objDirect, reader)
    } else {
      s"ind*${refNum}"
    }
  }

  def formatStreamObject(obj: PdfObject, reader: PdfReader): Box = {



    val strm = obj.asInstanceOf[PdfStream]

    // val xo = obj.asInstanceOf[PdfStream]
    // new java.io.OutputStreamWriter(new java.io.StringWriter())
    // val sw = new java.io.StringWriter()

    // val baos = new java.io.ByteArrayOutputStream()
    // strm.writeContent(baos)
    // val strmBytes = baos.toByteArray().mkString("[", ",", "]")

    // val rlen = strm.getRawLength
    val bytes = if (strm.getBytes != null) {
      strm.getBytes.mkString(",")
    } else "[]"

    // val strmNum = strm.getIndRef.getNumber
    // strm.getDirectObject(strm.getIndRef)

    val b = s"""B:${bytes}""".box atop formatDictionary(strm, reader)
    val bi = indent(3)(b)

    s"stream len=${strm.getRawLength}/${strm.size()}".box atop bi

  }

  def objectType(obj: PdfObject, reader: PdfReader): String = {
    if (obj.isArray()) {
      "array"
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
    } else if (obj.isIndirect()) {
      val iref: PdfIndirectReference = obj.asInstanceOf[PdfIndirectReference]
      val refNum =iref.getNumber
      val refGen =iref.getGeneration
      val objDirect = reader.getPdfObject(refNum)
      val dtype = objectType(objDirect, reader)
      s"${dtype}*${refNum}"
    } else if (obj.isStream()) {
      "stream"
    } else if (obj.isDictionary) {
      "dict"
    } else {
      "T?"
    }
  }
  def formatDictionary(obj: PdfObject, reader: PdfReader): Box = {
    val xo = obj.asInstanceOf[PdfDictionary]
    // val cbi = obj.canBeInObjStm()
    // val objNumber = obj.getIndRef.getNumber
    vcat(left)(
      xo.getKeys.toList.map{ key =>
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
    // obj.canBeInObjStm

    if (obj.isArray()) {
      val arr = obj.asInstanceOf[PdfArray]
      val arrstr = arr.mkString("[", ", ", "]")
      arrstr
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
    } else if (obj.isIndirect()) {
      formatIndirectObject(obj, reader)
    } else if (obj.isStream()) {
      formatStreamObject(obj, reader)
    } else if (obj.isDictionary) {
      val xo = obj.asInstanceOf[PdfDictionary]
      indent(4)(formatDictionary(obj, reader))
    } else {
      s"??unknown-pdf-obj??".box
    }
  }

  def main(args: Array[String]) = {
    import ammonite.ops._
    val fn = cwd / RelPath(args(0))
    // val content = read(fn)
    val cbytes = read.bytes(fn)
    val deflated = PdfReader.FlateDecode(cbytes, false)
    println(cbytes.mkString(", "))
    println("deflated:")
    println(deflated.mkString(", "))
  }
}
