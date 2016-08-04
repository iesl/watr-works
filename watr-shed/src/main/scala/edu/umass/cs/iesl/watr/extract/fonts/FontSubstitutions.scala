package edu.umass.cs.iesl.watr
package extract
package fonts

object FontLookup {
  import com.itextpdf.io.font.FontProgramFactory
  import com.itextpdf.io.font.FontProgram
  import scala.collection.mutable


  val fontPrograms = mutable.HashMap[String, FontProgram]()

  val fontPaths = List[(String, String)](
  )

  def init(): Unit = {
    fontPaths.foreach { case (fname, fpath) =>
      fontPrograms.put(fname, FontProgramFactory.createFont(fpath))
    }
  }

  init()


  def getFontProgram(fontName: String): Option[FontProgram] = {
    fontPrograms.get(fontName)
  }

  // def substituteFontProgram()
}


object GlyphHashLookup {
  import java.io.BufferedInputStream
  import java.util.zip.GZIPInputStream

  def hex2int(h: String) = Integer.parseInt(h, 16)

  def apply(): GlyphHashLookup = {
    val inputStream = new GZIPInputStream(new BufferedInputStream(this.getClass.getResourceAsStream("/glyph-hash.tsv.gz")))
    val sourceFile = io.Source.fromInputStream(inputStream)

    val mappings = sourceFile.getLines
      .map({line =>
        val recs = line.split("\t")
        val hash = recs(0).trim
        val names = recs(1).trim
        val unicodes = recs(2).trim.split(",").map(_.trim).filterNot(_.toLowerCase=="ffffffff").map(hex2int(_))
        val widths = recs(3).trim.split(",").map(_.trim).map(_.toInt)

        unicodes
          .filterNot(_ == -1)
          .headOption.map({ unicode =>
            (hash, unicode)
          })
      })
      .toSeq
      .flatten

    new GlyphHashLookup(mappings.toMap)
  }

  lazy val global = apply()
}

class GlyphHashLookup(mapping: Map[String, Int]) {
  def lookup(hash: String): Option[Char] = {
    mapping.get(hash).map(_.toChar)
  }

}
