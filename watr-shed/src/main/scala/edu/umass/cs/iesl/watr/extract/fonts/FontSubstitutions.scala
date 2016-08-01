package edu.umass.cs.iesl.watr
package extract
package fonts

object FontLookup {
  import com.itextpdf.io.font.FontProgramFactory
  import com.itextpdf.io.font.FontProgram
  import scala.collection.mutable


  val fontPrograms = mutable.HashMap[String, FontProgram]()

  val fontPaths = List[(String, String)](
    "arial" -> "/usr/share/fonts/truetype/msttcorefonts/Arial.ttf"
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
