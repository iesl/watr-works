package edu.umass.cs.iesl.watr
package textreflow //;import acyclic.file


import geometry._

trait ImageTextReflow extends PlainTextReflow {
  import com.sksamuel.scrimage._
  import com.sksamuel.scrimage.canvas._

  def stringsToMultiPageAtomsWithImages(
    docId: String@@DocumentID,
    strs: String*
  ): Seq[(Seq[PageAtom], PageGeometry, Image)] = {
    for {
      ((atoms, geom), pstr) <- stringsToMultiPageAtoms(docId, strs:_*).zip(strs.toList)
    } yield (atoms, geom, textToImage(pstr))
  }

  // def createMultiPageIndexWithImages(docId: String@@DocumentID, strs: String*): MultiPageIndex = {
  //   val mpageIndex = createMultiPageIndex(docId, strs)
  //   mpageIndex
  // }

  def textToImage(text: String): Image = {
    val blank = Image.filled(300, 200, X11Colorlist.White)
    val canvas1 = new Canvas(blank)

    for {
      (line, n) <- lines(text).zipWithIndex
    } yield {
      canvas1.draw(Drawable(str, 0, line*yscale))
    }
    canvas1.image
  }


}

  // def textToImage(text0: String): Image = {

  //   import java.awt.Color
  //   import java.awt.Font
  //   import java.awt.FontMetrics
  //   import java.awt.Graphics2D
  //   import java.awt.RenderingHints
  //   import java.awt.image.BufferedImage

  //   val font: Font  = new Font("Arial", Font.PLAIN, 12)

  //   /*
  //    Because font metrics is based on a graphics context, we need to create
  //    a small, temporary image so we can ascertain the width and height
  //    of the final image
  //    */
  //   def getTextMetrics(s: String): (Int, Int) = {
  //     val imgtmp: BufferedImage =  new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
  //     val g2dtmp: Graphics2D  = imgtmp.createGraphics()
  //     g2dtmp.setFont(font)
  //     val fmetrics: FontMetrics  = g2dtmp.getFontMetrics()
  //     val width = fmetrics.stringWidth(s)
  //     val height = fmetrics.getHeight()
  //     g2dtmp.dispose()
  //     (width, height)
  //   }

  //   for {
  //     (line, n) <- lines(text0).zipWithIndex
  //   } yield {
  //     val (height, width) = getTextMetrics(line)
  //     val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  //     val g2d = img.createGraphics()
  //     g2d.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY)
  //     g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  //     g2d.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
  //     g2d.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_ENABLE)
  //     g2d.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
  //     g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
  //     g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
  //     g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
  //     g2d.setFont(font)
  //     val fm = g2d.getFontMetrics()
  //     g2d.setColor(Color.BLACK)
  //     g2d.drawString(line, 0, fm.getAscent())
  //     g2d.dispose()
  //     Image.wrapAwt(img)

  //   }

  // }
