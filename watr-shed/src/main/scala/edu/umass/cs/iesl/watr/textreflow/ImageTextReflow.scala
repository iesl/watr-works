package edu.umass.cs.iesl.watr
package textreflow 


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


  def textToImage(text: String): Image = {
    val blank = Image.filled(300, 200, X11Colorlist.White)
    val canvas1 = new Canvas(blank)

    for {
      (line, n) <- lines(text).zipWithIndex
    } yield {
      canvas1.draw(Drawable(line, 0, n*yscale.toInt))
    }
    canvas1.image
  }

  import PageComponentImplicits._

  def textReflowToImage(pageReflow: TextReflow): Image = {
    val vlines =  pageReflow.sliceLabels(LB.VisualLine)

    // total page target region
    val TargetRegion(id, docId, pageId, LTBounds(l, t, w, h) ) =
      pageReflow.targetRegions.reduce(_ union _)

    val blank = Image.filled((w*10).toInt, (h*10).toInt, X11Colorlist.White)
    val canvas = new Canvas(blank)
    for ((vline, n) <- vlines.zipWithIndex) yield {
      val ltext = vline.toText()
      canvas.draw(Drawable(ltext, 0, 10*n*yscale.toInt))
    }

    canvas.image.scale(0.10, ScaleMethod.FastScale)
  }
}
