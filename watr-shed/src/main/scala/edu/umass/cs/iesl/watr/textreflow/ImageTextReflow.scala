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
}
