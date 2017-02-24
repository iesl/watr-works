package edu.umass.cs.iesl.watr
package images

import com.sksamuel.scrimage._
import geometry._

import GeometryImplicits._

trait ImageManipulation {

  // implicit val writer = nio.PngWriter.NoCompression

  def cropTo(imageBytes: Array[Byte], cropBox: LTBounds, pageBounds: LTBounds): Image = {
    println(s"cropTo(len=${imageBytes.length}, cropBox=${cropBox}, pageBounds=${pageBounds})")
    cropTo(Image(imageBytes), cropBox, pageBounds)
  }

  def cropTo(image: Image, cropBox: LTBounds, pageBounds: LTBounds): Image = {
    println(s"page geometry is ${pageBounds.prettyPrint}")
    println(s"image geometry is width:${image.width}, height:${image.height}")
    val scaled = image.scaleTo(pageBounds.width.toInt, pageBounds.height.toInt)

    val left = cropBox.left // - pageBounds.left
    val top = cropBox.top // - pageBounds.left
    val right = pageBounds.right - cropBox.right
    val bottom = pageBounds.bottom - cropBox.bottom

    val trimmed = scaled.trim(
      left   = left.toInt,
      top    = top.toInt,
      right  = right.toInt,
      bottom = bottom.toInt
    )

    val rescaleFactorX: Double = image.width.toDouble / pageBounds.width
    val rescaleFactorY: Double = image.height.toDouble / pageBounds.height

    println(s"scaling factors are  scaleX: ${rescaleFactorX}, scaleY: ${rescaleFactorY}")

    // .pad(3, Color.Transparent)
    trimmed
      .scale(rescaleFactorX)
  }

  // def textToImage(text: String): Image = {
  //   val blank = Image.filled(300, 200, X11Colorlist.White)
  //   val canvas1 = new Canvas(blank)

  //   for {
  //     (line, n) <- lines(text).zipWithIndex
  //   } yield {
  //     canvas1.draw(Drawable(line, 0, n*yscale.toInt))
  //   }
  //   canvas1.image
  // }

}

object ImageManipulation extends ImageManipulation
