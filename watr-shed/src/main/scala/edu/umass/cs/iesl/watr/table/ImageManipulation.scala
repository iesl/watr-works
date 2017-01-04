package edu.umass.cs.iesl.watr
package table

// import acyclic.file
import com.sksamuel.scrimage._
import geometry._
import GeometricFigure._
import EnrichGeometricFigures._


trait ImageManipulation {
  // - get image for specified pdf page(s)
  // - clip image to bounds
  // - load image as byte array (to return to client via http get)

  def cropTo(image: Image, cropBox: LTBounds, pageGeometry: PageGeometry): Image = {
    println(s"page geometry is ${pageGeometry.bounds.prettyPrint}")
    println(s"image geometry is width:${image.width}, height:${image.height}")
    val pageBounds = pageGeometry.bounds
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

    trimmed.scale(rescaleFactorX)
  }

}

object ImageManipulation extends ImageManipulation

