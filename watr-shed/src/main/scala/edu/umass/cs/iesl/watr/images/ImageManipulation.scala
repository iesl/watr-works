package edu.umass.cs.iesl.watr
package images

import com.sksamuel.scrimage._
import geometry._

// import geometry.syntax._
import utils.ExactFloats._

trait ImageManipulation {


  def cropTo(imageBytes: Array[Byte], cropBox: LTBounds, pageBounds: LTBounds): Image = {
    // println(s"cropTo(len=${imageBytes.length}, cropBox=${cropBox}, pageBounds=${pageBounds})")
    cropTo(Image(imageBytes), cropBox, pageBounds)
  }

  def cropTo(image: Image, cropBox: LTBounds, pageBounds: LTBounds): Image = {
    // println(s"page geometry is ${pageBounds.prettyPrint}")
    // println(s"image geometry is width:${image.width}, height:${image.height}")
    val scaled = image.scaleTo(pageBounds.width.asInt, pageBounds.height.asInt)

    val left = cropBox.left // - pageBounds.left
    val top = cropBox.top // - pageBounds.left
    val right = pageBounds.right - cropBox.right
    val bottom = pageBounds.bottom - cropBox.bottom

    val trimmed = scaled.trim(
      left   = left.asInt,
      top    = top.asInt,
      right  = right.asInt,
      bottom = bottom.asInt
    )

    val rescaleFactorX: Double = image.width.toDouble / pageBounds.width.asDouble()
    // val rescaleFactorY: Double = image.height.toDouble / pageBounds.height.asDouble()
    // println(s"scaling factors are  scaleX: ${rescaleFactorX}, scaleY: ${rescaleFactorY}")

    trimmed.scale(rescaleFactorX)
  }

}

object ImageManipulation extends ImageManipulation
