package edu.umass.cs.iesl.watr
package images

import com.sksamuel.scrimage
import scrimage._
import scrimage.{canvas => SC}
import geometry._

import utils.ExactFloats._

trait ImageManipulation {

  def createCanvas(dim: LTBounds): SC.Canvas = {
    val LTBounds.Ints(_, _, w, h) = dim
    new SC.Canvas(Image.filled(w+40, h+40, Color.White))
  }


  def rescale(bbox: LTBounds, page1: LTBounds, page2: LTBounds): LTBounds = {
    val LTBounds(l, t, w, h) = bbox
    val scaleX = page2.width/page1.width
    val scaleY = page2.height/page1.height
    val l2 = l * scaleX
    val t2 = t * scaleY
    val w2 = w * scaleX
    val h2 = h * scaleY

    LTBounds(l2+20, t2+20, w2, h2)
  }



  def ltBoundsToDrawables(
    bbox: LTBounds,
    pageGeometry: PageGeometry,
    imageGeometry: LTBounds,
    color: Color
  ): List[SC.Drawable] =  {
    val LTBounds.Ints(rl, rt, rw, rh) = rescale(bbox, pageGeometry.bounds, imageGeometry)

    val lighter = SC.Context.painter(color.copy(alpha=128))
    val darker = SC.Context.painter(color.copy(alpha=255))
    val r = SC.Rect(rl, rt, rw, rh, lighter)
    val underline = SC.Line(x0=r.x, y0=r.y+r.height, r.x+r.width, r.y+r.height, darker)
    val upperLine = SC.Line(x0=r.x, y0=r.y, r.x+r.width, r.y, darker)
    val leftline = SC.Line(x0=r.x, y0=r.y, r.x, r.y+r.height, darker)


    List[SC.Drawable](underline, leftline, upperLine)
  }

  def ltBoundsToDrawablesFilled(
    bbox: LTBounds,
    pageGeometry: PageGeometry,
    imageGeometry: LTBounds,
    color: Color,
    alpha: Int = 128
  ): List[SC.Drawable] =  {
    val LTBounds.Ints(rl, rt, rw, rh) = rescale(bbox, pageGeometry.bounds, imageGeometry)
    val draw0 = ltBoundsToDrawables(bbox, pageGeometry, imageGeometry, color)
    val fill = SC.Context.painter(color.copy(alpha=alpha))
    val r = SC.FilledRect(rl+2, rt+2, rw-2, rh-2, fill)

    r :: draw0
  }


  def cropTo(imageBytes: Array[Byte], cropBox: LTBounds, pageBounds: LTBounds): Image = {
    // val img = Image.filled(2, 2, 3, 3)
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
