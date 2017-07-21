package edu.umass.cs.iesl.watr
package images

import com.sksamuel.scrimage
import scrimage._
import scrimage.{canvas => SC}
import geometry._

// import geometry.syntax._
import utils.ExactFloats._

trait ImageManipulation {


  def createCanvas(dim: LTBounds): SC.Canvas = {
    val LTBounds.Ints(_, _, w, h) = dim
    new SC.Canvas(Image.filled(w, h, Color.White))
  }


  def rescale(bbox: LTBounds, page1: LTBounds, page2: LTBounds): LTBounds = {
    val LTBounds(l, t, w, h) = bbox
    val scaleX = page2.width/page1.width
    val scaleY = page2.height/page1.height
    val l2 = l * scaleX
    val t2 = t * scaleY
    val w2 = w * scaleX
    val h2 = h * scaleY

    LTBounds(l2, t2, w2, h2)
  }



  def ltBoundsToDrawables(
    bbox: LTBounds,
    pageGeometry: PageGeometry,
    imageGeometry: LTBounds,
    color: Color
  ): List[SC.Drawable] =  {
    val LTBounds.Ints(rl, rt, rw, rh) = rescale(bbox, pageGeometry.bounds, imageGeometry)

    val lightest = SC.Context.painter(color.copy(alpha=20))
    val lighter = SC.Context.painter(color.copy(alpha=40))
    val darker = SC.Context.painter(color.copy(alpha=100))

    val r = SC.Rect(rl, rt, rw, rh, lighter)
    // val r = SC.FilledRect(rl, rt, rw, rh, lightest)
    val underline = SC.Line(x0=r.x, y0=r.y+r.height, r.x+r.width, r.y+r.height, darker)
    val leftline = SC.Line(x0=r.x, y0=r.y, r.x, r.y+r.height, darker)


    List[SC.Drawable](r, underline, leftline)
  }

  // def embossTargetRegion(targetRegion: TargetRegion, labels: Seq[Label]): Unit = {
  //   val docId = targetRegion.docId
  //   val pageId = targetRegion.pageId

  //   val (pageImage, pageGeometry) = reflowDB.getPageImageAndGeometry(docId, pageId)

  //   val (pageW, pageH) = pageImage.dimensions
  //   val imageGeometry = LTBounds(0, 0, pageW.toDouble, pageH.toDouble)

  //   val maskCanvas = new SC.Canvas(
  //     Image.filled(pageW.asInt, pageH.asInt, Color.Transparent)
  //   )

  //   val hardcodeLabel = LB.VisualLine
  //   // select all zones w/label on given page
  //   val zones = reflowDB.selectZones(docId, pageId, hardcodeLabel)
  //   val embossings = for {
  //     zone <- zones
  //     region <- zone.regions
  //   } yield {
  //     ltBoundsToDrawables(region.bbox, pageGeometry, imageGeometry)
  //   }

  //   val embossedCanvas = maskCanvas.draw(embossings.flatten)
  //   val overlay = pageImage.overlay(embossedCanvas.image, 0, 0)

  //   val imageClipRegion @ LTBounds(clipL, clipT, clipW, clipH) =
  //     rescale(targetRegion.bbox, pageGeometry.bounds, imageGeometry)

  //   // println(s"embossTargetRegion: clipping image to ${imageClipRegion} w/geom ${imageGeometry}")

  //   val clippedPageImage = overlay.subimage(
  //     clipL.toInt,
  //     clipT.toInt,
  //     clipL.toInt + clipW.toInt,
  //     clipT.toInt + clipH.toInt
  //   )

  //   reflowDB.overwriteTargetRegionImage(targetRegion, clippedPageImage)
  // }




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
