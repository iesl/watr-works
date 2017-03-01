package edu.umass.cs.iesl.watr
package watrcolors
package server

import corpora._
import geometry._

import com.sksamuel.scrimage
import scrimage._
import scrimage.{canvas => SC}
import watrmarks._
import docstore._

class LabelingServer(
  reflowDB: TextReflowDB,
  corpus: Corpus
) {

  private lazy val docStore = reflowDB.docstorage

  def rescale(bbox: LTBounds, page1: LTBounds, page2: LTBounds): LTBounds = {
    val LTBounds(l, t, w, h) = bbox
    val scaleX = page2.width/page1.width
    val scaleY = page2.height/page1.height
    val l2 = l * scaleX
    val t2 = t * scaleY
    val w2 = w * scaleX
    val h2 = h * scaleY
    val res = LTBounds(l2, t2, w2, h2)
    res
  }



  def ltBoundsToDrawables(bbox: LTBounds, pageGeometry: PageGeometry, imageGeometry: LTBounds): List[SC.Drawable] =  {
    val LTBounds(rl, rt, rw, rh) = rescale(bbox, pageGeometry.bounds, imageGeometry)
    val ctx = SC.Context.painter(Color(10, 10, 220, 40))
    val r = SC.Rect(rl.toInt, rt.toInt, rw.toInt, rh.toInt, ctx)
    val ctx2 = SC.Context.painter(Color(10, 10, 0, 150))
    val underline = SC.Line(x0=r.x, y0=r.y+r.height, r.x+r.width, r.y+r.height, ctx2)
    val leftline = SC.Line(x0=r.x, y0=r.y, r.x, r.y+r.height, ctx2)
    List[SC.Drawable](r.fill, underline, leftline)
  }

  def embossTargetRegion(targetRegion: TargetRegion, labels: Seq[Label]): Unit = {
    val pageNum = targetRegion.pageNum

    // val maybePageId = docStore.getPage(targetRegion.stableId, pageNum)
    // val maybePageImage = docStore.getPageImage(pageId)
    // val maybePageGeometry = docStore.getPageGeometry(pageId)

    // (maybePageId |@| maybePageImage |@| maybePageGeometry).apply({
    //   case (pageId, pageImageBytes, pageBounds) =>

    //   // val (pageW, pageH) = pageImage.dimensions
    //   // val imageGeometry = LTBounds(0, 0, pageW.toDouble, pageH.toDouble)

    //   // val maskCanvas = new SC.Canvas(
    //   //   Image.filled(pageW.toInt, pageH.toInt, Color.Transparent)
    //   // )

    //   // val hardcodeLabel = LB.VisualLine
    //   // // select all zones w/label on given page
    //   // val zones = reflowDB.selectZones(docId, pageId, hardcodeLabel)
    //   // val embossings = for {
    //   //   zone <- zones
    //   //   region <- zone.regions
    //   // } yield {
    //   //   ltBoundsToDrawables(region.bbox, pageGeometry, imageGeometry)
    //   // }

    //   // val embossedCanvas = maskCanvas.draw(embossings.flatten)
    //   // val overlay = pageImage.overlay(embossedCanvas.image, 0, 0)

    //   // val imageClipRegion @ LTBounds(clipL, clipT, clipW, clipH) =
    //   //   rescale(targetRegion.bbox, pageGeometry.bounds, imageGeometry)

    //   // // println(s"embossTargetRegion: clipping image to ${imageClipRegion} w/geom ${imageGeometry}")

    //   // val clippedPageImage = overlay.subimage(
    //   //   clipL.toInt,
    //   //   clipT.toInt,
    //   //   clipL.toInt + clipW.toInt,
    //   //   clipT.toInt + clipH.toInt
    //   // )

    //   // reflowDB.overwriteTargetRegionImage(targetRegion, clippedPageImage)
    //     ???
    // )}

  }

}

