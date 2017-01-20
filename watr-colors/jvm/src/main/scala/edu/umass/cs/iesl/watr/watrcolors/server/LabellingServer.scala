package edu.umass.cs.iesl.watr
package watrcolors
package server


import corpora._
import textreflow._
import geometry._

// import autowire._
// import upickle.{default => UPickle}
// import UPickle._

// api: ClientSiteConn.Conn[WatrTableApi],

class LabelingServer(
  reflowDB: TextReflowDB,
  corpus: Corpus
) {


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

  def showPageImage(docId: String@@DocumentID, pagenum: Int): Unit = {
    val pageId = PageID(pagenum)
    val (pageImage, pageGeometry) = reflowDB.getPageImageAndGeometry(docId, pageId)
    val pageTargetRegion = TargetRegion(RegionID(0), docId, pageId, pageGeometry.bounds)

    showTargetRegion(pageTargetRegion, LB.VisualLine)
  }

  import com.sksamuel.scrimage
  import scrimage._
  import scrimage.canvas._
  import watrmarks._

  def ltBoundsToDrawables(bbox: LTBounds, pageGeometry: PageGeometry, imageGeometry: LTBounds): List[Drawable] =  {
    val LTBounds(rl, rt, rw, rh) = rescale(bbox, pageGeometry.bounds, imageGeometry)
    val ctx = Context.painter(Color(10, 10, 220, 40))
    val r = Rect(rl.toInt, rt.toInt, rw.toInt, rh.toInt, ctx)
    val ctx2 = Context.painter(Color(10, 10, 0, 150))
    val underline = Line(x0=r.x, y0=r.y+r.height, r.x+r.width, r.y+r.height, ctx2)
    val leftline = Line(x0=r.x, y0=r.y, r.x, r.y+r.height, ctx2)
    List[Drawable](r.fill, underline, leftline)
  }

  def embossTargetRegion(targetRegion: TargetRegion, labels: Seq[Label]): Unit = {
    val docId = targetRegion.docId
    val pageId = targetRegion.pageId

    val (pageImage, pageGeometry) = reflowDB.getPageImageAndGeometry(docId, pageId)

    val (pageW, pageH) = pageImage.dimensions
    val imageGeometry = LTBounds(0, 0, pageW.toDouble, pageH.toDouble)

    val maskCanvas = new Canvas(
      Image.filled(pageW.toInt, pageH.toInt, Color.Transparent)
    )

    val hardcodeLabel = LB.VisualLine
    // select all zones w/label on given page
    val zones = reflowDB.selectZones(docId, pageId, hardcodeLabel)
    val embossings = for {
      zone <- zones
      region <- zone.regions
    } yield {
      ltBoundsToDrawables(region.bbox, pageGeometry, imageGeometry)
    }

    val embossedCanvas = maskCanvas.draw(embossings.flatten)
    val overlay = pageImage.overlay(embossedCanvas.image, 0, 0)

    val imageClipRegion @ LTBounds(clipL, clipT, clipW, clipH) =
      rescale(targetRegion.bbox, pageGeometry.bounds, imageGeometry)

    // println(s"embossTargetRegion: clipping image to ${imageClipRegion} w/geom ${imageGeometry}")

    val clippedPageImage = overlay.subimage(
      clipL.toInt,
      clipT.toInt,
      clipL.toInt + clipW.toInt,
      clipT.toInt + clipH.toInt
    )

    reflowDB.overwriteTargetRegionImage(targetRegion, clippedPageImage)
  }

  // FIXME this is NOT a showTargetRegion function, it is hardcoded to show all VisualLines on a page image
  def showTargetRegion(targetRegion: TargetRegion, label: watrmarks.Label): TargetRegion = {

    val docId = targetRegion.docId
    val pageId = targetRegion.pageId

    val (pageImage, pageGeometry) = reflowDB.getPageImageAndGeometry(docId, pageId)

    // select all zones w/label on given page
    val zones = reflowDB.selectZones(docId, pageId, label)
    val (w, h) = pageImage.dimensions
    val pageImageBounds = LTBounds(0, 0, w.toDouble, h.toDouble)
    println(s"showTargetRegion: pageImage has dims ${w}, $h = ${pageImageBounds}")

    // val blank = Image.filled(w, h, Color(0, 0, 0, 20))
    val blank = Image.filled(w, h, Color.Transparent)
    val maskCanvas = new Canvas(blank)

    val zoneRects = for { zone <- zones } yield {
      zone.regions.flatMap({
        case TargetRegion(id, docId, pageId, bbox @ LTBounds(l, t, w, h)) =>
          val re @ LTBounds(rl, rt, rw, rh) = rescale(bbox, pageGeometry.bounds, pageImageBounds)
          val ctx = Context.painter(Color(10, 10, 220, 40))
          val r = Rect(rl.toInt, rt.toInt, rw.toInt, rh.toInt, ctx)
          val ctx2 = Context.painter(Color(10, 10, 0, 150))
          val underline = Line(x0=r.x, y0=r.y+r.height, r.x+r.width, r.y+r.height, ctx2)
          val leftline = Line(x0=r.x, y0=r.y, r.x, r.y+r.height, ctx2)
          List[Drawable](r.fill, underline, leftline)
      })
    }

    val rectsCanvas = maskCanvas.draw(zoneRects.flatten)

    val maskImage = rectsCanvas.image

    val overlay = pageImage.overlay(maskImage, 0, 0)

    val pageTargetRegion = TargetRegion(RegionID(0), docId, pageId, pageGeometry.bounds)
    // val labelUri = pageUri + "?l=" + label.fqn

    reflowDB.putTargetRegionImage(pageTargetRegion, overlay)
    pageTargetRegion

  }

  implicit class Labeler_RicherCorpusEntry(val theCorpusEntry: CorpusEntry) extends {
    def text(): Seq[TextReflow] = {
      ???
    }



  }
}
