package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}
import edu.umass.cs.iesl.watr.tracing.VisualTracer
import images.{ImageManipulation => IM}

import watrmarks._
import geometry._
import spindex._
import utils.ExactFloats._
import com.sksamuel.scrimage
import scrimage._

import scala.concurrent.duration._
import watrmarks.{StandardLabels => LB, _}
import TypeTags._
import com.sksamuel.scrimage.{X11Colorlist => Clr, Color}
import ammonite.{ops => fs}, fs._

case class RTreeVisualizer(
  pageIndex: PageIndex,
  labelColors: Map[Label, Color],
  outputRoot: Path,
  vtrace: VisualTracer
) {
  implicit val writer = scrimage.nio.GifWriter.Progressive // nio.GifSequenceWriter
  val rTreeIndex = pageIndex.componentRTree
  val pageNum = pageIndex.pageGeometry.id

  def segvisFile(name: String) = s"${name}.pg${pageNum}.png"
  def segvisGifFile(name: String) = s"${name}.pg${pageNum}.gif"

  def writeRTreeImage(name: String, l0: Label, labels: Label*): Unit = {
    vtrace {
      val image = createRTreeImage(l0, labels:_*)
      writeImage(name, image)
    }
  }

  def indicateRegionImage(bboxes: Seq[LTBounds]): Image = {

    val LTBounds(l, t, w, h) = pageIndex.pageGeometry.bounds
    val pageBounds = LTBounds(l, t, w, h)
    val pageCanvas = IM.createCanvas(pageBounds)


    // val alphaStep = 200 / bboxes.length
    val overlays = bboxes.zipWithIndex.map { case (bbox, i) =>
      // val alpha = 255 - (i*alphaStep)
      IM.ltBoundsToDrawablesFilled(bbox, pageIndex.pageGeometry, pageBounds, Clr.Blue, 200)
    }

    val embossedCanvas = pageCanvas.draw(
      (overlays.flatten).reverse
    )

    embossedCanvas.image
  }

  def indicateRegion(name: String, bbox: LTBounds): Unit = {
    indicateRegions(name, Seq(bbox))
  }

  def indicateRegions(name: String, bboxes: Seq[LTBounds]): Unit = {
    val image = indicateRegionImage(bboxes)

    writeImage(name, image)
  }


  def createRTreeImage(l0: Label, labels: Label*): Image = {

    val LTBounds(l, t, w, h) = pageIndex.pageGeometry.bounds
    val pageBounds = LTBounds(l, t, w, h)
    val pageCanvas = IM.createCanvas(pageBounds)

    val lbls = labels.toList

    val highlight = rTreeIndex.getItems
      .filter { _.hasLabel(l0) }
      .map { c => IM.ltBoundsToDrawablesFilled(c.bounds, pageIndex.pageGeometry, pageBounds, labelColors(c.roleLabel), 10 ) }

    val overlays = rTreeIndex.getItems
      .filter { c =>
        lbls.contains(c.roleLabel) || lbls.exists(c.hasLabel(_))
      }
      .map { c => IM.ltBoundsToDrawablesFilled(c.bounds, pageIndex.pageGeometry, pageBounds, labelColors(c.roleLabel), 200 ) }

    val embossedCanvas = pageCanvas.draw(
      (highlight.flatten ++ overlays.flatten)
    )

    embossedCanvas.image
  }

  private def writeImage(name: String, image: Image): Unit = {

    // val outRelDir = RelPath(new java.io.File(dirname))
    if (!fs.exists(outputRoot)) {
      fs.mkdir(outputRoot)
    }

    val outPath = outputRoot / segvisFile(name)

    if (fs.exists(outPath)) {
      fs.rm(outPath)
    }
    fs.write(outPath, image.bytes)

  }

  def cleanRTreeImageFiles(): Unit = {
    vtrace {
      if (fs.exists(outputRoot)) {
        fs.rm(outputRoot)
      }
    }
  }

  def gifBuilder(name: String, frameRate: FiniteDuration) =
    GifBuilder(name, frameRate, this)

}

case class GifBuilder(
  name: String,
  frameRate: FiniteDuration,
  vis: RTreeVisualizer
) {

  import com.sksamuel.scrimage.nio.StreamingGifWriter
  import java.awt.image.BufferedImage;
  import scala.collection.mutable
  import com.sksamuel.scrimage
  import scrimage.Image

  val rTreeIndex = vis.rTreeIndex
  val vtrace = vis.vtrace
  // val pageNum = pageIndex.pageGeometry.id


  val gifFrames = mutable.ArrayBuffer[Image]()

  def mkPageRegion(bounds: LTBounds) = PageRegion(
    StablePage(DocumentID(""), PageNum(0)),
    bounds
  )

  def indicateRegion(caption: String, bounds:LTBounds): Unit = {
    vtrace {
      val image = vis.indicateRegionImage(Seq(bounds))
      addFrameWithCaption(caption, image)
    }
  }
  def indicate(caption: String, bounds:LTBounds, labels: Label*): Unit = {
    vtrace {
      val tmpRegion = RegionComponent(ComponentID(0), LB.Marked, mkPageRegion(bounds))

      rTreeIndex.add(tmpRegion)
      addFrame(caption, LB.Marked, LB.Marked)
      addFrame(s"+${caption}", LB.Marked, labels:_*)
      rTreeIndex.remove(tmpRegion)

    }
  }

  def indicate(caption: String, bounds:Seq[LTBounds], labels: Label*): Unit = {
    vtrace {
      val tmps = bounds.map { b =>
        val tmpRegion = RegionComponent(ComponentID(0), LB.Marked, mkPageRegion(b))
        rTreeIndex.add(tmpRegion)
        tmpRegion
      }
      addFrame(" "+caption, LB.Marked)
      addFrame("+"+caption, LB.Marked, labels:_*)
      // deleteComponentsWithLabel(LB.Marked)
      tmps.foreach { tmp => rTreeIndex.remove(tmp) }
    }
  }

  def addFrame(caption: String, l0: Label, labels: Label*): Unit = {
    vtrace {
      val img0 = vis.createRTreeImage(l0, labels:_*)
      addFrameWithCaption(caption, img0)
    }
  }

  def addFrameWithCaption(caption: String, frame: Image): Unit = {
    vtrace {
      val filter = new scrimage.canvas.CaptionFilter(
        caption,
        textColor=Clr.Black,
        textAlpha=0.8,
        captionBackground= Clr.Grey20,
        captionAlpha=0.4
      )
      filter.apply(frame)
      gifFrames.append(frame)
    }
  }


  def finish(): Unit = {
    vtrace {
      val gifWriter = StreamingGifWriter().withFrameDelay(frameRate)
      // val outputPath = fs.pwd / segvisRoot / segvisGifFile(name)
      val outputPath =  vis.outputRoot / vis.segvisGifFile(name)
      if (fs.exists(outputPath)) {
        fs.rm(outputPath)
      }

      val gifStream =  gifWriter.prepareStream(outputPath.toNIO, BufferedImage.TYPE_INT_ARGB)

      // println(s"gifFrames.len (final): ${gifFrames.length}")
      gifFrames.foreach { img => gifStream.writeFrame(img) }
      gifStream.finish()
    }
  }
}
