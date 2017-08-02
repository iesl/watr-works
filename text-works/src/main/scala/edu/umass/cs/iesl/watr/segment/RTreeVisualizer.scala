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

class RTreeVisualizer(
  pageIndex: PageIndex,
  labelColors: Map[Label, Color],
  outputRoot: Path,
  vtrace: VisualTracer
) {
  implicit val writer = scrimage.nio.GifWriter.Progressive // nio.GifSequenceWriter
  val rTreeIndex = pageIndex.componentIndex
  val pageNum = pageIndex.pageGeometry.id

  def segvisFile(name: String) = s"${name}.pg${pageNum}.png"
  def segvisGifFile(name: String) = s"${name}.pg${pageNum}.gif"


  def writeRTreeImage(name: String, l0: Label, labels: Label*): Unit = {
    vtrace.ifTrace{
      val image = createRTreeImage(l0, labels:_*)
      writeImage(name, image)
    }
  }

  private def createRTreeImage(l0: Label, labels: Label*): Image = {

    val LTBounds(l, t, w, h) = pageIndex.pageGeometry.bounds
    val pageBounds = LTBounds(l, t, w+10, h+10)
    val pageCanvas = IM.createCanvas(pageBounds)

    val lbls = l0 :: labels.toList

    val overlays = rTreeIndex.getItems
      .filter { c =>
        lbls.contains(c.roleLabel) || lbls.exists(c.hasLabel(_))
      }
      .map { c => IM.ltBoundsToDrawables(c.bounds, pageIndex.pageGeometry, pageBounds, labelColors(c.roleLabel) ) }

    val embossedCanvas = pageCanvas.draw(overlays.flatten.reverse)

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
    vtrace.ifTrace {
      if (fs.exists(outputRoot)) {
        fs.rm(outputRoot)
      }
    }
  }

  def gifBuilder(name: String, frameRate: FiniteDuration) =
    GifBuilder(name, frameRate)

  case class GifBuilder(name: String, frameRate: FiniteDuration) {

    import com.sksamuel.scrimage.nio.StreamingGifWriter
    import java.awt.image.BufferedImage;
    import scala.collection.mutable
    import com.sksamuel.scrimage
    import scrimage.Image


    val gifFrames = mutable.ArrayBuffer[Image]()

    def mkPageRegion(bounds: LTBounds) = PageRegion(
      RecordedPageID(
        PageID(0),
        StablePageID(DocumentID(""), PageNum(0))
      ),
      bounds
    )

    def indicate(caption: String, bounds:LTBounds, labels: Label*): Unit = {
      vtrace.ifTrace {
        val tmpRegion = RegionComponent(ComponentID(0), LB.Marked, mkPageRegion(bounds))

        rTreeIndex.add(tmpRegion)
        addFrame(caption, LB.Marked)
        addFrame(s"+${caption}", LB.Marked, labels:_*)
        rTreeIndex.remove(tmpRegion)

      }
    }

    def indicate(caption: String, bounds:Seq[LTBounds], labels: Label*): Unit = {
      vtrace.ifTrace {
        val tmps = bounds.map { b =>
          val tmpRegion = RegionComponent(ComponentID(0), LB.Marked, mkPageRegion(b))
          rTreeIndex.add(tmpRegion)
          tmpRegion
        }
        addFrame(caption, LB.Marked)
        addFrame("+"+caption, LB.Marked, labels:_*)
        // deleteComponentsWithLabel(LB.Marked)
        tmps.foreach { tmp => rTreeIndex.remove(tmp) }
      }
    }

    def addFrame(caption: String, l0: Label, labels: Label*): Unit = {
      vtrace.ifTrace {
        val img0 = createRTreeImage(l0, labels:_*)
        val filter = new scrimage.canvas.CaptionFilter(
          caption,
          textColor=Clr.Black,
          textAlpha=0.8,
          captionBackground= Clr.Grey20,
          captionAlpha=0.4
        )
        filter.apply(img0)
        gifFrames.append(img0)
      }
    }


    def finish(): Unit = {
      vtrace.ifTrace {
        val gifWriter = StreamingGifWriter().withFrameDelay(frameRate)
        // val outputPath = fs.pwd / segvisRoot / segvisGifFile(name)
        val outputPath =  outputRoot / segvisGifFile(name)
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
}
