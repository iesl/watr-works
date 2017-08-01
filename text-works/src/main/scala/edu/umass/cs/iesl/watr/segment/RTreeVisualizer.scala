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

class RTreeVisualizer(
  labelColors: Map[Label, Color]
  // vtrace: VisualTracer
) {
  implicit val writer = scrimage.nio.GifWriter.Progressive
  // implicit val writer = scrimage.nio.GifSequenceWriter

  def createRTreeImage(pageIndex: PageIndex, l0: Label, labels: Label*): Image = {
    val rTreeIndex = pageIndex.componentIndex

    val LTBounds(l, t, w, h) = pageIndex.pageGeometry.bounds
    val pageBounds = LTBounds(l, t, w+10, h+10)
    val pageCanvas = IM.createCanvas(pageBounds)

    val lbls = l0 :: labels.toList

    val overlays = rTreeIndex.getItems
      .filter { c =>
        lbls.contains(c.roleLabel)
      }
      .map { c => IM.ltBoundsToDrawables(c.bounds, pageIndex.pageGeometry, pageBounds, labelColors(c.roleLabel) ) }

    val embossedCanvas = pageCanvas.draw(overlays.flatten.reverse)

    embossedCanvas.image
  }

  def writeRTreeImage(dirname: String, filename: String, image: Image): Unit = {

    // val outRelDir = RelPath(new java.io.File(dirname))
    val outDir = fs.pwd / dirname
    if (!fs.exists(outDir)) {
      fs.mkdir(outDir)
    }

    val outPath = outDir / filename

    if (fs.exists(outPath)) {
      fs.rm(outPath)
    }
    fs.write(outPath, image.bytes)

  }

  def cleanRTreeImageFiles(dirname: String): Unit = {
    val outDir = fs.pwd / dirname
    if (fs.exists(outDir)) {
      fs.rm(outDir)
    }
  }
}
