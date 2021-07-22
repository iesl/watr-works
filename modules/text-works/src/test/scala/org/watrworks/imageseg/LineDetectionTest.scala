package org.watrworks
package imageseg

import OpenCVUtils._
import org.bytedeco.opencv.global.opencv_core._
import org.bytedeco.opencv.opencv_core._


class LineDetectionTest extends utils.SegmentationTestUtils {
  behavior of "Image-based Line Detection"

  import org.bytedeco.javacpp.indexer.UByteIndexer

  it should "demo mat" in {

    val mat = new Mat(2, 12, CV_8U)

    val indx = mat.createIndexer[UByteIndexer]()

    for {
      col <- 0 until mat.cols()
    } {
      println(s"put: ${col}")
      indx.put(0L, col.toLong, 0)
      indx.put(1L, col.toLong, 1)
    }
    // val mat: Mat = ImgProc.getStructuringElement(ImgProc.MORPH_RECT, new Size(4, 1));

    printInfo(mat)
    for {
      col <- 0 until mat.cols()
      row <- 0 until mat.rows()
    } {
      val c = indx.get(row, col)
      println(s"${row}/${col} = ${c}")
    }

  }

}
