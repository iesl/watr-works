package org.watrworks
package imageseg

import org.bytedeco.opencv.global.opencv_core._
import org.bytedeco.opencv.global.opencv_imgcodecs._
import org.bytedeco.opencv.global.{opencv_imgproc => ImgProc}
import org.bytedeco.opencv.opencv_core._
import org.bytedeco.opencv.opencv_imgproc.Vec2fVector
import org.bytedeco.javacpp.indexer.UByteIndexer

import OpenCVUtils._
import zio._
import zio.console._
import ImgProc._
import math._
import org.bytedeco.opencv.opencv_dnn._
import org.bytedeco.opencv.opencv_text._
import org.bytedeco.opencv.global.opencv_text._

object LineDetection {

  def readImage(imgPath: String): Task[Mat] = Task {
    val r        = imread(imgPath, IMREAD_GRAYSCALE)
    val channels = r.channels()
    println(s"Image Channels = ${channels}")
    r
  }
  def readImageColor(imgPath: String): Task[Mat] = Task {
    val r        = imread(imgPath)
    val channels = r.channels()
    println(s"Image Channels = ${channels}")
    r
  }
  def writeImage(mat: Mat, imgPath: String): Task[Unit] = Task {
    imwrite(imgPath, mat)
  }

  def showImage(mat: Mat, caption: String): Task[Closeable] = Task {
    show(mat, caption)
  }
  def showImageUntilKeypress(mat: Mat, caption: String): Task[Unit] = Task {
    showUntilKeypress(mat, caption)
  }

  def runThreshold(in: Mat): Task[Mat] = Task {
    val out = new Mat()
    // ImgProc.threshold(in, out, 128, 255, ImgProc.THRESH_BINARY_INV)
    ImgProc.threshold(in, out, 128, 255, ImgProc.THRESH_OTSU)
    out
  }

  def runTextDetectionUsingSWT(in: Mat): Task[Mat] = Task {
    // detectTextSWT

    ???
  }

  def runTextDetectionUsingTextBoxesCNN(in: Mat): Task[Mat] = Task {
    // Load model weights
    val detector = TextDetectorCNN.create(
      "./models.d/textbox.prototxt",
      "./models.d/TextBoxes_icdar13.caffemodel"
    )

    val rectVec  = new RectVector();
    val floatVec = new FloatVector();
    detector.detect(in, rectVec, floatVec)

    val detected = rectVec.size()
    println(s"detected ${detected} rects ")
    val out = in.clone()
    for (i <- 0L until detected) {
      val r = rectVec.get(i)
      ImgProc.rectangle(out, r, new Scalar(128, 0, 200, 0))
    }

    out

  }

  def runCanny(in: Mat): Task[Mat] = Task {
    val canny        = new Mat()
    val threshold1   = 125d
    val threshold2   = 350d
    val apertureSize = 3
    ImgProc.Canny(in, canny, threshold1, threshold2, apertureSize, false /*L2 gradient*/ )
    canny
  }

  def runHough(in: Mat): Task[Mat] = Task {
    // Hough transform for line detection
    // val storage                    = cvCreateMemStorage(0)
    // val method                     = HOUGH_STANDARD
    val lines                      = new Vec2fVector()
    val distanceResolutionInPixels = 1
    val angleResolutionInRadians   = math.Pi / 180
    val minimumVotes               = 80
    val srn                        = 0.0
    val stn                        = 0.0
    val min_theta                  = 0.0
    val max_theta                  = CV_PI
    HoughLines(in, lines, distanceResolutionInPixels, angleResolutionInRadians, minimumVotes, srn, stn, min_theta, max_theta)

    // Draw lines on the canny contour image
    // val result = new Mat()
    val result = in.clone()
    // in.copyTo(result)
    cvtColor(in, result, COLOR_GRAY2BGR)
    println(s"Hough found ${lines.size()} lines")
    for (i <- 0L until lines.size().toLong) {
      val rho           = lines.get(i).get(0)
      val theta: Double = lines.get(i).get(1).toDouble

      val (pt1, pt2) = if (theta < Pi / 4.0 || theta > 3.0 * Pi / 4.0) {
        // ~vertical line
        // point of intersection of the line with first row
        val p1 = new Point(round(rho / cos(theta)).toInt, 0)
        // point of intersection of the line with last row
        val p2 = new Point(round((rho - result.rows * sin(theta)) / cos(theta)).toInt, result.rows)
        (p1, p2)
      } else {
        // ~horizontal line
        // point of intersection of the line with first column
        val p1 = new Point(0, round(rho / sin(theta)).toInt)
        // point of intersection of the line with last column
        val p2 = new Point(result.cols, round((rho - result.cols * cos(theta)) / sin(theta)).toInt)
        (p1, p2)
      }
      // draw a white line
      line(result, pt1, pt2, new Scalar(0, 0, 255, 0), 2, LINE_8, 0)
    }

    result
  }

  def runAdaptiveThreshold(in: Mat): Task[Mat] = Task {
    bitwise_not(in, in)
    val out       = new Mat()
    val maxVal    = 244
    val blockSize = 15
    val const     = -2
    ImgProc.adaptiveThreshold(in, out, maxVal, ImgProc.ADAPTIVE_THRESH_GAUSSIAN_C, ImgProc.THRESH_BINARY, blockSize, const)
    out

  }

  def runBinarize(in: Mat): Task[Mat] = Task {
    bitwise_not(in, in)
    val bw = new Mat()
    ImgProc.adaptiveThreshold(in, bw, 244, ImgProc.ADAPTIVE_THRESH_MEAN_C, ImgProc.THRESH_BINARY, 15, -2)
    bw
  }
  def combineMats(mat1: Mat, mat2: Mat): Task[Mat] = Task {
    val matOut = new Mat()
    addWeighted(mat1, 0.5, mat2, 0.5, 0, matOut)
    matOut
  }

  def blend(mat1: Mat, alpha: Double, mat2: Mat): Task[Mat] = Task {
    val matOut = new Mat()
    val beta   = 1.0 - alpha
    val gamma  = 0d
    addWeighted(mat1, alpha, mat2, beta, gamma, matOut)
    matOut
  }

  def runErode(in: Mat, kernel: Mat): Task[Mat] = Task {
    val out = in.clone()
    ImgProc.erode(in, out, kernel);
    out
  }

  def runDilate(in: Mat, kernel: Mat): Task[Mat] = Task {
    val out = in.clone()
    ImgProc.dilate(in, out, kernel);
    out
  }

  def runClose(in: Mat, kernel: Mat): Task[Mat] =
    runDilate(in, kernel).flatMap(runErode(_, kernel))

  def runOpen(in: Mat, kernel: Mat): Task[Mat] =
    runErode(in, kernel).flatMap(runDilate(_, kernel))

  def makeHLineKernel0(len: Int): Task[Mat] = Task {
    ImgProc.getStructuringElement(ImgProc.MORPH_RECT, new Size(len, 1));
  }
  def makeHLineKernel(len: Int): Task[Mat] = Task {
    val mat = new Mat(2, len, CV_8U)

    val index = mat.createIndexer[UByteIndexer]()

    for { col <- 0 until mat.cols() } {
      index.put(0L, col.toLong, 0)
      index.put(1L, col.toLong, 1)
    }
    mat
  }

  // val FlipX = 0
  val FlipY = 1
  // val FlipXY = -1
  // flip(mat, mflip, FlipY)
  def makeHLineKernelUp(len: Int): Task[Mat] =
    makeHLineKernel(len)

  def makeHLineKernelDown(len: Int): Task[Mat] =
    makeHLineKernel(len).map { mat =>
      val mflip = mat.clone()
      flip(mat, mflip, FlipY)
      mflip
    }

  def makeVLineKernel0(len: Int): Task[Mat] = Task {
    ImgProc.getStructuringElement(ImgProc.MORPH_RECT, new Size(1, len));
  }
  def makeVLineKernel(len: Int): Task[Mat] = Task {
    val mat = new Mat(len, 2, CV_8U)

    val index = mat.createIndexer[UByteIndexer]()

    for {
      row <- 0 until mat.rows()
    } {
      index.put(row.toLong, 0L, 0)
      index.put(row.toLong, 1L, 1)
    }

    mat
  }

  def makeRectKernel(h: Int, w: Int): Task[Mat] = Task {
    ImgProc.getStructuringElement(ImgProc.MORPH_RECT, new Size(w, h));
  }

  val writePng: (Mat, String) => ZIO[Console, Any, Any] = (mat, path) => {
    val finalPath = s"${path}.lines.png"
    for {
      _ <- putStrLn(s"Writing ${finalPath}")
      _ <- writeImage(mat, finalPath)
    } yield ()
  }

  def runLineDetection(
    imgPath: String
  ): Unit = {

    println(s"Segmenting page Image ${imgPath}")

    val cannyApp: ZIO[Console, Any, Any] = for {
      initImg     <- readImageColor(imgPath)
      _           <- putStrLn(s"Loaded ${imgPath} = ${initImg}")
      kernelSize  <- Task(40)
      threshed    <- runCanny(initImg)
      hlineKernel <- makeHLineKernel(kernelSize)
      vlineKernel <- makeVLineKernel(kernelSize)
      rKernel     <- makeRectKernel(5, 5)
      mOpenH      <- runOpen(threshed, hlineKernel)
      mOpenV      <- runOpen(threshed, vlineKernel)
      mVH         <- combineMats(mOpenV, mOpenH)
      mVHDilate   <- runDilate(mVH, rKernel)
      mDisplay    <- blend(initImg, 0.1, mVHDilate)
      _           <- writePng(mDisplay, imgPath)
    } yield ()

    val hvlineApp: ZIO[Console, Any, Any] = for {
      initImg     <- readImage(imgPath)
      _           <- putStrLn(s"Loaded ${imgPath} = ${initImg}")
      kernelSize  <- Task(40)
      thresh      <- runAdaptiveThreshold(initImg)
      hlineKernel <- makeHLineKernel(kernelSize)
      vlineKernel <- makeVLineKernel(kernelSize)
      rKernel     <- makeRectKernel(5, 5)
      mOpenH      <- runOpen(thresh, hlineKernel)
      mOpenV      <- runOpen(thresh, vlineKernel)
      mVH         <- combineMats(mOpenV, mOpenH)
      mVHDilate   <- runDilate(mVH, rKernel)
      mDisplay    <- blend(initImg, 0.1, mVHDilate)
      _           <- writePng(mDisplay, imgPath)
    } yield ()

    val runtime = Runtime.default
    // runtime.unsafeRun(app)
    runtime.unsafeRunSync(hvlineApp)

  }
}
