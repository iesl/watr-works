package org.watrworks
package apps

import zio._
import zio.console._

import java.io.{
  IOException
}

import imageseg._
import org.bytedeco.opencv.opencv_core._
import org.watrworks.corpora.filesys.CorpusEntry
// import zio.console.Console.{ printLine, readLine, _ }


object ImageSegApp {
  import Pipelines._
  val LD = LineDetection

  val userPrompt: String => ZIO[Console, Throwable, String] = (prompt: String) =>
    for {
      // _      <- Console.printLine(s":> ${prompt}")
      _      <- Unsafe.unsafe { implicit unsafe =>   Console.printLine(s":> ${prompt}") }
      filter <- readLine
    } yield filter

  // val imgPath = "./corpus.d/cmp-lg9503025.pdf.d/page-images/page-4.opt.png"
  // val imgPath = "./corpus.d/101007bf01567112.pdf.d/page-images/page-5.opt.png"
  val imgPath = "corpus.d/10.1101-055202.d/page-images/page-6.opt.png"
  val pagesOfInterest: List[String] = List(
    "corpus.d/10.1101-055202.d/page-images/page-6.opt.png",
    "corpus.d/10.1101-055202.d/page-images/page-11.opt.png",
    "corpus.d/10.1101-055202.d/page-images/page-12.opt.png",
    "./corpus.d/cmp-lg9503025.pdf.d/page-images/page-4.opt.png",
    "./corpus.d/101007bf01567112.pdf.d/page-images/page-5.opt.png"
  )

  val showImageMat = (m: Mat, caption: String) =>
    for {
      closeImg <- LD.showImageUntilKeypress(m, caption)
      // _        <- userPrompt("Press Key Continue")
      // _        <- Task { closeImg.close() }
    } yield ()

  val tryKernelSizes = (mBin: Mat) =>
    for {
      kernelSize  <- userPrompt("Kernel Size> ").map(_.toInt)
      _           <- printLine(s"Using Kernel Size ${kernelSize}")
      hlineKernel <- LD.makeHLineKernel(kernelSize)
      vlineKernel <- LD.makeVLineKernel(kernelSize)
      mOpenH      <- LD.runOpen(mBin, hlineKernel)
      mOpenV      <- LD.runOpen(mBin, vlineKernel)
      mVH         <- LD.combineMats(mOpenV, mOpenH)
      mDisplay    <- LD.blend(mBin, 0.1, mVH)
      _           <- showImageMat(mDisplay, "Combined")
    } yield ()

  val chooseCorpusEntry: ZIO[Console, Throwable, CorpusEntry] = {
    val choose1 = (filter: String) =>
      for {
        entries: Chunk[CorpusEntry] <- corpusEntryStream(config(filter)).runCollect
                                         .absorbWith(_ => new IOException(""))
        _ <- printLine(s"Entries:")
        _ <- printLine(entries.map(e => e.entryDescriptor.split("/").last).mkString("\n"))
      } yield entries

    var fstr = ".*"

    val res: ZIO[Console, Throwable, Seq[CorpusEntry]] = (for {
      choices <- choose1(fstr)
      usr     <- userPrompt("Filter> ")
      _ = { fstr = usr }
    } yield choices).repeatUntil(entries => entries.length == 1)

    res.map(_.head)
  }

  val loadImage: String => ZIO[Console, Any, Mat] = (path) =>
    for {
      _       <- printLine(s"___ ImageSeg ___")
      initImg <- LD.readImageColor(path)
      _       <- printLine(s"Loaded ${path} = ${initImg}")
      _       <- showImageMat(initImg, "Initial Image")
    } yield initImg

  val loadImageGrayscale: String => ZIO[Console, Any, Mat] = (path) =>
    for {
      _       <- printLine(s"___ ImageSeg ___")
      initImg <- LD.readImage(path)
      _       <- printLine(s"Loaded ${path} = ${initImg}")
      _       <- showImageMat(initImg, "Initial Image")
    } yield initImg

  val cannyContoursApp: ZIO[Console, Any, Any] = for {
    initImg <- loadImage(imgPath)
    canny   <- LD.runCanny(initImg)
    _       <- showImageMat(canny, "Canny")
    _       <- tryKernelSizes(canny)
  } yield ()

  val doubleLineApp: ZIO[Console, Any, Any] = for {
    // initImg      <- loadImage(pagesOfInterest(1))
    initImg      <- loadImageGrayscale(pagesOfInterest(1))
    threshed     <- LD.runCanny(initImg)
    _            <- showImageMat(threshed, "Thresh")
    hlineKernelU <- LD.makeHLineKernelUp(30)
    hlineKernelD <- LD.makeHLineKernelDown(30)

    // hlineKernel <- hlineKernels
    mOpenHU    <- LD.runOpen(threshed, hlineKernelU)
    _          <- showImageMat(mOpenHU, "Opened")
    mOpenHD    <- LD.runOpen(threshed, hlineKernelD)
    _          <- showImageMat(mOpenHD, "Opened")
    composite  <- LD.blend(initImg, 0.1, mOpenHU)
    composite2  <- LD.blend(initImg, 0.1, mOpenHD)
    composite3 <- LD.blend(composite, 0.5, composite2)
    _          <- showImageMat(composite3, "Opened")
    // _          <- showImageMat(initImg, "Opened")
  } yield ()

  val textDetectApp: ZIO[Console, Any, Any] = for {
    initImg   <- loadImage(pagesOfInterest(2))
    textBoxes <- LD.runTextDetectionUsingTextBoxesCNN(initImg)
    composite <- LD.blend(initImg, 0.1, textBoxes)
    _         <- showImageMat(composite, "Text Boxes")
  } yield ()

  def go() = {
    val runtime = Runtime.default
    // runtime.unsafeRun(cannyContoursApp)
    runtime.unsafeRun(doubleLineApp)
  }

}