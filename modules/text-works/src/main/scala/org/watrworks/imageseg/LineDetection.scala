package org.watrworks
package imageseg

import java.awt.Color
import java.awt.image.BufferedImage

import OpenCVUtils.{wrapInIntPointer, wrapInMatVector, show}
import org.bytedeco.javacpp._
import org.bytedeco.javacpp.indexer.FloatRawIndexer
// import org.bytedeco.javacpp.indexer._
// import org.bytedeco.javacpp.{FloatPointer, IntPointer}
// import org.bytedeco.javacv._
// import org.bytedeco.javacv.{CanvasFrame, OpenCVFrameConverter}
// import org.bytedeco.opencv.global.opencv_calib3d._
// import org.bytedeco.opencv.global.opencv_objdetect._
// import org.bytedeco.opencv.opencv_calib3d._
// import org.bytedeco.opencv.opencv_imgproc._
// import org.bytedeco.opencv.opencv_objdetect._
// import org.opencv.core.MatOfRect
// import org.opencv.core.Point
// import org.opencv.core.Scalar
// import org.opencv.imgcodecs.Imgcodecs
// import org.opencv.imgproc.Imgproc
// import org.opencv.objdetect.CascadeClassifier
// import reflect._
import org.bytedeco.javacpp.indexer.UByteIndexer
import org.bytedeco.opencv.global.opencv_core._
import org.bytedeco.opencv.global.opencv_imgcodecs._
import org.bytedeco.opencv.global.opencv_imgproc._
import org.bytedeco.opencv.global.{opencv_imgproc => ImgProc}
import org.bytedeco.opencv.opencv_core._
// import org.opencv.core.Core

// import scala.Ordering.Float.IeeeOrdering

object LineDetection {
// We must load the native library before using any OpenCV functions.
  // You must load this library _exactly once_ per Java invocation.
  // If you load it more than once, you will get a java.lang.UnsatisfiedLinkError.
  // System.loadLibrary(Core.NATIVE_LIBRARY_NAME)

  def runLineDetection(
    imgPath: String
  ): Unit = {

    println(s"Segmenting page Image ${imgPath}")

    println("Reading image..")
    //  img = cv2.imread(img_for_box_extraction_path, 0)  # Read the image
    val imgMatrix = imread(imgPath)

    val thresholdOut = new Mat()
    //  (thresh, img_bin) = cv2.threshold(img, 128, 255, cv2.THRESH_BINARY | cv2.THRESH_OTSU)  # Thresholding the image
    val thresh = ImgProc.threshold(
      imgMatrix,
      thresholdOut,
      128,
      255,
      ImgProc.THRESH_BINARY // | ImgProc.THRESH_OTSU
    )
    //>>  img_bin = 255-img_bin  # Invert the image
    // Create inverted lookup table

    show(imgMatrix, "inital image")
    val dim  = 256
    val lut  = new Mat(1, dim, CV_8U)
    val lutI = lut.createIndexer().asInstanceOf[UByteIndexer]
    for (i <- 0 until dim) {
      lutI.put(i, (dim - 1 - i).toByte)
    }

    // Apply look-up
    val dest = Histogram1D.applyLookUp(thresholdOut, lut)

    //>>  print("Storing binary image to Images/Image_bin.jpg..")
    //>>  cv2.imwrite("Images/Image_bin.jpg",img_bin)

    //>>  print("Applying Morphological Operations..")
    //>>  # Defining a kernel length
    //>>  kernel_length = np.array(img).shape[1]//40

    //>>  # A verticle kernel of (1 X kernel_length), which will detect all the verticle lines from the image.
    //>>  verticle_kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (1, kernel_length))
    //>>  # A horizontal kernel of (kernel_length X 1), which will help to detect all the horizontal line from the image.
    //>>  hori_kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (kernel_length, 1))

    // ImgProc.getStructuringElement()
    //>>  # A kernel of (3 X 3) ones.
    //>>  kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))

    //>>  # Morphological operation to detect verticle lines from an image
    //>>  img_temp1 = cv2.erode(img_bin, verticle_kernel, iterations=3)
    //>>  verticle_lines_img = cv2.dilate(img_temp1, verticle_kernel, iterations=3)
    //>>  cv2.imwrite("Images/verticle_lines.jpg",verticle_lines_img)

    //>>  # Morphological operation to detect horizontal lines from an image
    //>>  img_temp2 = cv2.erode(img_bin, hori_kernel, iterations=3)
    //>>  horizontal_lines_img = cv2.dilate(img_temp2, hori_kernel, iterations=3)
    //>>  cv2.imwrite("Images/horizontal_lines.jpg",horizontal_lines_img)

    //>>  # Weighting parameters, this will decide the quantity of an image to be added to make a new image.
    //>>  alpha = 0.5
    //>>  beta = 1.0 - alpha
    //>>  # This function helps to add two image with specific weight parameter to get a third image as summation of two image.
    //>>  img_final_bin = cv2.addWeighted(verticle_lines_img, alpha, horizontal_lines_img, beta, 0.0)
    //>>  img_final_bin = cv2.erode(~img_final_bin, kernel, iterations=2)
    //>>  (thresh, img_final_bin) = cv2.threshold(img_final_bin, 128, 255, cv2.THRESH_BINARY | cv2.THRESH_OTSU)

    //>>  # For Debugging
    //>>  # Enable this line to see verticle and horizontal lines in the image which is used to find boxes
    //>>  print("Binary image which only contains boxes: Images/img_final_bin.jpg")
    //>>  cv2.imwrite("Images/img_final_bin.jpg",img_final_bin)
    //>>  # Find contours for image, which will detect all the boxes
    //>>  contours, hierarchy = cv2.findContours(
    //>>      img_final_bin, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
    //>>  # Sort all the contours by top to bottom.
    //>>  (contours, boundingBoxes) = sort_contours(contours, method="top-to-bottom")

    //>>  print("Output stored in Output directiory!")

    //>>  idx = 0
    //>>  for c in contours:
    //>>      # Returns the location and width,height for every contour
    //>>      x, y, w, h = cv2.boundingRect(c)

    //>>      # If the box height is greater then 20, widht is >80, then only save it as a box in "cropped/" folder.
    //>>      if (w > 80 and h > 20) and w > 3*h:
    //>>          idx += 1
    //>>          new_img = img[y:y+h, x:x+w]
    //>>          cv2.imwrite(cropped_dir_path+str(idx) + '.png', new_img)

    //>>  # For Debugging
    //>>  # Enable this line to see all contours.
    //>>  # cv2.drawContours(img, contours, -1, (0, 0, 255), 3)

  }
}

/** Helper methods for performing histogram and look-up table operations, correspond to part of C++ class
  * Histogram1D in the OpenCV Cookbook sample code.
  */
object Histogram1D {

  /** Apply a look-up table to an image.
    * It is a wrapper for OpenCV function `LUT`.
    *
    * @param image  input image
    * @param lookup look-up table
    * @return new image
    */
  def applyLookUp(image: Mat, lookup: Mat): Mat = {
    val dest = new Mat()
    LUT(image, lookup, dest)
    dest
  }

  /** Equalize histogram of an image. The algorithm normalizes the brightness and increases the contrast of the image.
    * It is a wrapper for OpenCV function `equalizeHist`.
    *
    * @param src input image
    * @return new image
    */
  def equalize(src: Mat): Mat = {
    val dest = new Mat()
    equalizeHist(src, dest)
    dest
  }

}

/** Helper class that simplifies usage of OpenCV `calcHist` function for single channel images.
  *
  * See OpenCV [[http://opencv.itseez.com/modules/imgproc/doc/histograms.html?highlight=histogram]]
  * documentation to learn backend details.
  */
class Histogram1D {

  private val numberOfBins         = 256
  private val channels: IntPointer = wrapInIntPointer(0)
  private var _minRange            = 0.0f
  private var _maxRange            = 255.0f

  def setRanges(minRange: Float, maxRange: Float): Unit = {
    _minRange = minRange
    _maxRange = maxRange
  }

  def getHistogramImage(image: Mat): BufferedImage = {

    // Output image size
    val width  = numberOfBins
    val height = numberOfBins

    val hist = getHistogramAsArray(image)
    // Set highest point to 90% of the number of bins
    val scale = 0.9 / hist.max * height

    // Create a color image to draw on
    val canvas = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g      = canvas.createGraphics()

    // Paint background
    g.setPaint(Color.WHITE)
    g.fillRect(0, 0, width, height)

    // Draw a vertical line for each bin
    g.setPaint(Color.BLUE)
    for (bin <- 0 until numberOfBins) {
      def h = math.round(hist(bin) * scale).toInt
      g.drawLine(bin, height - 1, bin, height - h - 1)
    }

    // Cleanup
    g.dispose()

    canvas
  }

  /** Computes histogram of an image.
    *
    * @param image input image
    * @return histogram represented as an array
    */
  def getHistogramAsArray(image: Mat): Array[Float] = {
    // Create and calculate histogram object
    val hist = getHistogram(image)

    // Extract values to an array
    val dest  = new Array[Float](numberOfBins)
    val histI = hist.createIndexer().asInstanceOf[FloatRawIndexer]
    for (bin <- 0 until numberOfBins) {
      dest(bin) = histI.get(bin)
    }

    dest
  }

  /** Computes histogram of an image.
    *
    * @param image input image
    * @param mask  optional mask
    * @return OpenCV histogram object
    */
  def getHistogram(image: Mat, mask: Mat = new Mat()): Mat = {
    val histSize = wrapInIntPointer(numberOfBins)
    val ranges   = new FloatPointer(_minRange, _maxRange)
    // Compute histogram
    val hist = new Mat()
    calcHist(wrapInMatVector(image), channels, mask, hist, histSize, ranges)
    hist
  }
}
