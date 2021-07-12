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
import org.bytedeco.opencv.opencv_imgproc.Vec2fVector
// import org.opencv.core.Core

// import scala.Ordering.Float.IeeeOrdering

class LineDetectionTest extends utils.SegmentationTestUtils {
  behavior of "Image-based Line Detection"

  it should "run" in {
    val imgPath = "./corpus.d/cmp-lg9503025.pdf.d/page-images/page-2.opt.png"


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
      // ImgProc.THRESH_BINARY | ImgProc.THRESH_OTSU
      ImgProc.THRESH_BINARY_INV
    )

    //>>  img_bin = 255-img_bin  # Invert the image
    // Create inverted lookup table

    show(thresholdOut, "inital image")
    System.in.read()
    // val dim  = 256
    // val lut  = new Mat(1, dim, CV_8U)
    // val lutI = lut.createIndexer().asInstanceOf[UByteIndexer]
    // for (i <- 0 until dim) {
    //   lutI.put(i, (dim - 1 - i).toByte)
    // }

    // // Apply look-up
    // val dest = Histogram1D.applyLookUp(thresholdOut, lut)

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

    //
  }

  it should "use hough detection" in {


    import math.{
      Pi, cos, round, sin
    }
    val imgPath = "./corpus.d/cmp-lg9503025.pdf.d/page-images/page-2.opt.png"
    // Read input image
    // val src = loadAndShowOrExit(new File("data/road.jpg"), IMREAD_GRAYSCALE)
    val src = imread(imgPath, IMREAD_GRAYSCALE)

    // Canny contours
    val canny        = new Mat()
    val threshold1   = 125
    val threshold2   = 350
    val apertureSize = 3
    Canny(src, canny, threshold1, threshold2, apertureSize, false /*L2 gradient*/ )
    show(canny, "Canny Contours")

    // Hough transform for line detection
    val lines                      = new Vec2fVector()
    val storage                    = cvCreateMemStorage(0)
    val method                     = HOUGH_STANDARD
    val distanceResolutionInPixels = 1
    val angleResolutionInRadians   = math.Pi / 180
    val minimumVotes               = 80
    val srn                        = 0.0
    val stn                        = 0.0
    val min_theta                  = 0.0
    val max_theta                  = CV_PI
    HoughLines(canny, lines, distanceResolutionInPixels, angleResolutionInRadians, minimumVotes, srn, stn, min_theta, max_theta)

    // Draw lines on the canny contour image
    val result = new Mat()
    src.copyTo(result)
    cvtColor(src, result, COLOR_GRAY2BGR)
    for (i <- 0 until lines.size().toInt) {
      val rho   = lines.get(i).get(0)
      val theta = lines.get(i).get(1)

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
      line(result, pt1, pt2, new Scalar(0, 0, 255, 0), 1, LINE_8, 0)
    }

    show(result, "Hough Lines")
    // show(OpenCVUtils.toMat8U(result), "Hough Lines")
    System.in.read()
  }

}
