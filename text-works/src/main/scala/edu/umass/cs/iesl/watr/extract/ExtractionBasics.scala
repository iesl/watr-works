package edu.umass.cs.iesl.watr
package extract


// import utils.EnrichNumerics._
// import utils.ExactFloats._
import geometry._
import geometry.syntax._
import utils.{RelativeDirection => Dir}

sealed trait ExtractedItem {
  val charProps: CharProps = new CharProps {}

  def id: Int@@CharID
  def bbox: LTBounds

  lazy val location: Point = bbox.toPoint(Dir.BottomLeft)

  def strRepr(): String

}

object ExtractedItem {
  implicit class RicherExtractedItem(val self: CharItem) extends AnyVal {


    // def isWonky: Boolean = self.wonkyCharCode.isDefined

    // def isSpace: Boolean = self.wonkyCharCode.exists(_==32)
    // def isControl: Boolean = self.wonkyCharCode.exists(_<32)
    // def isNonPrintable: Boolean = self.wonkyCharCode.exists(_<=32)

  }

  case class CharItem(
    id: Int@@CharID,
    bbox: LTBounds,
    fontBbox: LTBounds,
    char: String
    // wonkyCharCode: Option[Int] = None
  ) extends ExtractedItem {
    def strRepr(): String = char
  }


  case class ImgItem(
    id: Int@@CharID,
    bbox: LTBounds
  ) extends ExtractedItem {
    def strRepr(): String = s"[image ${bbox.prettyPrint}]"
  }

  case class PathItem(
    id: Int@@CharID,
    bbox: LTBounds,
    waypoints: Seq[Point]
    // waypoints: Seq[Seq[Seq[Point]]]
  ) extends ExtractedItem {

    lazy val pp = waypoints.map(_.prettyPrint).take(4).mkString(", ")
    def strRepr(): String = s"[path ${bbox.prettyPrint}=[$pp]]"
  }


}



class CharProps()  {
  var charRunId: Int = -1
  var isRunBegin: Boolean = false
}

