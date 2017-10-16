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

// Font properties:
//   name/type
//   unique id within document (e.g, 104 0 R)
//   # of glyphs represented (per page, per document)
//   is natural language??
//   is symbolic??

object ExtractedItem {
  // implicit class RicherExtractedItem(val self: CharItem) extends AnyVal {}

  case class CharItem(
    id: Int@@CharID,
    bbox: LTBounds,
    fontBbox: LTBounds,
    char: String
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
  ) extends ExtractedItem {

    lazy val pp = waypoints.map(_.prettyPrint).take(4).mkString(", ")
    def strRepr(): String = s"[path ${bbox.prettyPrint}=[$pp]]"
  }


}



class CharProps()  {
  var charRunId: Int = -1
  var isRunBegin: Boolean = false
}
