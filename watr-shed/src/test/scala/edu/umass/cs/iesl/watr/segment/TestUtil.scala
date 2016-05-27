package edu.umass.cs.iesl
package watr
package segment

import org.scalatest._
import watrmarks._
import java.io.InputStream
import scalaz.@@

trait DocsegTestUtil extends  FlatSpec with Matchers with DocstrumUtils {

  type @@[A, B] = scalaz.@@[A, B]

  // val papers = papers

  val LB = watrmarks.StandardLabels

  object page {
    val page = (0 to 10).map(PageID(_))
    def apply(i:Int) = page(i)
  }

  def wsv(s: String) = s.split(" ").map(_.trim).toList

  def cutAndPasteToTestExample(cAndp: TextExample): ParsedExample = {
    val sourcePdfName = cAndp.source.split("/").last.trim
    val pageNumber = cAndp.source.split(":")(1).takeWhile(_.isDigit).mkString.toInt

    val parsedFrags = cAndp.fragments.split("\n").toList.map{ frag =>
      val boundsPart = frag.reverse.trim.drop(1).takeWhile(_ != '(').reverse
      val foundText = frag.substring(0, frag.length - boundsPart.length - 2).trim

      val bounds = boundsPart
        .replaceAll("[ltwh:]", "")
        .split(", ")
        .map(_.toDouble)

      (foundText, PageID(pageNumber), LTBounds(bounds(0), bounds(1), bounds(2), bounds(3)))
    }

    ParsedExample(
      sourcePdfName,
      parsedFrags,
      cAndp.expectedOutput
    )
  }

}


case class TestRegion(
  pdf: InputStream,
  page: Int@@PageID,
  bbox: LTBounds
)

case class Example(
  region: TestRegion,
  identifyingRegex: List[String],
  skip: Boolean = false
)

case class TextExample(
  source: String,
  fragments: String,
  expectedOutput: String
)

case class ParsedExample(
  source: String,
  regions: Seq[(String, Int@@PageID, LTBounds)],
  expectedOutput: String
)
