package edu.umass.cs.iesl.watr
package segment

import org.scalatest._
// import watrmarks._
import java.io.InputStream
import scalaz.@@
import spindex._
import TypeTags._

trait DocsegTestUtil extends  FlatSpec with Matchers with DocumentUtils {


  val LB = watrmarks.StandardLabels

  object page {
    val page = (0 to 10).map(PageID(_))
    def apply(i:Int) = page(i)
  }

  def wsv(s: String) = s.split(" ").map(_.trim).toList


  def svgCutAndPasteToTest(svgstr: String): ParsedExample = {
    val lines = svgstr.split("\n").toList
      .map(_.trim)

    val (pageLines, nexts) = lines
      .span(!_.startsWith("---"))

    val (boundsLines, expectedOutputLines) = nexts.drop(1)
      .span(!_.startsWith("===="))

    val fileAndPageLine = pageLines
      .filter(_.startsWith("page=")).head

    val page = fileAndPageLine
      .dropWhile(!_.isDigit).takeWhile(_.isDigit).mkString
      .toInt

    val file = fileAndPageLine
      .split("/").last.dropRight(1)

    def getAttrVal(attr: String, text:String): String = {
      val index = text.indexOf(s""" ${attr}=""")
      text.substring(index)
        .dropWhile(_ != '"')
        .drop(1)
        .takeWhile(_ != '"')
        .mkString
    }

    def attrsToBounds(text: String): LTBounds = {
      LTBounds(
        getAttrVal("x", text).toDouble,
        getAttrVal("y", text).toDouble,
        getAttrVal("width", text).toDouble,
        getAttrVal("height", text).toDouble
      )
    }

    val parsedFrags = boundsLines
      .map({line =>
        val lineBounds = attrsToBounds(line)
        val foundText = line.split("-->").head.trim

        (foundText, PageID(page), lineBounds)
      })

    ParsedExample(
      file,
      parsedFrags,
      expectedOutputLines.drop(1)
    )
  }

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
      cAndp.expectedOutput.trim.split("\n").map(_.trim)
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
  expectedOutput: Seq[String]
)
