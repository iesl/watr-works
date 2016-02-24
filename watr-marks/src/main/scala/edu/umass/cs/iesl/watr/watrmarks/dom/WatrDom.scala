package edu.umass.cs.iesl.watr
package watrmarks
package dom


import scalaz.{Show, TreeLoc, Tree}
import org.apache.commons.lang3.StringEscapeUtils.escapeXml11

sealed trait WatrElement

case class WatrDom(
  tree: Tree[WatrElement]
) {

  def prettyPrint: String = tree.drawTree

  def toDomCursor = DomCursor(tree.loc)

  // def toCursor(l: BioLabel):Option[LatticeCursor] = for {
  //   tspanCursor <- toDomCursor.nextTSpan
  //   bioCursor <- BioLattice.initFromDom(this).initLatticeCursor(l)
  // } yield bioCursor


  def toSvg(): String = {
    // import scala.collection.mutable.Stack
    // val stack = Stack[String]()

    def transformString(e: Transformable): String = {
      val tstr = e.transforms.map({
        t => t match {
          case m:Matrix => List(m.i11, m.i12, m.i21, m.i22, m.i31, m.i32).mkString("matrix(", " ", ")")
          case m:Scale => List(m.m0, m.m1).mkString("scale(", " ", ")")
          case m:Translate => List(m.m0, m.m1).mkString("translate(", " ", ")")
        }
      }).mkString(" ")

      s"""transform=\"${tstr}\""""
    }


    def _toSvg(eloc: TreeLoc[WatrElement]): String = {
      val ch = eloc.firstChild.map(_toSvg(_))
      val rt = eloc.right.map(_toSvg(_))

      eloc.getLabel match {
        case e: WatrDom =>
          ""
        case e: Document  =>
          s"""|<?xml version="1.0" encoding="UTF-8"?>
              |${ch.getOrElse("")}
              |${rt.getOrElse("")}
              |""".stripMargin

        case e: Svg  =>
          val vb = s"${e.viewBox.x} ${e.viewBox.y} ${e.viewBox.width} ${e.viewBox.height}"
          s"""|<svg:svg
              |  xmlns:svg="http://www.w3.org/2000/svg"
              |  xmlns:xlink="http://www.w3.org/1999/xlink"
              |  version="1.1"
              |  width="${e.width}"
              |  height="${e.height}"
              |  viewBox="$vb"
              |  ${transformString(e)}
              |>
              |${ch.getOrElse("")}
              |</svg:svg>
              |${rt.getOrElse("")}
              |""".stripMargin

        case e: Desc  =>
          s"""|<svg:desc>
              |${ch.getOrElse("")}
              |</svg:desc>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: Grp  =>
          s"""|<svg:g
              |  ${transformString(e)}
              |>
              |${ch.getOrElse("")}
              |</svg:g>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: Defs  =>
          s"""|<svg:defs>
              |${ch.getOrElse("")}
              |</svg:defs>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: Text  =>
          s"""|<svg:text
              |  ${transformString(e)}
              |>
              |  ${ch.getOrElse("")}
              |</svg:text>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: Path  =>
          s"""|<svg:path>
              |  ${transformString(e)}
              |>
              |${ch.getOrElse("")}
              |</svg:path>
              |${rt.getOrElse("")}
              |""".stripMargin

        // case e: TSpanAttribs  =>
        //   val xstr = e.xs.mkString(" ")
        //   val ystr = e.ys.mkString(" ")
        //   // val endxstr = e.textXYOffsets.map(o => s"""endX=\"${o.endX}\"""").getOrElse("")
        //   val text = e.chars.mkString
        //   val esc = escapeXml11(text)

          // s"""|<svg:tspan
          //     |  x="$xstr"
          //     |  y="$ystr"
          //     |  font-size="1px"
          //     |  font-family="Helvetica"
          //     |>${esc}</svg:tspan>
          //     |${rt.getOrElse("")}
          //     |""".stripMargin

        case e: TSpan  =>
          val xstr = e.textXYOffsets.map(o => "x="+o.xs.mkString(" ")).getOrElse("")
          val ystr = e.textXYOffsets.map(o => "y="+o.ys.mkString(" ")).getOrElse("")
          val endxstr = e.textXYOffsets.map(o => s"""endX=\"${o.endX}\"""").getOrElse("")

          s"""|<svg:tspan
              |  $xstr
              |  $ystr
              |  $endxstr
              |  font-size="${e.fontSize}"
              |  font-family="${e.fontFamily}"
              |>${e.text}</svg:tspan>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e  =>
          ""
      }

    }

    _toSvg(tree.loc)
  }

}

sealed trait Transformable {
  def transforms: List[Transform]
}

case class Document (
  labelDictionary: BioLabelDictionary
) extends WatrElement  {
  override def toString = """<doc:>"""
}

case class ViewBox(x: Double, y: Double, width: Double, height: Double)

case class Svg (
  width: Double, height: Double, viewBox: ViewBox,
  transforms: List[Transform] = List()
) extends WatrElement with Transformable {
  override def toString = s"""<svg:${transforms}>"""
}

case class Desc (
) extends WatrElement {
  override def toString = s"""<desc:>"""
}

case class Grp (
  transforms: List[Transform] = List()
) extends WatrElement with Transformable {
  override def toString = s"""<g:${transforms.mkString("{", ", ", "}")}>"""
}

case class Defs (
) extends WatrElement {
  override def toString = s"""<defs:>"""
}

case class Text (
  transforms: List[Transform] = List()
) extends WatrElement  with Transformable  {
  override def toString = s"""<text:${transforms}>"""
}

case class Path (
  transforms: List[Transform] = List()
) extends WatrElement   with Transformable {
  override def toString = s"""<path:>"""
}

object NullElement extends WatrElement   {
  override def toString = s"""<null-elem>"""
}


case class TextXYOffsets(
  xs: List[Double], endX: Double,
  ys: List[Double]
)


// helper class for deserializing Dom - not to be used directly
case class TSpanInit (
  text: String,
  transforms: List[Transform],
  textXYOffsets: Option[TextXYOffsets],
  fontSize: String,
  fontFamily: String
) extends WatrElement

case class TSpan (
  text: String,
  textXYOffsets: Option[TextXYOffsets],
  fontSize: String,
  fontFamily: String,
  document: Document
) extends WatrElement  {

  override def toString = s"""<tspan:${text}>"""
}

