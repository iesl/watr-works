// package edu.umass.cs.iesl.watr
// package watrmarks
// package dom

// object IO {

//   import scala.collection.JavaConverters._
//   import java.io.Reader
//   import org.jdom2
//   import jdom2._
//   // import jdom2.filter._

//   def maybeAttrValue(e: Element, s:String): Option[String] = {
//     val maybeAttr = e.getAttribute(s)
//     if (maybeAttr!=null) Some(maybeAttr.getValue())
//     else None
//   }

//   def attrValue(e: Element, s:String): String = {
//     e.getAttribute(s).getValue()
//   }

//   def getTransforms(e: Element): List[Transform] = {
//     transformParser.parse(maybeAttrValue(e, "transform").getOrElse(""))
//       .left.map(err => sys.error(s"parsing transform='${attrValue(e, "transform")}': ${err}"))
//       .right.get
//   }
//   def getXs(e: Element): List[Double] = { maybeAttrValue(e, "x").map(_.split(" ").map(_.toDouble).toList).getOrElse(List()) }
//   def getEndX(e: Element): Option[Double]     = { maybeAttrValue(e, "endX").map(_.toDouble) }
//   def getY(e: Element): Double                = { attrValue(e, "y").toDouble }
//   def getFontSize(e: Element): String         = { maybeAttrValue(e, "font-size").getOrElse("0") }
//   def getFontFamily(e: Element): String       = { maybeAttrValue(e, "font-family").getOrElse("") }

//   def getSelfAndAncestorTransforms(elem: Element): List[Transform] = {
//     if (elem.isRootElement()) {
//       getTransforms(elem)
//     } else {
//       getSelfAndAncestorTransforms(elem.getParentElement) ++ getTransforms(elem)
//     }
//   }

//   def readTSpans(ins: Reader): Seq[TSpanX] = {
//     println("reading...")
//     val document = new jdom2.input.SAXBuilder().build(ins)

//     // val f = filter.Filters.element("tspan")
//     // val elements  = document.getDescendants(f).iterator.asScala
//     val elements  = document.getDescendants().iterator.asScala

//     val tspans = for {
//       e <- elements if e.isInstanceOf[jdom2.Element]
//       elem = e.asInstanceOf[jdom2.Element]
//       if elem.getName.toLowerCase == "tspan"
//     } yield {

//       val offs = TextXYOffsets(
//         xs=getXs(elem),
//         ys=List(getY(elem))
//       )
//       val ts = TSpanX(
//         elem.getText,
//         offs,
//         getSelfAndAncestorTransforms(elem),
//         getFontSize(elem),
//         getFontFamily(elem)
//       )

//       ts
//     }

//     tspans.toSeq
//   }

// }

// import scalaz.Tree
// import scalaz.TreeLoc
// // import org.apache.commons.lang3.StringEscapeUtils.escapeXml11

// sealed trait WatrElement

// case class WatrDom(
//   tree: Tree[WatrElement],
//   annotations: Seq[Annotation] = List()
// ) {

//   def prettyPrint: String = tree.drawTree

//   def toDomCursor = DomCursor(tree.loc)

//   // def toCursor(l: Label):Option[LatticeCursor] = for {
//   //   tspanCursor <- toDomCursor.nextTSpan
//   //   bioCursor <- BioLattice.initFromDom(this).initLatticeCursor(l)
//   // } yield bioCursor


//   def toSvg(): String = {
//     // import scala.collection.mutable.Stack
//     // val stack = Stack[String]()

//     def transformString(e: Transformable): String = {
//       val tstr = e.transforms.map({
//         t => t match {
//           case m:Matrix => List(m.i11, m.i12, m.i21, m.i22, m.i31, m.i32).mkString("matrix(", " ", ")")
//           case m:Scale => List(m.m0, m.m1).mkString("scale(", " ", ")")
//           case m:Translate => List(m.m0, m.m1).mkString("translate(", " ", ")")
//         }
//       }).mkString(" ")

//       s"""transform=\"${tstr}\""""
//     }


//     def _toSvg(eloc: TreeLoc[WatrElement]): String = {
//       val ch = eloc.firstChild.map(_toSvg(_))
//       val rt = eloc.right.map(_toSvg(_))

//       eloc.getLabel match {
//         case e: WatrDom =>
//           ""
//         case e: Document  =>
//           s"""|<?xml version="1.0" encoding="UTF-8"?>
//               |${ch.getOrElse("")}
//               |${rt.getOrElse("")}
//               |""".stripMargin

//         case e: Svg  =>
//           val vb = s"${e.viewBox.x} ${e.viewBox.y} ${e.viewBox.width} ${e.viewBox.height}"
//           s"""|<svg:svg
//               |  xmlns:svg="http://www.w3.org/2000/svg"
//               |  xmlns:xlink="http://www.w3.org/1999/xlink"
//               |  version="1.1"
//               |  width="${e.width}"
//               |  height="${e.height}"
//               |  viewBox="$vb"
//               |  ${transformString(e)}
//               |>
//               |${ch.getOrElse("")}
//               |</svg:svg>
//               |${rt.getOrElse("")}
//               |""".stripMargin

//         case e: Desc  =>
//           s"""|<svg:desc>
//               |${ch.getOrElse("")}
//               |</svg:desc>
//               |${rt.getOrElse("")}
//               |""".stripMargin
//         case e: Grp  =>
//           s"""|<svg:g
//               |  ${transformString(e)}
//               |>
//               |${ch.getOrElse("")}
//               |</svg:g>
//               |${rt.getOrElse("")}
//               |""".stripMargin
//         case e: Defs  =>
//           s"""|<svg:defs>
//               |${ch.getOrElse("")}
//               |</svg:defs>
//               |${rt.getOrElse("")}
//               |""".stripMargin
//         case e: Text  =>
//           s"""|<svg:text
//               |  ${transformString(e)}
//               |>
//               |  ${ch.getOrElse("")}
//               |</svg:text>
//               |${rt.getOrElse("")}
//               |""".stripMargin
//         case e: Path  =>
//           s"""|<svg:path>
//               |  ${transformString(e)}
//               |>
//               |${ch.getOrElse("")}
//               |</svg:path>
//               |${rt.getOrElse("")}
//               |""".stripMargin

//         // case e: TSpanAttribs  =>
//         //   val xstr = e.xs.mkString(" ")
//         //   val ystr = e.ys.mkString(" ")
//         //   // val endxstr = e.textXYOffsets.map(o => s"""endX=\"${o.endX}\"""").getOrElse("")
//         //   val text = e.chars.mkString
//         //   val esc = escapeXml11(text)

//           // s"""|<svg:tspan
//           //     |  x="$xstr"
//           //     |  y="$ystr"
//           //     |  font-size="1px"
//           //     |  font-family="Helvetica"
//           //     |>${esc}</svg:tspan>
//           //     |${rt.getOrElse("")}
//           //     |""".stripMargin

//         case e: TSpan  =>
//           val xstr = e.textXYOffsets.map(o => "x="+o.xs.mkString(" ")).getOrElse("")
//           val ystr = e.textXYOffsets.map(o => "y="+o.ys.mkString(" ")).getOrElse("")
//           // val endxstr = e.textXYOffsets.map(o => s"""endX=\"${o.endX}\"""").getOrElse("")

//           // |  $endxstr

//           s"""|<svg:tspan
//               |  $xstr
//               |  $ystr
//               |  font-size="${e.fontSize}"
//               |  font-family="${e.fontFamily}"
//               |>${e.text}</svg:tspan>
//               |${rt.getOrElse("")}
//               |""".stripMargin
//         case e  =>
//           ""
//       }

//     }

//     _toSvg(tree.loc)
//   }

// }

// sealed trait Transformable {
//   def transforms: List[Transform]
// }

// case class Document (
//   labelDictionary: LabelDictionary
// ) extends WatrElement  {
//   override def toString = """<doc:>"""
// }

// case class ViewBox(x: Double, y: Double, width: Double, height: Double)

// case class Svg (
//   width: Double, height: Double, viewBox: ViewBox,
//   transforms: List[Transform] = List()
// ) extends WatrElement with Transformable {
//   override def toString = s"""<svg:${transforms}>"""
// }

// case class Desc (
// ) extends WatrElement {
//   override def toString = s"""<desc:>"""
// }

// case class Grp (
//   transforms: List[Transform] = List()
// ) extends WatrElement with Transformable {
//   override def toString = s"""<g:${transforms.mkString("{", ", ", "}")}>"""
// }

// case class Defs (
// ) extends WatrElement {
//   override def toString = s"""<defs:>"""
// }

// case class Text (
//   transforms: List[Transform] = List()
// ) extends WatrElement  with Transformable  {
//   override def toString = s"""<text:${transforms}>"""
// }

// case class Path (
//   transforms: List[Transform] = List()
// ) extends WatrElement with Transformable {
//   override def toString = s"""<path:>"""
// }

// case class Annotation(
//   id: String,
//   labelName: Label,
//   bboxes: Seq[Rect]
// )

// case class Rect(
//   x: Double, y: Double, width: Double, height: Double
// ) extends WatrElement {
//   override def toString = s"""<rect:>"""
// }

// object NullElement extends WatrElement   {
//   override def toString = s"""<null-elem>"""
// }

// case class TextXYOffsets(
//   xs: List[Double], //endX: Double,
//   ys: List[Double]
// )


// // helper class for deserializing Dom - not to be used directly
// case class TSpanInit (
//   text: String,
//   transforms: List[Transform],
//   textXYOffsets: Option[TextXYOffsets],
//   fontSize: String,
//   fontFamily: String
// ) extends WatrElement

// case class TSpan (
//   text: String,
//   textXYOffsets: Option[TextXYOffsets],
//   fontSize: String,
//   fontFamily: String,
//   document: Document
// ) extends WatrElement  {

//   override def toString = s"""<tspan:${text}>"""
// }

// case class TSpanX (
//   text: String,
//   textXYOffsets: TextXYOffsets,
//   transforms: List[Transform],
//   fontSize: String,
//   fontFamily: String
// ) extends WatrElement  {

//   override def toString = s"""<tspan:${text}>"""
// }
