package edu.umass.cs.iesl.watr
package watrmarks


import java.io.Reader
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events._;


sealed trait WatrElement

import scalaz._
// import Scalaz._


case class WatrDomCursor(
  loc: TreeLoc[WatrElement]
) {


  def root: WatrDomCursor                     = WatrDomCursor(loc.root)
  def parent: Option[WatrDomCursor]           = loc.parent map {p => WatrDomCursor(p) }
  def left: Option[WatrDomCursor]             = loc.left map {p => WatrDomCursor(p) }
  def right: Option[WatrDomCursor]            = loc.right map {p => WatrDomCursor(p) }
  def firstChild: Option[WatrDomCursor]       = loc.firstChild map {p => WatrDomCursor(p) }
  def lastChild: Option[WatrDomCursor]        = loc.lastChild map {p => WatrDomCursor(p) }
  def getChild(n: Int): Option[WatrDomCursor] = loc.getChild(n) map {p => WatrDomCursor(p) }

  // def findChild(p: Tree[A] =
  // def split(acc: TreeForest[A], xs: TreeForest[A]): Option[(TreeForest[A], Tree[A], TreeForest[A])] =
  // def find(p: WatrDomCursor =
  // def toTree: Tree[A] =
  // def toForest: TreeForest[A] =
  // def isRoot: Boolean =
  // def isFirst: Boolean =
  // def isLast: Boolean =
  // def isLeaf: Boolean =
  // def isChild: Boolean =
  // def hasChildren: Boolean =

}

case class WatrDom(
  tree: Tree[WatrElement]
) {
  implicit val s = watrdom.showElementTree

  def prettyPrint: String = tree.drawTree

  def toCursor = WatrDomCursor(tree.loc)

}

class Document extends WatrElement{
  override def toString = s"""<doc:>"""

}
class Svg      extends WatrElement{
  override def toString = s"""<svg:>"""

}
class Desc     extends WatrElement{
  override def toString = s"""<desc:>"""

}
class Grp      extends WatrElement{
  override def toString = s"""<g:>"""
}
class Defs     extends WatrElement{
  override def toString = s"""<defs:>"""

}
class Text     extends WatrElement {
  override def toString = s"""<text:>"""

}
class Path     extends WatrElement {
  override def toString = s"""<path:>"""

}
class Chars(chs: String)    extends WatrElement {
  override def toString = s"""<ch:${chs}>"""
}
class TSpan    extends WatrElement {
  override def toString = s"""<tspan:>"""

}

object watrdom {

  implicit val showElementTree = Show.shows[WatrElement](_.toString())

  def document(): WatrElement = new Document
  def svg(): WatrElement      = new Svg
  def desc(): WatrElement     = new Desc
  def grp(): WatrElement      = new Grp
  def defs(): WatrElement     = new Defs
  def text(): WatrElement     = new Text
  def path(): WatrElement     = new Path
  def chars(cs: String): WatrElement    = new Chars(cs)
  def tspan(): WatrElement    = new TSpan

  def read(ins: Reader): WatrDom = {
    val factory = XMLInputFactory.newInstance();
    val reader = factory.createXMLEventReader(ins);

    var accum: TreeLoc[WatrElement] = null

    while (reader.hasNext()) {
      val event = reader.nextEvent();

      event match {
        case elem: Namespace =>
          // println(s"Namespace: ${elem}")
        case attribute: Attribute =>
          // println(s"Attribute: ${attribute}")
          val name = attribute.getName();
          val value = attribute.getValue();
          // println("Attribute name/value: " + name + "/" + value);
        case elem: StartDocument =>
          accum = Tree.Leaf(document()).loc
          // println(s"StartDocument: ${elem}")

        case elem: EndDocument =>
          // println(s"EndDocument: ${elem}")

        case elem: StartElement =>
          // println(s"StartElement: ${elem}")
          // println(s"Start Element: ${elem.getName}, prefix: ${elem.getName().getPrefix()}, local: ${elem.getName().getLocalPart()}")
          elem.getName.getLocalPart match {
            case "svg"   => accum = accum.insertDownLast(Tree.Leaf(svg()))
            case "g"     => accum = accum.insertDownLast(Tree.Leaf(grp()))
            case "defs"  => accum = accum.insertDownLast(Tree.Leaf(defs()))
            case "text"  => accum = accum.insertDownLast(Tree.Leaf(text()))
            case "tspan" => accum = accum.insertDownLast(Tree.Leaf(tspan()))
          }

        case elem: EndElement =>
          // println(s"EndElement: ${elem}")
          accum = accum.parent.get
        case elem: EntityReference =>
          // println(s"EntityReference: ${elem}")
        case elem: Characters =>
            // println(s"Characters: '${elem.getData().trim()}'")

          accum.getLabel match {
            case t: TSpan =>
              accum = accum.insertDownLast(Tree.Leaf(
                chars(elem.getData())
              )).parent.get
            case _ =>
              // sys.error(s"xml text found outside of tspan element: '${elem.getData()}'")
          }
      }

      // println(s"""accu: ${if(accum!=null) accum.getLabel else "<null>"} """)
    }

    WatrDom(accum.toTree)
  }

}
