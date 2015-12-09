package edu.umass.cs.iesl.watr
package shell


// import org.jdom.output.Format
import java.io.Reader
import javax.xml.stream.XMLInputFactory
import org.jdom2.Document
// import org.jdom2.input.SAXBuilder

// sealed trait WElement {}
// trait WContainer extends WElement
// case class WDocument() extends WElement
// case class WSvg() extends WElement
// case class WDesc() extends WElement
// case class WGrp() extends WElement
// case class WDefs() extends WElement
// case class WText() extends WElement
// case class WChars() extends WElement
// case class WTSpan() extends WElement

// case class WElementCursor()
// case class WCharsCursor()

import java.io.FileInputStream
import java.io.InputStream
import java.io.InputStreamReader
import java.io.StringReader
import org.jdom2.input.SAXBuilder
import scala.collection.mutable
import javax.xml.stream.events._;

object wdocuments {


  def readWDocument(ins: Reader): WDocument = {
    val factory = XMLInputFactory.newInstance();
    // val insreader = new java.io.InputStreamReader(ins)
    val reader = factory.createXMLEventReader(ins);


    val stack = mutable.Stack[WElement]()

    while (reader.hasNext()) {
      val event = reader.nextEvent();

      event match {
        case elem: Namespace =>
        case attribute: Attribute =>
          val name = attribute.getName();
          val value = attribute.getValue();
          println("Attribute name/value: " + name + "/" + value);
        case elem: StartDocument =>
          stack push WDocument()

        case elem: EndDocument =>

        case elem: StartElement =>
          // println(s"Start Element: ${elem.getName}, prefix: ${elem.getName().getPrefix()}, local: ${elem.getName().getLocalPart()}")
          elem.getName.getLocalPart match {
            case "svg" => stack push WSvg()
            case "g" => stack push WGrp()
            case "defs" => stack push WDefs()
            case "text" => stack push WText()
            case "tspan" => stack push WTSpan()
          }

        case elem: EndElement =>
          val t = stack.pop()
          val u = stack.pop()
          stack.push(u addChild t)
        case elem: EntityReference =>
        case elem: Characters =>
          if (stack.top.isInstanceOf[WTSpan]) {
            val t = stack.pop

            stack.push(
              WTSpan(WChars(
                elem.getData()
              )))

          }
      }
    }
    stack.top.asInstanceOf[WDocument]
  }


}

sealed trait WElement {
  def children: Seq[WElement]
  def addChild(c: WElement): WContainer
}

trait WContainer extends WElement {

}

case class WDocument(
  children: Seq[WElement] = Seq()
) extends WContainer {

  def addChild(c: WElement): WContainer =
    this.copy(children = children ++ Seq(c))

  def toCursor: WElementCursor = {
    WElementCursor(this,
      List(), List(), List(), children
    )
  }
}

case class WElementCursor(
  current: WElement,
  path: Seq[WElement],
  prevs: Seq[WElement],
  nexts: Seq[WElement],
  children: Seq[WElement]
) {

}


case class WCharsCursor(
  current: Char,
  prev: List[Char],
  next: List[Char]
) {

}

case class WSvg(
  children: Seq[WElement] = Seq()
) extends WContainer {
  def addChild(c: WElement): WContainer =
    this.copy(children = children ++ Seq(c))

}

case class WDesc(
) extends WElement {
  def children: Seq[WElement]=
    sys.error("operation undefined")

  def addChild(c: WElement): WContainer =
    sys.error("operation undefined")
}

case class WGrp(
  children: Seq[WElement] = Seq()
) extends WContainer {
  def addChild(c: WElement): WContainer =
    this.copy(children = children ++ Seq(c))
}

case class WDefs(
  children: Seq[WElement] = Seq()
) extends WContainer {
  def addChild(c: WElement): WContainer =
    this.copy(children = children ++ Seq(c))

}

case class WText(
  children: Seq[WElement] = Seq()
) extends WContainer {
  def addChild(c: WElement): WContainer =
    this.copy(children = children ++ Seq(c))

}

case class WChars(
  chars: Seq[Char] = Seq()
) extends WElement {

  def children: Seq[WElement]=
    sys.error("operation undefined")

  def addChild(c: WElement): WContainer =
    sys.error("operation undefined")

}

case class WTSpan(
  chars: WChars = WChars()
) extends WElement {
  def children: Seq[WElement]=
    sys.error("operation undefined")

  def addChild(c: WElement): WContainer =
    sys.error("operation undefined")
}

class WCursor(
  elemCurrent: WElementCursor,
  charCurrent: WCharsCursor
)
