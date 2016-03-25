package edu.umass.cs.iesl.watr
package watrmarks

import javax.xml.namespace._
import javax.xml.stream.events._

object JavaxXmlUtils {
  implicit class RicherString(val s: String) extends AnyVal {
    def qname: QName = new QName(s)


  }

  implicit class RicherAttribute(val attr: Attribute) extends AnyVal {
    def toDouble = attr.getValue.toDouble
    def toFloat = attr.getValue.toFloat
  }


  implicit class RicherStartElement(val elem: StartElement) extends AnyVal {

    // def getAttrValue(s: String): Attribute = {
    //   elem.getAttributeByName("class".qname)
    // }

    def getAttr(s: String): Attribute = {
      elem.getAttributeByName("class".qname)
    }

    def getClassAttr = getAttr("class").getValue
    def getIdAttr = getAttr("id").getValue
    def getTargetAttr = getAttr("target").getValue
  }
}
