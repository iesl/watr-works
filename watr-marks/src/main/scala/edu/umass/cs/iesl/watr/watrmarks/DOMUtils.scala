package edu.umass.cs.iesl.watr.watrmarks


import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.filter.ElementFilter
import org.jdom2.util.IteratorIterable
import scala.collection.JavaConversions.iterableAsScalaIterable


object DOMUtils {

  /** Function to get all non-empty tspan elements **/
  def getTSpanElements(dom: Document): Iterable[Element] = {
    dom.getRootElement().getDescendants(new ElementFilter("tspan")).toIterable.filter(e => {
      e.getText().size > 0
    })
  }

  def getTspanDescendants(e: Element) = {
    e.getDescendants(new ElementFilter("tspan")).toIterable.toList
  }

  /** Function to get the most recent common ancestor of two elements **/
  def commonAncestor(e1: Element, e2: Element): Element = {
    require(e1 != null && e2 != null, "one of the elements has invalid null value")
    if (e1 == e2) {
      e1
    } else if (e1.isAncestor(e2)) {
      e1
    } else if (e2.isAncestor(e1)) {
      e2
    } else {
      commonAncestor(e1.getParentElement(), e2.getParentElement())
    }
  }
}

object Attr {

  /** Function to get element's font size **/
  def fontSize(e: Element): Double = {
    val raw = e.getAttribute("font-size").getValue()
    if (raw.endsWith("px")) {
      raw.dropRight(2).toDouble
    } else raw.toDouble
  }

  /** Function to get element's y position **/
  def y(e: Element): Double = {
    e.getAttribute("y").getValue().toDouble
  }

  /** Function to get element's x positions **/
  def xs(e: Element): Array[Double] = {
    e.getAttribute("x").getValue().split(" ").map(_.toDouble)
  }

  /** Function to get element's last character's right edge position **/
  def endX(e: Element): Double = {
    e.getAttribute("endX").getValue().toDouble
  }


}
