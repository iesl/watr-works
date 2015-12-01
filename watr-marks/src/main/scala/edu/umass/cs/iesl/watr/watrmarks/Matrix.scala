package edu.umass.cs.iesl.watr.watrmarks

import org.jdom2.Content
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

object SvgMatrix {

  /** svg matrix
    *
    * it is defined at http://www.w3.org/TR/SVG/coords.html#EstablishingANewUserSpace
    * and at https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
    */
  type SvgMatrix = Array[Double]

  /** Function to multiply two svg matrices **/
  def svgMatrixMultiply(m1: SvgMatrix, m2: SvgMatrix): SvgMatrix = {
    require(m1.size == 6 && m2.size == 6, "one or more SvgMatrix has invalid size instead of size of 6")

    val _0 = m1(0) * m2(0) + m1(2) * m2(1)
    val _1 = m1(1) * m2(0) + m1(3) * m2(1)
    val _2 = m1(0) * m2(2) + m1(2) * m2(3)
    val _3 = m1(1) * m2(2) + m1(3) * m2(3)
    val _4 = m1(0) * m2(4) + m1(2) * m2(5) + m1(4)
    val _5 = m1(1) * m2(4) + m1(3) * m2(5) + m1(5)
    Array(_0, _1, _2, _3, _4, _5)
  }

  /** Function to extract svg matrix from element's transform attribute **/
  def svgMatrix(e: Element): SvgMatrix = {

    val identity = Array(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
    def translate2Matrix(array: Array[Double]) = Array(1.0, 0.0, 0.0, 1.0, array(0), array(1))
    def scale2Matrix(array: Array[Double]) = Array(array(0), 0.0, 0.0, array(1), 0.0, 0.0)

    Option(e.getAttribute("transform")) match {
      case Some(attr) if !attr.getValue().isEmpty  =>

        val ms = attr.getValue().trim().split("((?<=\\))[\\s,]+)").map(str => {
          val firstN = str.indexOf("(") + 1
          val lastN = str.size - str.indexOf(")")
          val doubleArray = str.drop(firstN).dropRight(lastN).split("[\\s,]+").map(_.toDouble)
          if (str.startsWith("matrix(")) {
            assert(doubleArray.size == 6, "svg matrix has invalid size")
            doubleArray
          } else if (str.startsWith("translate(")) {
            assert(doubleArray.size == 2, "svg translate has invalid size")
            translate2Matrix(doubleArray)
          } else if (str.startsWith("scale(")) {
            assert(doubleArray.size == 2, "svg scale has invalid size")
            scale2Matrix(doubleArray)
          } else {
            assert(false, "could not parse: " + e.getAttribute("transform"))
            identity
          }
        })

        ms.foldLeft(identity) {
          case (mAcc, m) => svgMatrixMultiply(mAcc, m)
        }
      case _ =>
        identity
    }
  }

}
