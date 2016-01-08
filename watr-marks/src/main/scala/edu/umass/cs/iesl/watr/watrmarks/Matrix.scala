package edu.umass.cs.iesl.watr
package watrmarks

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

  /** Function to multiply two svg matrices **/
  def svgMatrixMultiply(m1: Matrix, m2: Matrix): Matrix = {
    Matrix(
      m1.m0 * m2.m0 + m1.m2 * m2.m1,
      m1.m1 * m2.m0 + m1.m3 * m2.m1,
      m1.m0 * m2.m2 + m1.m2 * m2.m3,
      m1.m1 * m2.m2 + m1.m3 * m2.m3,
      m1.m0 * m2.m4 + m1.m2 * m2.m5 + m1.m4,
      m1.m1 * m2.m4 + m1.m3 * m2.m5 + m1.m5
    )
  }

  // val identity = Array(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)

  // /** Function to extract svg matrix from element's transform attribute **/
  // def svgMatrix(e: Element): SvgMatrix = {

  //   val identity = Array(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
  //   def translate2Matrix(array: Array[Double]) = Array(1.0, 0.0, 0.0, 1.0, array(0), array(1))
  //   def scale2Matrix(array: Array[Double]) = Array(array(0), 0.0, 0.0, array(1), 0.0, 0.0)

  //   Option(e.getAttribute("transform")) match {
  //     case Some(attr) if !attr.getValue().isEmpty  =>

  //       val ms = attr.getValue().trim().split("((?<=\\))[\\s,]+)").map(str => {
  //         val firstN = str.indexOf("(") + 1
  //         val lastN = str.size - str.indexOf(")")
  //         val doubleArray = str.drop(firstN).dropRight(lastN).split("[\\s,]+").map(_.toDouble)
  //         if (str.startsWith("matrix(")) {
  //           assert(doubleArray.size == 6, "svg matrix has invalid size")
  //           doubleArray
  //         } else if (str.startsWith("translate(")) {
  //           assert(doubleArray.size == 2, "svg translate has invalid size")
  //           translate2Matrix(doubleArray)
  //         } else if (str.startsWith("scale(")) {
  //           assert(doubleArray.size == 2, "svg scale has invalid size")
  //           scale2Matrix(doubleArray)
  //         } else {
  //           assert(false, "could not parse: " + e.getAttribute("transform"))
  //           identity
  //         }
  //       })

  //       ms.foldLeft(identity) {
  //         case (mAcc, m) => svgMatrixMultiply(mAcc, m)
  //       }
  //     case _ =>
  //       identity
  //   }
  // }

  import dom._

  /** Constructor for position groups
    *
    * It used by Annotator instances to hold
    * the x and y positions of every character in a particular tspan
    * and the x position of the right side of the last character
    **/
  // case class PositionGroup(xs: List[Double], endX: Double, ys: List[Double])

  def getElemTransforms(e: WatrElement): List[Transform] = {
    e match {
      case t:Transformable => t.transforms
      case _               => List()
    }
  }

  /** Function to get source element's x and y positions in its ancestor's coordinate system **/
  def getTransformedCoords(domCursor: DomCursor): Option[TextXYOffsets] = {
    domCursor.getLabelAsTSpan
      .textXYOffsets
      .map({ offs =>

        domCursor.loc.path.map({case ancestorElem =>
          getElemTransforms(ancestorElem)
        }).foldLeft(offs)({ case (acc, tranformList) =>
          tranformList.foldLeft(acc){case (xys, t) =>
            applyTransform(t, xys)
          }
        })
      })
  }

  def applyTransform(t: Transform, offsets: TextXYOffsets): TextXYOffsets = {
    t match {
      case t: Scale =>
        applyTransform(t.toMatrix, offsets)
      case t: Translate =>
        applyTransform(t.toMatrix, offsets)

      case m: Matrix =>

        val _xs = offsets.xs.map((x =>
          m.m0 * x + m.m2 * offsets.ys.head + m.m4
        ))

        val _ys = offsets.xs.map(x=>
          m.m1 * x + m.m3 * offsets.ys.head + m.m5
        )

        val _endX = m.m0 * offsets.endX + m.m2 * offsets.ys.head + m.m4

        TextXYOffsets(
          xs=_xs,
          endX = _endX,
          ys=_ys
        )

    }

  }
  // def getTransformedCoordsElem(sourceE: Element, ancestorE: Element): PositionGroup = {

  //   def matrixTotal(e: Element): SvgMatrix = {
  //     require(e != null)
  //     val m = svgMatrix(e)
  //     if (e == ancestorE) {
  //       m
  //     } else {
  //       svgMatrixMultiply(matrixTotal(e.getParentElement()), m)
  //     }
  //   }

  //   val m = matrixTotal(sourceE)
  //   val sourceXs = Attr.xs(sourceE)
  //   val sourceY = Attr.y(sourceE)

  //   val _xs = sourceXs.map(x => {
  //     m(0) * x + m(2) * sourceY + m(4)
  //   })

  //   val _ys = sourceXs.map(x => {
  //     m(1) * x + m(3) * sourceY + m(5)
  //   })

  //   val _endX = m(0) * Attr.endX(sourceE) + m(2) * sourceY + m(4)

  //   PositionGroup(_xs.toList, _endX, _ys.toList)

  // }

}
