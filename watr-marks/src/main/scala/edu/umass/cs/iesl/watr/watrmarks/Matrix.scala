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
  // def svgMatrixMultiply(m1: Matrix, m2: Matrix): Matrix = {
  //   Matrix(
  //     m1.m0 * m2.m0 + m1.m2 * m2.m1,
  //     m1.m1 * m2.m0 + m1.m3 * m2.m1,
  //     m1.m0 * m2.m2 + m1.m2 * m2.m3,
  //     m1.m1 * m2.m2 + m1.m3 * m2.m3,
  //     m1.m0 * m2.m4 + m1.m2 * m2.m5 + m1.m4,
  //     m1.m1 * m2.m4 + m1.m3 * m2.m5 + m1.m5
  //   )
  // }

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

  def getElemTransforms(e: WatrElement): List[Transform] = {
    e match {
      case t:Transformable => t.transforms
      case _               => List()
    }
  }

  // /** Function to get source element's x and y positions in its ancestor's coordinate system **/
  // def getTransformedCoords(domCursor: DomCursor): Option[TextXYOffsets] = {
  //   domCursor.getLabelAsTSpan
  //     .textXYOffsets
  //     .map({ offs =>
  //       domCursor.loc.path.map({case ancestorElem =>
  //         getElemTransforms(ancestorElem)
  //       }).foldLeft(offs)({ case (acc, tranformList) =>
  //         tranformList.foldLeft(acc){case (xys, t) =>
  //           applyTransform(t, xys)
  //         }
  //       })
  //     })
  // }

  // def applyTransform(t: Matrix, offsets: TextXYOffsets): TextXYOffsets = {
  //   t match {
  //     case t: Scale =>
  //       applyTransform(t.toMatrix, offsets)
  //     case t: Translate =>
  //       applyTransform(t.toMatrix, offsets)

  //     case m: Matrix =>

  //       val _xs = offsets.xs.map((x =>
  //         m.m0 * x + m.m2 * offsets.ys.head + m.m4
  //       ))

  //       val _ys = offsets.xs.map(x=>
  //         m.m1 * x + m.m3 * offsets.ys.head + m.m5
  //       )

  //       val _endX = m.m0 * offsets.endX + m.m2 * offsets.ys.head + m.m4

  //       TextXYOffsets(
  //         xs=_xs,
  //         endX = _endX,
  //         ys=_ys
  //       )

  //   }

  // }
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

sealed trait Transform {
  def toMatrix: Matrix
}

case class Scale(
  m0: Double, m1: Double
) extends Transform {
  override def toString = s"""scale[$m0,$m1]"""
  override val toMatrix = Matrix.scale(m0, m1)
}

// case class Matrix(
//   m0: Double, m1: Double, m2: Double,
//   m3: Double, m4: Double, m5: Double
// ) extends Transform {

//   override def toString = s"""mat[$m0,$m1,$m2,$m3,$m4,$m5]"""

//   override val toMatrix = this
// }

case class Translate(
  m0: Double, m1: Double
) extends Transform {
  override def toString = s"""tr[$m0,$m1]"""

  // override val toMatrix = Matrix(1.0, 0.0, 0.0, 1.0, m0, m1)
  override val toMatrix = Matrix.translate(m0, m1)
}


object Matrix {
  def scale(sx: Double, sy:Double): Matrix = {
    Matrix(
      i11=sx, i22=sy
    )
  }

  def translate(tx: Double, ty:Double): Matrix = {
    Matrix(
      i31=tx, i32=ty
    )
  }

  def apply(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double): Matrix = {
    Matrix(
      i11 = a, i12 = b, i13 = 0,
      i21 = c, i22 = d, i23 = 0,
      i31 = e, i32 = f, i33 = 1
    )
  }

}


case class Vector(x: Double, y: Double)


/** I11 I12 I13<p>
  * I21 I22 I23<p>
  * I31 I32 I33<p>
  */
case class Matrix(
  i11: Double=1, i12: Double=0, i13: Double=0,
  i21: Double=0, i22: Double=1, i23: Double=0,
  i31: Double=0, i32: Double=0, i33: Double=1
) extends Transform {

  override val toMatrix = this


  def transform(v: Vector): Vector = {
    val x0 = i11 * v.x + i21 * v.y + i31
    val y0 = i21 * v.x + i22 * v.y + i32
    Vector(x0, y0)
  }


  def multiply(by: Matrix): Matrix = {
    Matrix(
      i11*by.i11 + i12*by.i21 + i13*by.i31,
      i11*by.i12 + i12*by.i22 + i13*by.i32,
      i11*by.i13 + i12*by.i23 + i13*by.i33,
      i21*by.i11 + i22*by.i21 + i23*by.i31,
      i21*by.i12 + i22*by.i22 + i23*by.i32,
      i21*by.i13 + i22*by.i23 + i23*by.i33,
      i31*by.i11 + i32*by.i21 + i33*by.i31,
      i31*by.i12 + i32*by.i22 + i33*by.i32,
      i31*by.i13 + i32*by.i23 + i33*by.i33
    )
  }

  // /**
  //  * Subtracts a matrix from this matrix and returns the results
  //  * @param arg the matrix to subtract from this matrix
  //  * @return a Matrix object
  //  */
  // public Matrix subtract(Matrix arg){
  //     Matrix rslt = new Matrix();

  //     float[] a = vals;
  //     float[] b = arg.vals;
  //     float[] c = rslt.vals;

  //     c[I11] = a[I11]-b[I11];
  //     c[I12] = a[I12]-b[I12];
    //     c[I13] = a[I13]-b[I13];
    //     c[I21] = a[I21]-b[I21];
    //     c[I22] = a[I22]-b[I22];
    //     c[I23] = a[I23]-b[I23];
    //     c[I31] = a[I31]-b[I31];
    //     c[I32] = a[I32]-b[I32];
    //     c[I33] = a[I33]-b[I33];

    //     return rslt;
    // }

    // /**
    //  * Computes the determinant of the matrix.
    //  * @return the determinant of the matrix
    //  * @since 5.0.3
    //  */
    // public float getDeterminant(){
    //     // ref http://en.wikipedia.org/wiki/Determinant
    //     // note that in PDF, I13 and I23 are always 0 and I33 is always 1
    //     // so this could be simplified/faster
    //     return    vals[I11] * vals[I22] * vals[I33]
    //             + vals[I12] * vals[I23] * vals[I31]
    //             + vals[I13] * vals[I21] * vals[I32]
    //             - vals[I11] * vals[I23] * vals[I32]
    //             - vals[I12] * vals[I21] * vals[I33]
    //             - vals[I13] * vals[I22] * vals[I31];
    // }


  override def toString() = {
    s"""|$i11  $i12  $i13
        |$i21  $i22  $i13
        |$i31  $i32  $i33
        |""".stripMargin
  }
}
