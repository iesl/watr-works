package edu.umass.cs.iesl.watr
package watrmarks

object SvgMatrix {

  // import dom._

  // def getElemTransforms(e: WatrElement): List[Transform] = {
  //   e match {
  //     case t:Transformable => t.transforms
  //     case _               => List()
  //   }
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

case class Translate(
  m0: Double, m1: Double
) extends Transform {
  override def toString = s"""tr[$m0,$m1]"""

  override val toMatrix = Matrix.translate(m0, m1)
}


object Matrix {
  def scale(sx: Double, sy:Double): Matrix = {
    apply(sx, 0, 0, sy, 0, 0)
  }

  def translate(tx: Double, ty:Double): Matrix = {
    apply(1, 0, 0, 1, tx, ty)
  }

  def apply(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double): Matrix = {
    Matrix(
      i11 = a, i12 = c, i13 = e,
      i21 = b, i22 = d, i23 = f,
      i31 = 0, i32 = 0, i33 = 1
    )
  }

  lazy val idMatrix = Matrix()

  def multiply(m1: Matrix, m2:Matrix): Matrix = {
    Matrix(
      m1.i11*m2.i11 + m1.i12*m2.i21 + m1.i13*m2.i31,
      m1.i11*m2.i12 + m1.i12*m2.i22 + m1.i13*m2.i32,
      m1.i11*m2.i13 + m1.i12*m2.i23 + m1.i13*m2.i33,
      //
      m1.i21*m2.i11 + m1.i22*m2.i21 + m1.i23*m2.i31,
      m1.i21*m2.i12 + m1.i22*m2.i22 + m1.i23*m2.i32,
      m1.i21*m2.i13 + m1.i22*m2.i23 + m1.i23*m2.i33,
      //
      m1.i31*m2.i11 + m1.i32*m2.i21 + m1.i33*m2.i31,
      m1.i31*m2.i12 + m1.i32*m2.i22 + m1.i33*m2.i32,
      m1.i31*m2.i13 + m1.i32*m2.i23 + m1.i33*m2.i33
    )
  }

  def multiply(m1: Matrix, m2:Vector): Vector = {
    Vector(
      m1.i11*m2.x + m1.i12*m2.y + m1.i13*1,
      m1.i21*m2.x + m1.i22*m2.y + m1.i23*1
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
    Matrix.multiply(this, v)
  }


  def multiply(by: Matrix): Matrix = {
    Matrix.multiply(this, by)
  }

  override def toString() = {
    // s"""||$i11  $i12  $i13|
    //     ||$i21  $i22  $i23|
    //     ||$i31  $i32  $i33|
    //     |""".stripMargin
    s"""[[$i11  $i12  $i13] [$i21  $i22  $i23] [$i31  $i32  $i33]]"""
  }
}
