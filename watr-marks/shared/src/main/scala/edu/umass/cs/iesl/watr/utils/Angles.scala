package edu.umass.cs.iesl.watr
package utils 

/**
  * Filter class for neighbor objects that checks if the angle of the
  * neighbor is within specified range.
  */
object AngleFilter {
  def apply(lowerAngle: Double, upperAngle: Double): AngleFilter = {
    val low = if (lowerAngle < -Math.PI/2) {
      lowerAngle + Math.PI;
    } else lowerAngle

    val hi = if (upperAngle >= Math.PI/2) {
      upperAngle - Math.PI;
    } else upperAngle

    if (low <= hi) {
      new AndFilter(low, hi);
    } else {
      new OrFilter(low, hi);
    }

  }
}

trait AngleFilter {
  def lowerAngle: Double
  def upperAngle: Double

  def matches(angle: Double): Boolean
}

class AndFilter(
  override val lowerAngle: Double,
  override val upperAngle: Double
) extends AngleFilter {


  def matches(angle: Double): Boolean = {
    // val nangle = n.bbox.toCenterPoint.angleTo(origin.bbox.toCenterPoint)
    lowerAngle <= angle && angle < upperAngle
  }
}

class OrFilter(
  override val lowerAngle: Double,
  override val upperAngle: Double
) extends AngleFilter {


  def matches(angle: Double): Boolean = {
    // val nangle = n.bbox.toCenterPoint.angleTo(origin.bbox.toCenterPoint)
    lowerAngle <= angle || angle < upperAngle
  }

}


