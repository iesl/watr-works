package edu.umass.cs.iesl.watr
package ext

import watrmarks._

import _root_.pl.edu.icm.cermine.structure.model._
// import ComponentXComparator._
// import NeighborDistanceComparator._
// import AngleFilter._
// import scala.reflect.{BeanProperty, BooleanBeanProperty}
// //remove if not needed
// import scala.collection.JavaConversions._

case class Component(chunk: BxChunk) {

  val x = chunk.getBounds.getX + chunk.getBounds.getWidth / 2

  val y = chunk.getBounds.getY + chunk.getBounds.getHeight / 2

  var neighbors: List[Neighbor] = _

  val bounds = chunk.getBounds

  if (bounds == null) {
    throw new IllegalArgumentException("Bounds must not be null")
  }

  if (java.lang.Double.isNaN(bounds.getX) || java.lang.Double.isInfinite(bounds.getX)) {
    throw new IllegalArgumentException("Bounds x coordinate must be finite")
  }

  if (java.lang.Double.isNaN(bounds.getY) || java.lang.Double.isInfinite(bounds.getY)) {
    throw new IllegalArgumentException("Bounds y coordinate must be finite")
  }

  if (java.lang.Double.isNaN(bounds.getWidth) || java.lang.Double.isInfinite(bounds.getWidth)) {
    throw new IllegalArgumentException("Bounds width must be finite")
  }

  if (java.lang.Double.isNaN(bounds.getHeight) || java.lang.Double.isInfinite(bounds.getHeight)) {
    throw new IllegalArgumentException("Bounds height must be finite")
  }

  def getHeight(): Double = chunk.getBounds.getHeight

  def distance(c: Component): Double = {
    val dx = x - c.x
    val dy = y - c.y
    Math.sqrt(dx * dx + dy * dy)
  }

  def horizontalDistance(c: Component, orientation: Double): Double = Math.abs(x - c.x)

  def verticalDistance(c: Component, orientation: Double): Double = Math.abs(y - c.y)

  def horizontalBoundsDistance(c: Component, orientation: Double): Double = {
    horizontalDistance(c, orientation) - chunk.getBounds.getWidth / 2 -
      c.chunk.getBounds.getWidth / 2
  }

  def angle(c: Component): Double = {
    if (x > c.x) {
      Math.atan2(y - c.y, x - c.x)
    } else {
      Math.atan2(c.y - y, c.x - x)
    }
  }

  def overlappingDistance(other: Component, orientation: Double): Double = {
    val xs = Array.ofDim[Double](4)
    val s = Math.sin(-orientation)
    val c = Math.cos(-orientation)
    xs(0) = c * x - s * y
    xs(1) = c * (x + chunk.getWidth) - s * (y + chunk.getHeight)
    xs(2) = c * other.x - s * other.y
    xs(3) = c * (other.x + other.chunk.getWidth) - s * (other.y + other.chunk.getHeight)
    val overlapping = xs(1) >= xs(2) && xs(3) >= xs(0)
    val xsSort = xs.sorted
    Math.abs(xsSort(2) - xsSort(1)) * (if (overlapping) 1 else -1)
  }
}

case class Neighbor(neighbor: Component, origin: Component) {

  val distance = neighbor.distance(origin)

  val angle = neighbor.angle(origin)

  val component = neighbor

  def getHorizontalDistance(orientation: Double): Double = {
    component.horizontalDistance(origin, orientation)
  }

  def getVerticalDistance(orientation: Double): Double = {
    component.verticalDistance(origin, orientation)
  }
}

// object ComponentXComparator {

//   @BeanProperty
//   var instance: ComponentXComparator = new ComponentXComparator()
// }

// protected class ComponentXComparator private () extends Comparator[Component] {

//   override def compare(o1: Component, o2: Component): Int = {
//     java.lang.Double.compare(o1.getX, o2.getX)
//   }
// }

// object NeighborDistanceComparator {

//   @BeanProperty
//   var instance: NeighborDistanceComparator = new NeighborDistanceComparator()
// }

// protected class NeighborDistanceComparator private () extends Comparator[Neighbor] {

//   override def compare(o1: Neighbor, o2: Neighbor): Int = {
//     java.lang.Double.compare(o1.getDistance, o2.getDistance)
//   }
// }

// protected class ComponentLine(@BeanProperty var components: List[Component], orientation: Double)
//     {

//   private val x0: Double = _

//   private val y0: Double = _

//   private val x1: Double = _

//   private val y1: Double = _

//   @BeanProperty
//   val height = computeHeight()

//   if (components.size >= 2) {
//     var sx = 0.0
//     var sxx = 0.0
//     var sxy = 0.0
//     var sy = 0.0
//     for (component <- components) {
//       sx += component.getX
//       sxx += component.getX * component.getX
//       sxy += component.getX * component.getY
//       sy += component.getY
//     }
//     val b = (components.size * sxy - sx * sy) / (components.size * sxx - sx * sx)
//     val a = (sy - b * sx) / components.size
//     this.x0 = components.get(0).getX
//     this.y0 = a + b * this.x0
//     this.x1 = components.get(components.size - 1).getX
//     this.y1 = a + b * this.x1
//   } else if (!components.isEmpty) {
//     val component = components.get(0)
//     val dx = component.getChunk.getBounds.getWidth / 3
//     val dy = dx * Math.tan(orientation)
//     this.x0 = component.getX - dx
//     this.x1 = component.getX + dx
//     this.y0 = component.getY - dy
//     this.y1 = component.getY + dy
//   } else {
//     throw new IllegalArgumentException("Component list must not be empty")
//   }

//   def getAngle(): Double = Math.atan2(y1 - y0, x1 - x0)

//   def getSlope(): Double = (y1 - y0) / (x1 - x0)

//   def getLength(): Double = {
//     Math.sqrt((x0 - x1) * (x0 - x1) + (y0 - y1) * (y0 - y1))
//   }

//   private def computeHeight(): Double = {
//     var sum = 0.0
//     for (component <- components) {
//       sum += component.getHeight
//     }
//     sum / components.size
//   }

//   def angularDifference(j: ComponentLine): Double = {
//     val diff = Math.abs(getAngle - j.getAngle)
//     if (diff <= Math.PI / 2) {
//       diff
//     } else {
//       Math.PI - diff
//     }
//   }

//   def horizontalDistance(other: ComponentLine, orientation: Double): Double = {
//     val xs = Array.ofDim[Double](4)
//     val s = Math.sin(-orientation)
//     val c = Math.cos(-orientation)
//     xs(0) = c * x0 - s * y0
//     xs(1) = c * x1 - s * y1
//     xs(2) = c * other.x0 - s * other.y0
//     xs(3) = c * other.x1 - s * other.y1
//     val overlapping = xs(1) >= xs(2) && xs(3) >= xs(0)
//     Arrays.sort(xs)
//     Math.abs(xs(2) - xs(1)) * (if (overlapping) 1 else -1)
//   }

//   def verticalDistance(other: ComponentLine, orientation: Double): Double = {
//     val xm = (x0 + x1) / 2
//     val ym = (y0 + y1) / 2
//     val xn = (other.x0 + other.x1) / 2
//     val yn = (other.y0 + other.y1) / 2
//     val a = Math.tan(orientation)
//     Math.abs(a * (xn - xm) + ym - yn) / Math.sqrt(a * a + 1)
//   }

//   def convertToBxLine(wordSpacing: Double): BxLine = {
//     val line = new BxLine()
//     var word = new BxWord()
//     var previousComponent: Component = null
//     for (component <- components) {
//       if (previousComponent != null) {
//         val dist = component.getChunk.getBounds.getX - previousComponent.getChunk.getBounds.getX -
//           previousComponent.getChunk.getBounds.getWidth
//         if (dist > wordSpacing) {
//           BxBoundsBuilder.setBounds(word)
//           line.addWord(word)
//           word = new BxWord()
//         }
//       }
//       word.addChunk(component.getChunk)
//       previousComponent = component
//     }
//     BxBoundsBuilder.setBounds(word)
//     line.addWord(word)
//     BxBoundsBuilder.setBounds(line)
//     line
//   }
// }

// object AngleFilter {

//   def newInstance(lowerAngle: Double, upperAngle: Double): AngleFilter = {
//     if (lowerAngle < -Math.PI / 2) {
//       lowerAngle += Math.PI
//     }
//     if (upperAngle >= Math.PI / 2) {
//       upperAngle -= Math.PI
//     }
//     if (lowerAngle <= upperAngle) {
//       new AndFilter(lowerAngle, upperAngle)
//     } else {
//       new OrFilter(lowerAngle, upperAngle)
//     }
//   }

//   class AndFilter private (lowerAngle: Double, upperAngle: Double) extends AngleFilter(lowerAngle, upperAngle) {

//     override def matches(neighbor: Neighbor): Boolean = {
//       getLowerAngle <= neighbor.getAngle && neighbor.getAngle < getUpperAngle
//     }
//   }

//   class OrFilter private (lowerAngle: Double, upperAngle: Double) extends AngleFilter(lowerAngle, upperAngle) {

//     override def matches(neighbor: Neighbor): Boolean = {
//       getLowerAngle <= neighbor.getAngle || neighbor.getAngle < getUpperAngle
//     }
//   }
// }

// protected abstract class AngleFilter private (@BeanProperty val lowerAngle: Double, @BeanProperty val upperAngle: Double)
//     {

//   def matches(neighbor: Neighbor): Boolean
// }
