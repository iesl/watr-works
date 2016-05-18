package edu.umass.cs.iesl.watr
package ext

import watrmarks._

/**
  * Internal representation of the text line.
  */
class ComponentLine(
  val components: Seq[CharBox],
  val orientation: Double
) {

  var  x0 = 0d
  var  y0 = 0d
  var  x1 = 0d
  var  y1 = 0d
  var height = 0d

  def centerX(cb: CharBox) = cb.bbox.toCenterPoint.x
  def centerY(cb: CharBox) = cb.bbox.toCenterPoint.y

  if (components.length >= 2) {
    // Simple linear regression
    var sx = 0.0d
    var sxx = 0.0d
    var sxy = 0.0d
    var sy = 0.0d

    for (component <- components) {
      val x = centerX(component)
      val y = centerY(component)

      sx += x // component.getX();
        sxx += x*x //component.getX() * component.getX();
        sxy += x*y // component.getX() * component.getY();
        sy += y //component.getY();
    }

    val b:Double = (components.length * sxy - sx * sy) / (components.length * sxx - sx * sx);
    val a:Double = (sy - b * sx) / components.length;

    this.x0 = components(0).bbox.toCenterPoint.x
    this.y0 = a + b * this.x0;
    this.x1 = centerX(components(components.length - 1))
    this.y1 = a + b * this.x1;
  } else if (!components.isEmpty) {
    val component = components(0);
    // val dx = component.getChunk().getBounds().getWidth() / 3;
    val dx = component.bbox.width / 3

    val dy = dx * Math.tan(orientation);
    this.x0 = centerX(component) - dx;
    this.x1 = centerX(component) + dx;
    this.y0 = centerY(component) - dy;
    this.y1 = centerY(component) + dy;
  }
  else {
    sys.error("Component list must not be empty") 
  }
  height = computeHeight();


  def getAngle(): Double = {
    return Math.atan2(y1 - y0, x1 - x0);
  }

  // public double getSlope() {
  //     return (y1 - y0) / (x1 - x0);
  // }

  def getLength(): Double = {
      return Math.sqrt((x0 - x1) * (x0 - x1) + (y0 - y1) * (y0 - y1));
  }

  def computeHeight(): Double = {
    var sum = 0.0d
      for ( component <- components) {
        sum += component.bbox.height
      }
      return sum / components.length;
  }

  def getHeight(): Double = {
    height;
  }


  def angularDifference(j: ComponentLine): Double = {
    val diff = Math.abs(getAngle() - j.getAngle());
    if (diff <= Math.PI/2) {
      return diff;
    } else {
      return Math.PI - diff;
    }
  }

  def horizontalDistance(other: ComponentLine,  orientation: Double): Double = {
    var xs = Array[Double](0, 0, 0, 0)
    var s = Math.sin(-orientation)
    var c = Math.cos(-orientation);
    xs(0) = c * x0 - s * y0;
    xs(1) = c * x1 - s * y1;
    xs(2) = c * other.x0 - s * other.y0;
    xs(3) = c * other.x1 - s * other.y1;
    var overlapping = xs(1) >= xs(2) && xs(3) >= xs(0);
    xs = xs.sorted
    Math.abs(xs(2) - xs(1)) * (if(overlapping) 1 else -1)
  }

  def verticalDistance(other: ComponentLine, orientation: Double): Double = {
    val xm = (x0 + x1) / 2
    val  ym = (y0 + y1) / 2;
    val xn = (other.x0 + other.x1) / 2
    val yn = (other.y0 + other.y1) / 2;
    val a = Math.tan(orientation);
    return Math.abs(a * (xn - xm) + ym - yn) / Math.sqrt(a * a + 1);
  }

  // public BxLine convertToBxLine(double wordSpacing) {
  //   BxLine line = new BxLine();
  //   BxWord word = new BxWord();
  //   Component previousComponent = null;
  //   for (Component component : components) {
  //     if (previousComponent != null) {
  //       double dist = component.getChunk().getBounds().getX() -
  //         previousComponent.getChunk().getBounds().getX() -
  //         previousComponent.getChunk().getBounds().getWidth();
  //       if(dist > wordSpacing) {
  //         BxBoundsBuilder.setBounds(word);
  //         line.addWord(word);
  //         word = new BxWord();
  //       }
  //     }
  //     word.addChunk(component.getChunk());
  //     previousComponent = component;
  //   }
  //   BxBoundsBuilder.setBounds(word);
  //   line.addWord(word);
  //   BxBoundsBuilder.setBounds(line);
  //   return line;
  // }
  def convertToBxLine(wordSpacing: Double): ComponentLine = {
    // BxLine line = new BxLine();
    // val line = new ComponentLine()

    // BxWord word = new BxWord();
    // Component previousComponent = null;
    // for (Component component : components) {
    //   if (previousComponent != null) {
    //     double dist = component.getChunk().getBounds().getX() -
    //       previousComponent.getChunk().getBounds().getX() -
    //       previousComponent.getChunk().getBounds().getWidth();
    //     if(dist > wordSpacing) {
    //       BxBoundsBuilder.setBounds(word);
    //       line.addWord(word);
    //       word = new BxWord();
    //     }
    //   }
    //   word.addChunk(component.getChunk());
    //   previousComponent = component;
    // }
    // BxBoundsBuilder.setBounds(word);
    // line.addWord(word);
    // BxBoundsBuilder.setBounds(line);
    // return line;
    ???
  }
}
