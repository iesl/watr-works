package edu.umass.cs.iesl.watr
package docseg

import watrmarks._
import Bounds._
import scala.collection.JavaConversions._
import scala.collection.mutable
import pl.edu.icm.cermine.tools.Histogram
import scalaz._
import Scalaz._


object Component {
  def centerX(cb: CharBox) = cb.bbox.toCenterPoint.x
  def centerY(cb: CharBox) = cb.bbox.toCenterPoint.y

  def apply(charBox: CharBox): ConnectedComponents = {
    apply(Seq(CharComponent(charBox, 0d)), 0d)
  }

  def apply(components: Seq[Component], orientation: Double, labels: Label*): ConnectedComponents = {
    new ConnectedComponents(
      components,
      orientation,
      labels
    )
  }


  def renderToString(cc: ConnectedComponents): String = {
    ???
  }
}

import Component._
// import DocstrumSegmenter._


sealed trait Component {
  def toText: String

  def bounds: LTBounds

  def x0: Double
  def y0: Double
  def x1: Double
  def y1: Double
  def height: Double


  def getAngle(): Double = {
    math.atan2(y1 - y0, x1 - x0);
  }

   def getSlope(): Double = {
     (y1 - y0) / (x1 - x0);
  }

  def getLength(): Double = {
    math.sqrt((x0 - x1) * (x0 - x1) + (y0 - y1) * (y0 - y1));
  }


  def angularDifference(j: Component): Double = {
    val diff = Math.abs(getAngle() - j.getAngle());
    if (diff <= Math.PI/2) {
      return diff;
    } else {
      return Math.PI - diff;
    }
  }

  def horizontalDistance(other: Component,  orientation: Double): Double = {
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

  def verticalDistance(other: Component, orientation: Double): Double = {
    val xm = (x0 + x1) / 2
    val  ym = (y0 + y1) / 2;
    val xn = (other.x0 + other.x1) / 2
    val yn = (other.y0 + other.y1) / 2;
    val a = Math.tan(orientation);
    return Math.abs(a * (xn - xm) + ym - yn) / Math.sqrt(a * a + 1);
  }


  def withLabel(l: Label): Component

}


case class CharComponent(
  val component: CharBox,
  val orientation: Double
) extends Component {
  val dx = component.bbox.width / 3
  val dy = dx * math.tan(orientation);

  lazy val x0 = centerX(component) - dx;
  lazy val x1 = centerX(component) + dx;
  lazy val y0 = centerY(component) - dy;
  lazy val y1 = centerY(component) + dy;
  val bounds = component.bbox
  def height: Double  = bounds.height

  def toText: String = component.char
  def withLabel(l: Label): Component = {
    Component(Seq(this), orientation, l)
  }
}

case class ConnectedComponents(
  val components: Seq[Component],
  val orientation: Double,
  val labels: Seq[Label] = Seq()
) extends Component {

  def withLabel(l: Label): Component = {
    this.copy(labels = labels :+ l)
  }

  def toText = {

    def wrap(s: String): String = {
      if (labels.contains(LB.Sup)) {
        s"^${s}^"
      } else if (labels.contains(LB.Sub)) {
        s"_${s}_"
      } else s
    }

    val text  = if (labels.contains(LB.Line)) {
      val headbbox = components.headOption.map({c => c.bounds.prettyPrint})
      val lastbbox = components.lastOption.map({c => c.bounds.prettyPrint})
      components.map(_.toText).mkString(" ") + s"  ${headbbox} - ${lastbbox}"
    } else if (labels.contains(LB.Word)) {
      components.map(_.toText).mkString
    } else if (labels.contains(LB.Zone)) {
      components.map(_.toText).mkString("\n>","\n>", "\n")
    } else {
      components.map(_.toText).mkString("|")
    }

    wrap(text)

  }

  override val (x0, y0, x1, y1) = if (components.length >= 2) {
    val (sx, sxx, sxy, sy) = components
      .foldLeft((0d, 0d, 0d, 0d))({ case ((sx, sxx, sxy, sy), comp) =>
        val c = comp.bounds.toCenterPoint
        (sx + c.x, sxx + c.x*c.x, sxy + c.x*c.y, sy + c.y)
      })


    val b:Double = (components.length * sxy - sx * sy) / (components.length * sxx - sx * sx);
    val a:Double = (sy - b * sx) / components.length;

    val _x0 = components.head.bounds.toCenterPoint.x
    val _x1 = components.last.bounds.toCenterPoint.x
    val _y0 = a + b * _x0;
    val _y1 = a + b * _x1;

    (_x0, _y0, _x1, _y1)

  } else if (!components.isEmpty) {
    val component = components.head;
    val dx = component.bounds.width / 3
    val dy = dx * Math.tan(orientation);

    val _x0 = components.head.bounds.toCenterPoint.x - dx
    val _x1 = components.last.bounds.toCenterPoint.x + dx
    val _y0 = components.head.bounds.toCenterPoint.y - dy
    val _y1 = components.last.bounds.toCenterPoint.y + dy
    (_x0, _y0, _x1, _y1)
  }
  else {
    sys.error("Component list must not be empty")
  }

  val bounds: LTBounds = components.tail
    .map(_.bounds)
    .foldLeft(components.head.bounds)( { case (b1, b2) =>
      b1 union b2
    })

  def height: Double  = bounds.height

  // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)

  def pairwiseSpaceWidths(): Seq[Double] = {
    val cpairs = components.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.bounds.left - c1.bounds.right
      case _  => 0d
    })

    dists :+ 0d
  }

  def determineSpacings(): Seq[Double] = {
    val dists = pairwiseSpaceWidths()
    val resolution = 0.5d

    val histogram = Histogram.fromValues(dists.toList.map(new java.lang.Double(_)), resolution)

    val spaceDists = histogram.iterator.toList
      .sortBy(_.getFrequency)
      // .dropWhile(_.getFrequency==0)
      .map(_.getValue)
      .reverse

    spaceDists
  }

  def findCenterY(): Double = {
    components.map({c => c.bounds.toCenterPoint.y}).sum / components.length
  }

  def findCommonToplines(): Seq[Double] = {
    val baselines = components.map({c => c.bounds.top})
    val histogram = Histogram.fromValues(baselines.toList.map(new java.lang.Double(_)), 0.2)

    histogram.iterator.toList
      .sortBy(_.getFrequency)
      // .dropWhile(_.getFrequency==0)
      .map(_.getValue)
      .reverse
  }

  def findCommonBaselines(): Seq[Double] = {
    val baselines = components.map({c => c.bounds.bottom})
    val histogram = Histogram.fromValues(baselines.toList.map(new java.lang.Double(_)), 0.2)

    histogram.iterator.toList
      .sortBy(_.getFrequency)
      // .dropWhile(_.getFrequency==0)
      .map(_.getValue)
      .reverse
  }

  def splitAtBreaks(bis: Seq[Int], cs: Seq[Component]): Seq[Seq[Component]] = {
    // println(s"""splitAtBreaks: bis=${bis.mkString(",")}""")
    // println(s"""        cs=${cs.map(_.toText).mkString("")}""")
    if (bis.isEmpty){
      Seq(cs)
    } else {
      val (pre, post) = cs.splitAt(bis.head+1)
      // println(s"""        pre=${pre.map(_.toText).mkString("")}""")
      // println(s"""        post=${post.map(_.toText).mkString("")}""")
      pre +: splitAtBreaks(bis.tail.map(_-bis.head-1), post)
    }
  }

  def printCCStats(range: (Int, Int), centerY: Double): Unit = {
    import TB._

    val stats = components.zip(pairwiseSpaceWidths())
      .drop(range._1)
      .take(range._2).map({case (c, dist) =>
        (tbox(c.toText) +| "->" +| (dist.pp)) %
          c.bounds.top.pp %
          (c.bounds.left.pp +| c.bounds.right.pp) %
          (c.bounds.bottom.pp +| "(w:" +| c.bounds.width.pp)
      }).toList

    println(
      hsep(1)(TB.top)(stats)
    )
  }

  def tokenizeLine(): ConnectedComponents = {
    // println(s"   splitval = ${splitValue}")
    val tops = findCommonToplines()
    val bottoms = findCommonBaselines()
    val modalTop = tops.head
    val modalBottom = bottoms.head

    // find the center y-val (avg for all chars)
    val centerY = findCenterY()

    println(s"""tops: ${tops.map(_.pp).mkString(" ")}""")
    println(s"""bottoms: ${bottoms.map(_.pp).mkString(" ")}""")


    // label super/sub if char.ctr fall above/below centerline
    val supSubs = components.map({c =>

      // if (c.bounds.top.gtFuzzy(1.5)(modalTop) && c.bounds.bottom.gtFuzzy(0.4)(modalBottom)) {
    // } else if (c.bounds.bottom.ltFuzzy(1.5)(modalBottom) && c.bounds.top.ltFuzzy(0.1)(modalTop)) {
      if (c.bounds.top.gtFuzzy(1.5)(modalTop)) {
        c.withLabel(LB.Sub)
      } else if (c.bounds.bottom.ltFuzzy(1.5)(modalBottom)) {
        c.withLabel(LB.Sup)
      } else {
        c
      }
    })


    val charDists = determineSpacings()
    val modalLittleGap = charDists.head
    val modalBigGap = charDists.drop(1).headOption.getOrElse(modalLittleGap)

    // val splitValue = charDists.take(2).sum / 2
    val splitValue = (modalBigGap+modalLittleGap)/2

    println(s""" char dists = ${charDists.map(_.pp).mkString(", ")}, splitVal = ${splitValue}""")

    printCCStats((9, 12), centerY)
    // printCCStats((20, 15), centerY)

    val wordBreaks = mutable.ArrayBuffer[Int]()

    supSubs
      .sliding(2).toList
      .zipWithIndex
      .foreach({ case(Seq(c1, c2), i)  =>
        val dist = c2.bounds.left - c1.bounds.right

        if(dist > splitValue) {
          wordBreaks.append(i)
        }
      })

    val asTokens = splitAtBreaks(wordBreaks, supSubs)
      .map(Component(_, 0d, LB.Word))

    Component(asTokens, 0d, LB.Line)
  }



  def convertToBxLine(): ConnectedComponents = {

    val cpairs = components.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.bounds.left - c1.bounds.right
      case _  => 0d
    })

    val resolution = 0.5d

    val histogram = Histogram.fromValues(dists.toList.map(new java.lang.Double(_)), resolution)


    val top2Spacings = histogram.iterator.toList.sortBy(_.getFrequency).reverse.take(2)

    val splitValue = top2Spacings match {
      case sp1 :: sp2 :: Nil =>
        // println(s"    top 2 = ${sp1.getValue}, ${sp2.getValue}")
        (sp1.getValue+sp2.getValue) / 2
      case sp1 :: Nil =>
        // println(s"   top 1 = ${sp1.getValue}")
        math.abs(sp1.getValue)+1.0d
      case _ =>
        // println(s"   top = ?")
        0d
    }
    // println(s"   splitval = ${splitValue}")

    // val histstr = histogram.iterator().map(x => s"""v=${x.getValue}, fr=${x.getFrequency}""").toList.mkString("\n  ","  \n  ", "\n")
    // println(s"""|hist = ${histstr},
    //             |  peak = ${histogram.getPeakValue}
    //             |""".stripMargin)


    val wordBreaks = mutable.ArrayBuffer[Int]()

    if(components.length < 2) {
      Component(components, 0d)
    } else {
      cpairs.zipWithIndex
        .foreach({ case(Seq(c1, c2), i)  =>
          val dist = c2.bounds.left - c1.bounds.right

          // println(s"""| ${c1.toText} - ${c2.toText}
          //             |    ${c1.bounds.prettyPrint} - ${c2.bounds.prettyPrint}
          //             |    c1-left: ${c1.bounds.left} c1-right: ${c1.bounds.right} c2-left: ${c2.bounds.left}
          //             |    dist = ${c2.bounds.left} - ${c1.bounds.right} = ${c2.bounds.left - c1.bounds.right}
          //             |    dist= ${dist.pp} wordSpace=${splitValue}""".stripMargin)
          // println(s"""| ${c1.toText} - ${c2.toText}   ${c1.bounds.prettyPrint} - ${c2.bounds.prettyPrint}
          //             |     dist=${dist.pp} ws=${splitValue}""".stripMargin)

          if(dist > splitValue) {
            wordBreaks.append(i)
          }
        })



      val tokenized = splitAtBreaks(wordBreaks, components)

      val tccs = tokenized.map({ ts =>
        Component(ts, 0d, LB.Word)
      })

      val lineComp = Component(tccs, 0d, LB.Line)

      // val relWordbreaks = wordBreaks.sliding(2).map({
      //   case Seq(i) => i
      //   case Seq(i, j) => j-i
      // })

      // println(s"word breaks = ${wordBreaks.mkString(", ")}")
      // println(s"relative word breaks = ${relWordbreaks.mkString(", ")}")

      // val (ctail, words) = relWordbreaks
      //   .foldLeft((components, List[ConnectedComponents]()))({
      //     case ((remaining, words), breakIndex) =>
      //       println(s"""|remaining: ${remaining.map(_.toText).mkString},
      //                   |words: '${words.map(_.toText).mkString(" | ")}',
      //                   |break: ${breakIndex}""".stripMargin)

      //       val (w0, r0) = remaining.splitAt(breakIndex+1)
      //       val word = Component(w0, 0d)

      //       (r0, words :+ word)
      //   })

      // val lastWord = Component(ctail, 0d)
      // val finalLine = words :+ lastWord
      // val lineComp = Component(finalLine, 0d)

      // println(s"line = ${lineComp.toText}")

      lineComp

    }
  }


}

// class ComponentLine(
//   val components: Seq[CharBox],
//   val orientation: Double
// ) {

//   var  x0 = 0d
//   var  y0 = 0d
//   var  x1 = 0d
//   var  y1 = 0d
//   var height = 0d


//   if (components.length >= 2) {
//     // Simple linear regression
//     var sx = 0.0d
//     var sxx = 0.0d
//     var sxy = 0.0d
//     var sy = 0.0d

//     for (component <- components) {
//       val x = centerX(component)
//       val y = centerY(component)

//       sx += x // component.getX();
//         sxx += x*x //component.getX() * component.getX();
//         sxy += x*y // component.getX() * component.getY();
//         sy += y //component.getY();
//     }

//     val b:Double = (components.length * sxy - sx * sy) / (components.length * sxx - sx * sx);
//     val a:Double = (sy - b * sx) / components.length;

//     this.x0 = components(0).bbox.toCenterPoint.x
//     this.y0 = a + b * this.x0;
//     this.x1 = centerX(components(components.length - 1))
//     this.y1 = a + b * this.x1;
//   } else if (!components.isEmpty) {
//     val component = components(0);
//     // val dx = component.getChunk().getBounds().getWidth() / 3;
//     val dx = component.bbox.width / 3

//     val dy = dx * Math.tan(orientation);
//     this.x0 = centerX(component) - dx;
//     this.x1 = centerX(component) + dx;
//     this.y0 = centerY(component) - dy;
//     this.y1 = centerY(component) + dy;
//   }
//   else {
//     sys.error("Component list must not be empty")
//   }
//   height = computeHeight();


//   def getAngle(): Double = {
//     return Math.atan2(y1 - y0, x1 - x0);
//   }

//   // public double getSlope() {
//   //     return (y1 - y0) / (x1 - x0);
//   // }

//   def getLength(): Double = {
//       return Math.sqrt((x0 - x1) * (x0 - x1) + (y0 - y1) * (y0 - y1));
//   }

//   def computeHeight(): Double = {
//     var sum = 0.0d
//       for ( component <- components) {
//         sum += component.bbox.height
//       }
//       return sum / components.length;
//   }

//   def getHeight(): Double = {
//     height;
//   }


//   def angularDifference(j: ComponentLine): Double = {
//     val diff = Math.abs(getAngle() - j.getAngle());
//     if (diff <= Math.PI/2) {
//       return diff;
//     } else {
//       return Math.PI - diff;
//     }
//   }

//   def horizontalDistance(other: ComponentLine,  orientation: Double): Double = {
//     var xs = Array[Double](0, 0, 0, 0)
//     var s = Math.sin(-orientation)
//     var c = Math.cos(-orientation);
//     xs(0) = c * x0 - s * y0;
//     xs(1) = c * x1 - s * y1;
//     xs(2) = c * other.x0 - s * other.y0;
//     xs(3) = c * other.x1 - s * other.y1;
//     var overlapping = xs(1) >= xs(2) && xs(3) >= xs(0);
//     xs = xs.sorted
//     Math.abs(xs(2) - xs(1)) * (if(overlapping) 1 else -1)
//   }

//   def verticalDistance(other: ComponentLine, orientation: Double): Double = {
//     val xm = (x0 + x1) / 2
//     val  ym = (y0 + y1) / 2;
//     val xn = (other.x0 + other.x1) / 2
//     val yn = (other.y0 + other.y1) / 2;
//     val a = Math.tan(orientation);
//     return Math.abs(a * (xn - xm) + ym - yn) / Math.sqrt(a * a + 1);
//   }

//   // public BxLine convertToBxLine(double wordSpacing) {
//   //   BxLine line = new BxLine();
//   //   BxWord word = new BxWord();
//   //   Component previousComponent = null;
//   //   for (Component component : components) {
//   //     if (previousComponent != null) {
//   //       double dist = component.getChunk().getBounds().getX() -
//   //         previousComponent.getChunk().getBounds().getX() -
//   //         previousComponent.getChunk().getBounds().getWidth();
//   //       if(dist > wordSpacing) {
//   //         BxBoundsBuilder.setBounds(word);
//   //         line.addWord(word);
//   //         word = new BxWord();
//   //       }
//   //     }
//   //     word.addChunk(component.getChunk());
//   //     previousComponent = component;
//   //   }
//   //   BxBoundsBuilder.setBounds(word);
//   //   line.addWord(word);
//   //   BxBoundsBuilder.setBounds(line);
//   //   return line;
//   // }

//   def convertToBxLine(wordSpacing: Double): ComponentLine = {
//     // BxLine line = new BxLine();
//     // val line = new ComponentLine()

//     // BxWord word = new BxWord();
//     // Component previousComponent = null;
//     // for (Component component : components) {
//     //   if (previousComponent != null) {
//     //     double dist = component.getChunk().getBounds().getX() -
//     //       previousComponent.getChunk().getBounds().getX() -
//     //       previousComponent.getChunk().getBounds().getWidth();
//     //     if(dist > wordSpacing) {
//     //       BxBoundsBuilder.setBounds(word);
//     //       line.addWord(word);
//     //       word = new BxWord();
//     //     }
//     //   }
//     //   word.addChunk(component.getChunk());
//     //   previousComponent = component;
//     // }
//     // BxBoundsBuilder.setBounds(word);
//     // line.addWord(word);
//     // BxBoundsBuilder.setBounds(line);
//     // return line;
//     ???
//   }
// }
