package edu.umass.cs.iesl.watr
package segment

import watrmarks._
import Bounds._
// import scala.collection.JavaConversions._
import scala.collection.mutable
// import pl.edu.icm.cermine.tools.Histogram
import scalaz._
import Scalaz._


import DocumentSegmenter._

object CharacterAccumulator {
  val charSet: mutable.Set[Char] = mutable.Set()
}


case class CCRenderState(
  numOfPages: Int,
  startingPage: Int@@PageID = PageID(0),
  idgen: IdGenerator[TokenID] = IdGenerator[TokenID](),
  tokens:mutable.ArrayBuffer[(Int@@PageID, Int@@TokenID, LTBounds)] = mutable.ArrayBuffer()
) {
  private var currPage: Int@@PageID = startingPage

  def currentPage: Int@@PageID = currPage

  def advancePage(): Int@@PageID = {
    currPage = PageID(PageID.unwrap(currPage)+1)
    currentPage
  }
}

object Component {
  def centerX(cb: CharBox) = cb.bbox.toCenterPoint.x
  def centerY(cb: CharBox) = cb.bbox.toCenterPoint.y

  val LB = StandardLabels

  def apply(charBox: CharBox): ConnectedComponents = {
    new ConnectedComponents(
      Seq(CharComponent(charBox, 0d)),
      0.0d,
      None
    )
  }
  def apply(components: Seq[Component]): ConnectedComponents = {
    new ConnectedComponents(
      components,
      0.0d
    )
  }

  def apply(components: Seq[Component], label: Label): ConnectedComponents = {
    new ConnectedComponents(
      components,
      0.0d,
      Option(label)
    )
  }

  def spaceWidths(cs: Seq[CharBox]): Seq[Double] = {
    pairwiseSpaceWidths(cs.map(Component(_)))
  }

  def pairwiseSpaceWidths(cs: Seq[Component]): Seq[Double] = {
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.bounds.left - c1.bounds.right
      case _  => 0d
    })

    dists :+ 0d
  }

  def charInfosBox(cbs: Seq[CharBox]): Seq[TB.Box] = {
    import TB._

    cbs.zip(spaceWidths(cbs))
      .map{ case (c, dist) =>
        (tbox(c.char.toString) +| "->" +| (dist.pp)) %
          c.bbox.top.pp %
          (c.bbox.left.pp +| c.bbox.right.pp) %
          (c.bbox.bottom.pp +| "(w:" + c.bbox.width.pp + ")")
    }
  }

  def determineCharSpacings(chars: Seq[CharBox]): Seq[Double] = {
    val dists = spaceWidths(chars)
    val resolution = 0.5d

    val hist = histogram(dists, resolution)

    val spaceDists = hist.getFrequencies
      .sortBy(_.frequency)
      .dropWhile(_.frequency==0)
      .map(_.value)
      .reverse

    spaceDists
  }




  def renderConnectedComponents(_cc: Component)(implicit ostate: Option[CCRenderState] = None): Seq[TB.Box] = {
    import TB._

    _cc match {
      case cc: ConnectedComponents =>
        cc.blockRole.map{ _ match {
          case LB.Line =>
            renderConnectedComponents(cc.tokenizeLine())

          case LB.Page =>
            // should be a set of blocks
            val vs = cc.components.flatMap({ c=>
              renderConnectedComponents(c)
            })
            Seq(vcat(vs))

          case LB.Column =>
            Seq(
              vcat(cc.components.flatMap({ c=>
                renderConnectedComponents(c)
              })))


          case LB.TokenizedLine =>
            // println(s"   ${cc.blockRole}")
            ostate.map{ state =>
              val currPage = state.currentPage

              val vs = cc.components.map({ c=>
                val tokenId = state.idgen.nextId
                state.tokens.append((currPage, tokenId, c.bounds))
                val trend = renderConnectedComponents(c)
                val token = hcat(trend)

                val quoted = "\"".box+token+"\""
                (quoted,  tokenId.toString.box)
              })

              Seq(
                "[[".box + hsepb(vs.map(_._1), ",") +"]" + ",     " + "[" + hsepb(vs.map(_._2), ",") +"]]"
              )

            } getOrElse {
              val vs = cc.components.map({ c=>
                val trend = renderConnectedComponents(c)
                hcat(trend)
              })

              Seq(hsep(vs))
            }


          case LB.Token =>
            // println(s"   ${cc.blockRole}")
            val clabels:Set[Label] = cc.components.map(_.containedLabels).reduce(_++_)
            val texFmtLabels = (clabels intersect Set(LB.Sup, LB.Sub))

            val subs = Seq(
              ('"' -> "\\\""),
              ('\\' -> "\\\\"),
              ('{' -> "\\\\{"),
              ('}' -> "\\\\}")
            ) ++ (if (!texFmtLabels.isEmpty) Seq( // add extra escapes inside tex-formatted tokens
              ('_' -> "\\\\_"),
              ('^' -> "\\\\^")
            ) else Seq())

            val mapped = cc.components.map({c =>
              hcat(
                renderConnectedComponents(
                  c.mapChars(subs)))
            })

            // println(s"""| Rendering token: ${cc.chars}
            //             |   ${texFmtLabels}
            //             |   ${mapped.map(_.chars).mkString(" ")}
            //             |""".stripMargin)

            if (texFmtLabels.isEmpty) {
              mapped
            } else {
              "{".box +: mapped :+ "}".box
            }

          case LB.Sup   =>
            // println(s"   ${cc.blockRole}")
            val vs = cc.components.flatMap(c =>
              renderConnectedComponents(c)
            )

            Seq("^{".box + hcat(vs) + "}")

          case LB.Sub   =>
            // println(s"   ${cc.blockRole}")
            val vs = cc.components.flatMap(c =>
              renderConnectedComponents(c)
            )

            Seq("_{".box + hcat(vs) + "}")

          case LB.Block =>
            // println(s"   ${cc.blockRole}")
            val vs = cc.components.map(c =>
              hcat(renderConnectedComponents(c))
            )


            Seq(
              s"""{"labels": ["body"], "lines": [""".box %
                indent(4)(vjoinTrailSep(left, ",")(vs:_*)) %
              "]}".box
            )

          case LB.Para  => ???
          case LB.Image => ???
          case LB.Table => ???
          case x =>
            println(s"  ??? ${cc.blockRole}")
            val vs = cc.components.flatMap(c =>
              renderConnectedComponents(c)
            )

            Seq(hcat(vs))
        }} getOrElse {
          val vs = cc.components.flatMap(c =>
            renderConnectedComponents(c)
          )

          Seq(hcat(vs))
        }



      case charcomp: CharComponent =>
        Seq(charcomp.component.bestGuessChar.box)
    }
  }


  def debugLineComponentStats(linecc: ConnectedComponents): Unit = {
    // linecc.components.foreach{_ match {
    //   case cc: ConnectedComponents =>
    //     println(s"""    cc: ${cc.toText} ${cc.bounds.prettyPrint} cc.right: ${cc.bounds.right}""")

    //   case cc: CharComponent =>
    //     println(s"""    c:  ${cc.toText} ${cc.bounds.prettyPrint} cc.right: ${cc.bounds.right}""")

    // }}
    val firstCC = linecc.components.head
    linecc.components.sliding(2).foreach{_ match {
      case Seq(c1, c2) =>
        val totalBBox = firstCC.bounds.union(c2.bounds)
        println(s"""| ${c1.toText} - ${c2.toText}
                    |    ${c1.bounds.prettyPrint} - ${c2.bounds.prettyPrint}
                    |    c1.left: ${c1.bounds.left} c1.right: ${c1.bounds.right} c2.left: ${c2.bounds.left}
                    |    dist = ${c2.bounds.left} - ${c1.bounds.right} = ${c2.bounds.left - c1.bounds.right}
                    |    totalBBox = ${totalBBox.prettyPrint}, bb.right:${totalBBox.right.pp}
                    |""".stripMargin)
      case Seq(c1) =>
    }}

  }
}



sealed trait Component {
  def chars: String

  def charComponents: Seq[CharComponent]

  def mapChars(subs: Seq[(Char, String)]): Component

  def tokenizeLine(): ConnectedComponents

  def toText(implicit idgen:Option[CCRenderState] = None): String

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
  def removeLabel(): Component
  def label: Option[Label]
  def containedLabels: Set[Label]

}

import Component._

case class CharComponent(
  component: CharBox,
  orientation: Double,
  blockRole: Option[Label] = None
) extends Component {

  def tokenizeLine(): ConnectedComponents = {
    Component(Seq(this))
  }

  def charComponents: Seq[CharComponent] = Seq(this)


  def mapChars(subs: Seq[(Char, String)]): Component  = {
    subs
      .find(_._1.toString==component.char)
      .map({case (_, sub) =>
        this.copy(component=component.copy(subs = sub))
      })
      .getOrElse(this)
  }

  val dx = component.bbox.width / 3
  val dy = dx * math.tan(orientation);

  lazy val x0 = centerX(component) - dx;
  lazy val x1 = centerX(component) + dx;
  lazy val y0 = centerY(component) - dy;
  lazy val y1 = centerY(component) + dy;
  val bounds = component.bbox
  def height: Double  = bounds.height

  def toText(implicit idgen:Option[CCRenderState] = None): String = component.char.toString
  def chars: String = toText

  val containedLabels: Set[Label] = blockRole.toSet

  val label: Option[Label] = blockRole
  def withLabel(l: Label): Component = {
    this.copy(blockRole = Option(l))
  }
  def removeLabel(): Component = {
    this.copy(blockRole = None)
  }
}

case class ConnectedComponents(
  components: Seq[Component],
  orientation: Double,
  blockRole: Option[Label] = None
  // label: Label = LB.Line
  // labels: Seq[Label] = Seq()
) extends Component {


  def mapChars(subs: Seq[(Char, String)]): Component  = {
    copy(
      components = components.map(_.mapChars(subs))
    )
  }

  def containedLabels: Set[Label] = {
    blockRole.toSet ++ (components
      .map(_.containedLabels)
      .reduce(_++_))
  }

  val label: Option[Label] = blockRole
  def withLabel(l: Label): Component = {
    this.copy(blockRole = Option(l))
  }
  def removeLabel(): Component = {
    this.copy(blockRole = None)
  }

  def chars:String = {
    components.map(_.chars).mkString
  }

  def charComponents: Seq[CharComponent] =
    components.flatMap(_.charComponents)

  def toText(implicit idgen:Option[CCRenderState] = None): String ={
    val ccs = renderConnectedComponents(this)
    TB.hcat(ccs).toString()
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


  def determineNormalTextBounds: LTBounds = {
    val mfHeights = getMostFrequentValues(components.map(_.bounds.height), 0.1d)
    val mfTops = getMostFrequentValues(components.map(_.bounds.top), 0.1d)

    val mfHeight= mfHeights.headOption.map(_._1).getOrElse(0d)
    val mfTop = mfTops.headOption.map(_._1).getOrElse(0d)

    components
      .map({ c =>
        val cb = c.bounds
        LTBounds(
          left=cb.left, top=mfTop,
          width=cb.width, height=mfHeight
        )
      })
      .foldLeft(components.head.bounds)( { case (b1, b2) =>
        b1 union b2
      })
  }



  // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)
  def determineSpacings(): Seq[Double] = {
    val dists = pairwiseSpaceWidths(components)
    val resolution = 0.5d

    val hist = histogram(dists, resolution)

    val spaceDists = hist.getFrequencies
      .sortBy(_.frequency)
      // .dropWhile(_.getFrequency==0)
      .map(_.value)
      .reverse

    spaceDists
  }

  def findCenterY(): Double = {
    components.map({c => c.bounds.toCenterPoint.y}).sum / components.length
  }

  def findCommonToplines(): Seq[Double] = {
    getMostFrequentValues(
      components.map({c => c.bounds.top}),
      0.001d
    ).toList.map(_._1)
  }

  def findCommonBaselines(): Seq[Double] = {
    getMostFrequentValues(
      components.map({c => c.bounds.bottom}),
      0.001d
    ).toList.map(_._1)
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

    val stats = components.zip(pairwiseSpaceWidths(components))
      .drop(range._1)
      .take(range._2).map({case (c, dist) =>
        (tbox(c.toText) +| "->" +| (dist.pp)) %
          c.bounds.top.pp %
          (c.bounds.left.pp +| c.bounds.right.pp) %
          (c.bounds.bottom.pp +| "(w:" +| c.bounds.width.pp)
      }).toList

    println(
      hsep(stats)
    )
  }

  def angleBasedSuperSubFinding(): Unit = {

    // start w/first modal-top char, search forward, then back
    // val supOrSubs = mutable.ArrayBuffer[Int]()
    // components
    //   .sliding(2).toList
    //   .zipWithIndex
    //   .foreach({
    //     case (Seq(c1), i)  =>

    //     case (Seq(c1, c2), i)  =>
    //       val c1west = c1.bounds.toWesternPoint
    //       val c2east = c2.bounds.toEasternPoint
    //       val c1c2Angle = c1west.angleTo(c2east)
    //       val c1IsAboveC2 = c1c2Angle > 0
    //       if (c1.bounds.top.eqFuzzy(0.01)(modalTop)) {
    //       }
    //       if (c.bounds.top > modalTop) {
    //         c.withLabel(LB.Sub)
    //       } else if (c.bounds.bottom < modalBottom) {
    //         c.withLabel(LB.Sup)
    //       } else {
    //         c
    //       }
    //   })

  }

  def tokenizeLine(): ConnectedComponents = {
    // println("tokenizeLine")
    val tops = findCommonToplines()
    val bottoms = findCommonBaselines()
    val modalTop = tops.head // - 0.01d
    val modalBottom = bottoms.head // + 0.01d

    val modalCenterY = (modalBottom + modalTop)/2
    val meanCenterY = findCenterY()

    // try using angles for super/subs

    // val searchLog = mutable.ArrayBuffer[TB.Box]()

    // label super/sub if char.ctr fall above/below centerline
    val supSubs = components.map({c =>
      val cctr = c.bounds.toCenterPoint
      if (cctr.y.eqFuzzy(0.3)(modalCenterY)) {
        c
      } else if (cctr.y > modalCenterY) {
        c.withLabel(LB.Sub)
      } else {
        c.withLabel(LB.Sup)
      }
    })

    def slurpUnlabeled(cs: Seq[Component]): (Seq[Component], Seq[Component]) = {
      val unLabeled = cs.takeWhile({_.label.isEmpty })
      (unLabeled, cs.drop(unLabeled.length))
    }
    def slurpLabels(l: Label, cs: Seq[Component]): (Seq[Component], Seq[Component]) = {
      val withLabel = cs.takeWhile({_.label.exists(_ == l) })
      (withLabel, cs.drop(withLabel.length))
    }

    val unconnected = mutable.ArrayBuffer[Component](supSubs:_*)
    val connectedSupSubs = mutable.ArrayBuffer[Component]()

    while (!unconnected.isEmpty) {
      { val (withL, _) = slurpUnlabeled(unconnected)
        if (!withL.isEmpty) {
          connectedSupSubs ++= withL
          unconnected.remove(0, withL.length)
        } }

      { val (withL, _) = slurpLabels(LB.Sub, unconnected)
        if (!withL.isEmpty) {
          connectedSupSubs += Component(withL.map(_.removeLabel), LB.Sub)
          unconnected.remove(0, withL.length)
        } }

      { val (withL, _) = slurpLabels(LB.Sup, unconnected)
        if (!withL.isEmpty) {
          connectedSupSubs += Component(withL.map(_.removeLabel), LB.Sup)
          unconnected.remove(0, withL.length)
        } }
    }



    val charDists = determineSpacings()
    val modalLittleGap = charDists.head
    val modalBigGap = charDists.drop(1).headOption.getOrElse(modalLittleGap)
    val splitValue = (modalBigGap+modalLittleGap)/2
    val splittable = charDists.length > 1

    // println(s"""|    top char dists: ${charDists.map(_.pp).mkString(", ")}
    //             |    modalTop = ${modalTop} modalBottom = ${modalBottom}
    //             |    modalCenter: ${modalCenterY} meanCenter: ${meanCenterY}
    //             |    modal little gap = ${modalLittleGap} modal big gap = ${modalBigGap}
    //             |    splitValue = ${splitValue}
    //             |""".stripMargin)

    // { import TB._
    //   // println(s"""tops: ${tops.map(_.pp).mkString(" ")}""")
    //   // println(s"""bottoms: ${bottoms.map(_.pp).mkString(" ")}""")
    //   val stats = components.zip(pairwiseSpaceWidths(components))
    //     .map({case (c, dist) =>
    //       (tbox(c.toText) +| "->" +| (dist.pp)) %
    //         c.bounds.top.pp %
    //         (c.bounds.left.pp +| c.bounds.right.pp) %
    //         (c.bounds.bottom.pp +| "(w:" + c.bounds.width.pp) + ")"
    //     }).toList

    //   searchLog.append(
    //     vcat(left)(stats)
    //   )
    // }

    val wordBreaks = mutable.ArrayBuffer[Int]()

    connectedSupSubs
      .zip(pairwiseSpaceWidths(connectedSupSubs))
      .sliding(2).toList
      .zipWithIndex
      .foreach({
        case (Seq((c2, _)), i)  =>
          // single component, no need to append any word breaks

        case (Seq((c1, d1), (c2, _)), i)  =>
          val dist = math.abs(c2.bounds.left - c1.bounds.right)

          if(splittable && d1*1.1 > splitValue) {
            wordBreaks.append(i)
          }

          val angleC1C2 = c1.bounds.toCenterPoint.angleTo(
            c2.bounds.toCenterPoint
          )

          val c1ctr = c1.bounds.toCenterPoint
          val c2ctr = c2.bounds.toCenterPoint

          val c1west = c1.bounds.toWesternPoint
          val c2east = c2.bounds.toEasternPoint
          val c1c2Angle = c1west.angleTo(c2east)


          val c12diff = c2ctr - c1ctr
          val checkAngle = Point(0, 0).angleTo(c12diff)

          // { import TB._
          //   val stats = s"${c1.chars}  -  ${c2.chars}" %
          //   s"    ${c1.bounds.prettyPrint}  -  ${c2.bounds.prettyPrint}" %
          //   s"    pairwisedist: ${d1}  asbdist: ${dist}" %
          //   s"    c1ctr: ${c1ctr.prettyPrint} c2ctr: ${c2ctr.prettyPrint} c12diff: ${c12diff} " %
          //   s"    c1 dist to modal Y (c1ctr.y-modalCenterY):${math.abs(c1ctr.y-modalCenterY)}" %
          //   s"    c1wst: ${c1west.prettyPrint} c2east: ${c2east.prettyPrint} angle: ${c1west.angleTo(c2east)} " %
          //   s"    c1-c2 angle: ${angleC1C2}   checkAngle: ${checkAngle}"

          //   searchLog.append(stats)
          // }
        case _  =>
          sys.error("why are we here? wtf??")
      })

    val asTokens = splitAtBreaks(wordBreaks, connectedSupSubs)
      .map(Component(_, LB.Token))

    // { import TB._
    //   println(
    //       vcat(top)(searchLog.toList)
    //   )
    // }

    Component(asTokens,  LB.TokenizedLine)
  }


}
