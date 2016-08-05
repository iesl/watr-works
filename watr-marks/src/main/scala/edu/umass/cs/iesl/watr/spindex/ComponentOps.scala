package edu.umass.cs.iesl.watr
package spindex


import watrmarks.{StandardLabels => LB, Label}
// import scala.collection.mutable
import utils.Histogram
import utils.Histogram._
// import textboxing.{TextBoxing => TB}

import IndexShapeOperations._
import utils.SlicingAndDicing._
import utils.{CompassDirection => CDir}


object ComponentOperations {
  def centerX(cb: PageAtom) = cb.region.bbox.toCenterPoint.x
  def centerY(cb: PageAtom) = cb.region.bbox.toCenterPoint.y

  def spaceWidths(cs: Seq[CharAtom]): Seq[Double] = {
    // pairwiseSpaceWidths(cs.map(Component(_)))
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.region.bbox.left - c1.region.bbox.right
      case _  => 0d
    })

    dists :+ 0d
  }

  def pairwiseSpaceWidths(cs: Seq[Component]): Seq[Double] = {
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.bounds.left - c1.bounds.right
      case _  => 0d
    })

    dists :+ 0d
  }

  def determineCharSpacings(chars: Seq[CharAtom]): Seq[Double] = {
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

  // TODO remove
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

  implicit class RicherComponent(val component: Component) extends AnyVal {
    // component.zoneIndex

    def hasLabel(l: Label): Boolean = component.getLabels.contains(l)

    def vdist(other: Component): Double = {
      component.bounds.toPoint(CDir.W).vdist(
        other.bounds.toPoint(CDir.W)
      )
    }

    def columnContains(other: Component): Boolean = {
      val slopFactor = 4.5d // XXX test this magic number?

      val left = component.bounds.toWesternPoint.x
      val right = component.bounds.toEasternPoint.x

      val otherLeft = other.bounds.toWesternPoint.x + slopFactor
      val otherRight = other.bounds.toEasternPoint.x - slopFactor

      left <= otherLeft && otherRight <= right

    }

    def columnIntersects(other: Component): Boolean = {
      val slopFactor = 0.31d // XXX what is this magic number?

      val otherx0 = other.bounds.toWesternPoint.x-slopFactor
      val otherx1 = other.bounds.toEasternPoint.x+slopFactor
      val candx0 = component.bounds.toWesternPoint.x
      val candx1 = component.bounds.toEasternPoint.x
      val candRightInside = otherx0 <= candx1 && candx1 <= otherx1
      val candLeftOutside = candx0 < otherx0
      val candLeftInside = otherx0 <= candx0 && candx0 <= otherx1
      val candRightOutside = otherx1 < candx1

      val crossesLeft = candRightInside && candLeftOutside
      val crossesRight = candLeftInside && candRightOutside


      crossesLeft || crossesRight
    }

    def isOverlappedVertically(other: Component): Boolean = {
      !(component.isStrictlyAbove(other) || component.isStrictlyBelow(other))
    }

    def isStrictlyAbove(other: Component): Boolean = {
      val y1 = component.bounds.toPoint(CDir.S).y
      val y2 = other.bounds.toPoint(CDir.N).y
      y1 < y2
    }
    def isStrictlyBelow(other: Component): Boolean = {
      val y1 = component.bounds.toPoint(CDir.N).y
      val y2 = other.bounds.toPoint(CDir.S).y
      y1 > y2
    }

    def isStrictlyLeftOf(other: Component): Boolean = {
      val rightEdge = component.bounds.toEasternPoint.x
      val otherLeftEdge = other.bounds.toWesternPoint.x
      rightEdge < otherLeftEdge
    }

    def isStrictlyRightOf(other: Component): Boolean = {
      val leftEdge = component.bounds.toEasternPoint.x
      val otherRightEdge = other.bounds.toWesternPoint.x
      otherRightEdge < leftEdge
    }

    def candidateIsOutsideLineBounds(other: Component): Boolean = {
      component.isStrictlyLeftOf(other) || component.isStrictlyRightOf(other)
    }




    def isBelow(other: Component) = component.bounds.top > other.bounds.top
    def isAbove(other: Component) = component.bounds.top < other.bounds.top

    def hasSameVCenterPoint(tolerance: Double=0.1)(other: Component) =
      component.bounds.toCenterPoint.x.eqFuzzy(tolerance)(other.bounds.toCenterPoint.x)

    def hasSameLeftEdge(tolerance: Double=0.3)(other: Component) =
      component.bounds.toPoint(CDir.W).x.eqFuzzy(tolerance)(other.bounds.toPoint(CDir.W).x)

    def isEqualWidth(tolerance: Double=0.1)(other: Component) =
      component.bounds.width.eqFuzzy(tolerance)(other.bounds.width)


    def zoneIndex = component.zoneIndex

    def findCommonToplines(): Seq[Double] = {
      Histogram.getMostFrequentValues(
        component.children.map({c => c.bounds.top}),
        0.001d
      ).toList.map(_._1)
    }

    def findCommonBaselines(): Seq[Double] = {
      Histogram.getMostFrequentValues(
        component.children.map({c => c.bounds.bottom}),
        0.001d
      ).toList.map(_._1)
    }
    // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)
    def determineSpacings(): Seq[Double] = {
      val dists = pairwiseSpaceWidths(component.children)
      val resolution = 0.5d

      val hist = Histogram.histogram(dists, resolution)

      val spaceDists = hist.getFrequencies
        .sortBy(_.frequency)
      // .dropWhile(_.getFrequency==0)
        .map(_.value)
        .reverse

      spaceDists
    }


    def vtrace = component.zoneIndex.vtrace


    def tokenizeLine(): Component = {
      if (!component.getLabels.contains(LB.TokenizedLine)) {

        vtrace.trace(
          vtrace.begin("Tokenize Line"),
          vtrace.focusOn(component.targetRegion),
          vtrace.all(component.children.map(c => vtrace.showRegion(c.targetRegion)))
        )

        val tops = findCommonToplines()
        val bottoms = findCommonBaselines()
        val modalTop = tops.head // - 0.01d

        val modalBottom = bottoms.head // + 0.01d


        val modalCenterY = (modalBottom + modalTop)/2
        // val meanCenterY = component.characteristicLine.centerPoint.y

        vtrace.trace(
          vtrace.link(vtrace.message("modal top"), vtrace.hRuler(modalTop)),
          vtrace.link(vtrace.message("modal bottom"), vtrace.hRuler(modalBottom)),
          vtrace.link(vtrace.message("modal center Y"), vtrace.hRuler(modalCenterY))
        )


        // label individual chars as super/sub if char.ctr fall above/below centerline
        val supSubs = component.children.map({c =>
          val cctr = c.bounds.toCenterPoint
          val cbottom = c.bounds.bottom
          val supSubTolerance = component.bounds.height / 10.0

          vtrace.trace(vtrace.link(
            vtrace.message("char ctr"),
            vtrace.all(Seq(vtrace.hRuler(cctr.y), vtrace.vRuler(cctr.x)))
          ))

          val maybeLabel: Option[Label] =
            if (c.bounds.top < modalTop && c.bounds.bottom > modalBottom) {
              // if our child's top/bottom extends beyond modal top/bottom, it is a larger font and not super/sub
              None
            } else if (c.bounds.bottom.eqFuzzy(supSubTolerance)(modalBottom)) {
              None
            } else if (cctr.y < modalCenterY) {
              // println(s"""Line: ${component.chars}""")
              // println(s"""(sub)  ${c.chars}>  cctr.toCenterPoint: ${cctr.prettyPrint} modalCenterY: ${modalCenterY}""")
              // println(s"""modal bottom: ${modalBottom}, c.bottom = ${cbottom}""")
              LB.Sup.some
            } else {
              // println(s"""Line: ${component.chars}""")
              // println(s"""(sup)  ${c.chars}>  cctr.toCenterPoint: ${cctr.prettyPrint} modalCenterY: ${modalCenterY}""")
              // println(s"""modal bottom: ${modalBottom}, c.bottom = ${cbottom}""")
              LB.Sub.some
            }

          maybeLabel.foreach { c.addLabel(_) }
          c
        })


        def isLabelBoundary(l: Label, a: Component, b: Component): Boolean = {
          ((a.hasLabel(l) && !b.hasLabel(l)) ||
            (!a.hasLabel(l) && b.hasLabel(l)))
        }

        def concatLabels(l: Label, cs: Seq[Component]): Seq[Component] = {
          cs.headOption.map({ c0 =>
            if (c0.hasLabel(l)) {
              cs.foreach(_.removeLabel(l))
              Seq(zoneIndex.concatComponents(cs, l))
            } else { cs }
          }).getOrElse(cs)
        }

        val supSubGroups = supSubs.splitOnPairs { (ca, cb) =>
          isLabelBoundary(LB.Sup, ca, cb) || isLabelBoundary(LB.Sub, ca, cb)
        }

        val connectedSupSubs = (for {
          connSpan <- supSubGroups
        } yield {
          connSpan |>
            (concatLabels(LB.Sup, _)) |>
            (concatLabels(LB.Sub, _))
        }).flatten


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

        val tokenSpans = (connectedSupSubs
          .zip(pairwiseSpaceWidths(connectedSupSubs)))
          .splitOnPairs({ case ((c1, d1), (c2, d2)) =>
            val dist = math.abs(c2.bounds.left - c1.bounds.right)

            // val effectiveDist = d1*1.1
            val effectiveDist = dist

            // val stats =
            //   s"""|${c1.chars} -- ${c2.chars}
            //       |   ${c1.bounds.prettyPrint}  -  ${c2.bounds.prettyPrint}
            //       |                 ${c1.bounds.top.pp}
            //       |      ${c1.bounds.left.pp}    ${c1.bounds.right.pp})  (w= ${c1.bounds.width.pp})
            //       |            ${c1.bounds.bottom.pp}
            //       |
            //       |   pairwisedist: ${d1}  east-west dist: ${dist} effective dist: ${effectiveDist}
            //       |   split value: ${splitValue}, splittable? ${splittable}
            //       |""".stripMargin

            // println(stats)


            splittable && effectiveDist > splitValue

          })


        val asTokens = tokenSpans.map(_.map(_._1))
          .map({cs => zoneIndex.concatComponents(cs, LB.Token) })

        // println(s"    # of tokens in line = ${asTokens.length}")

        component.replaceChildren(asTokens)
        component.addLabel(LB.TokenizedLine)

        vtrace.trace(vtrace.end("Tokenize Line"))

      }
      component

    }

  }

}
