package edu.umass.cs.iesl.watr
package spindex


import watrmarks.{StandardLabels => LB, Label}
import scala.collection.mutable
import utils.Histogram
import utils.Histogram._

import IndexShapeOperations._

object ComponentOperations {
  def centerX(cb: PageRegion) = cb.region.bbox.toCenterPoint.x
  def centerY(cb: PageRegion) = cb.region.bbox.toCenterPoint.y

  def spaceWidths(cs: Seq[CharRegion]): Seq[Double] = {
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

  def determineCharSpacings(chars: Seq[CharRegion]): Seq[Double] = {
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

    def tokenizeLine(): Component = {
      // println("tokenizeLine")
      val tops = findCommonToplines()
      val bottoms = findCommonBaselines()
      val modalTop = tops.head // - 0.01d
      val modalBottom = bottoms.head // + 0.01d

      val modalCenterY = (modalBottom + modalTop)/2
      val meanCenterY = component.characteristicLine.centerPoint.y


      // val searchLog = mutable.ArrayBuffer[TB.Box]()


      // label individual chars as super/sub if char.ctr fall above/below centerline
      val supSubs = component.children.map({c =>
        val cctr = c.bounds.toCenterPoint
        if (cctr.y.eqFuzzy(0.3)(modalCenterY)) {
          c
        } else if (cctr.y > modalCenterY) {
          c.addLabel(LB.Sub)
        } else {
          c.addLabel(LB.Sup)
        }
      })

      def slurpUnlabeled(cs: Seq[Component]): (Seq[Component], Seq[Component]) = {
        val unLabeled = cs.takeWhile({ _.getLabels.isEmpty })
        (unLabeled, cs.drop(unLabeled.length))
      }

      def slurpLabels(l: Label, cs: Seq[Component]): (Seq[Component], Seq[Component]) = {
        val withLabel = cs.takeWhile(_.getLabels contains l)
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
            val connected = withL
              .map(_.removeLabel(LB.Sub))

            val c0 = zoneIndex.concatComponents(connected, LB.Sub)

            connectedSupSubs += c0
            unconnected.remove(0, withL.length)
          } }

        { val (withL, _) = slurpLabels(LB.Sup, unconnected)
          if (!withL.isEmpty) {
            val connected = withL
              .map(_.removeLabel(LB.Sup))
            val c0 = zoneIndex.concatComponents(connected, LB.Sup)
            connectedSupSubs += c0 
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
        .map({cs => zoneIndex.concatComponents(cs, LB.Token) })


      // { import TB._
      //   println(
      //       vcat(top)(searchLog.toList)
      //   )
      // }

      zoneIndex.concatComponents(asTokens, LB.TokenizedLine)
    }

  }

}
