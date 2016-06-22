package edu.umass.cs.iesl.watr
package spindex

      // val charDists = determineSpacings()
      // val modalLittleGap = charDists.head
      // val modalBigGap = charDists.drop(1).headOption.getOrElse(modalLittleGap)
      // val splitValue = (modalBigGap+modalLittleGap)/2
      // val splittable = charDists.length > 1

      // // println(s"""|    top char dists: ${charDists.map(_.pp).mkString(", ")}
      // //             |    modalTop = ${modalTop} modalBottom = ${modalBottom}
      // //             |    modalCenter: ${modalCenterY} meanCenter: ${meanCenterY}
      // //             |    modal little gap = ${modalLittleGap} modal big gap = ${modalBigGap}
      // //             |    splitValue = ${splitValue}
      // //             |""".stripMargin)

      // // { import TB._
      // //   // println(s"""tops: ${tops.map(_.pp).mkString(" ")}""")
      // //   // println(s"""bottoms: ${bottoms.map(_.pp).mkString(" ")}""")
      // //   val stats = components.zip(pairwiseSpaceWidths(components))
      // //     .map({case (c, dist) =>
      // //       (tbox(c.toText) +| "->" +| (dist.pp)) %
      // //         c.bounds.top.pp %
      // //         (c.bounds.left.pp +| c.bounds.right.pp) %
      // //         (c.bounds.bottom.pp +| "(w:" + c.bounds.width.pp) + ")"
      // //     }).toList

      // //   searchLog.append(
      // //     vcat(left)(stats)
      // //   )
      // // }


      // val tokenSpans = (connectedSupSubs
      //   .zip(pairwiseSpaceWidths(connectedSupSubs)))
      //   .splitOnPairs({ case ((c1, d1), (c2, d2)) =>
      //     val dist = math.abs(c2.bounds.left - c1.bounds.right)

      //     splittable && d1*1.1 > splitValue

      //     // val angleC1C2 = c1.bounds.toCenterPoint.angleTo(
      //     //   c2.bounds.toCenterPoint
      //     // )
      //     // val c1ctr = c1.bounds.toCenterPoint
      //     // val c2ctr = c2.bounds.toCenterPoint
      //     // val c1west = c1.bounds.toWesternPoint
      //     // val c2east = c2.bounds.toEasternPoint
      //     // val c1c2Angle = c1west.angleTo(c2east)
      //     // val c12diff = c2ctr - c1ctr
      //     // val checkAngle = Point(0, 0).angleTo(c12diff)
      //     // { import TB._
      //     //   val stats = s"${c1.chars}  -  ${c2.chars}" %
      //     //   s"    ${c1.bounds.prettyPrint}  -  ${c2.bounds.prettyPrint}" %
      //     //   s"    pairwisedist: ${d1}  asbdist: ${dist}" %
      //     //   s"    c1ctr: ${c1ctr.prettyPrint} c2ctr: ${c2ctr.prettyPrint} c12diff: ${c12diff} " %
      //     //   s"    c1 dist to modal Y (c1ctr.y-modalCenterY):${math.abs(c1ctr.y-modalCenterY)}" %
      //     //   s"    c1wst: ${c1west.prettyPrint} c2east: ${c2east.prettyPrint} angle: ${c1west.angleTo(c2east)} " %
      //     //   s"    c1-c2 angle: ${angleC1C2}   checkAngle: ${checkAngle}"
      //     //   searchLog.append(stats)
      //     // }


      //   })
// val asTokens = tokenSpans.map(_.map(_._1))
//   .map({cs => zoneIndex.concatComponents(cs, LB.Token) })


// // { import TB._
// //   println(vcat(top)(searchLog.toList)   )
// // }

// zoneIndex.concatComponents(asTokens, LB.TokenizedLine)

        // def slurpUnlabeled(cs: Seq[Component]): (Seq[Component], Seq[Component]) = {
        //   val unLabeled = cs.takeWhile({ _.getLabels.isEmpty })
        //   (unLabeled, cs.drop(unLabeled.length))
        // }

        // def slurpLabels(l: Label, cs: Seq[Component]): (Seq[Component], Seq[Component]) = {
        //   val withLabel = cs.takeWhile(_.getLabels contains l)
        //   (withLabel, cs.drop(withLabel.length))
        // }

        // val unconnected = mutable.ArrayBuffer[Component](supSubs:_*)
        // val connectedSupSubs = mutable.ArrayBuffer[Component]()

        // while (!unconnected.isEmpty) {
        //   { val (withL, _) = slurpUnlabeled(unconnected)
        //     if (!withL.isEmpty) {
        //       connectedSupSubs ++= withL
        //       unconnected.remove(0, withL.length)
        //     } }

        //   { val (withL, _) = slurpLabels(LB.Sub, unconnected)
        //     if (!withL.isEmpty) {
        //       val connected = withL
        //         .map(_.removeLabel(LB.Sub))

        //       val c0 = zoneIndex.concatComponents(connected, LB.Sub)

        //       connectedSupSubs += c0
        //       unconnected.remove(0, withL.length)
        //     } }

        //   { val (withL, _) = slurpLabels(LB.Sup, unconnected)
        //     if (!withL.isEmpty) {
        //       val connected = withL
        //         .map(_.removeLabel(LB.Sup))
        //       val c0 = zoneIndex.concatComponents(connected, LB.Sup)
        //       connectedSupSubs += c0
        //       unconnected.remove(0, withL.length)
        //     } }
        // }

