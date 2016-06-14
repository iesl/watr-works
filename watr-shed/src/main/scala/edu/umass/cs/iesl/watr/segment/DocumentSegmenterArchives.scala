
// println("starting wiht lines: ")
// pageLinesx.foreach { line =>
//   val pstr = Component.renderConnectedComponents(line)
//   println(s"""  >> ${pstr}""")
// }

// println("--------------")
// println("binned lines for page")
// lineBins.widthBin.foreach { case ((width, wfreq), binnedLines) =>
//   println(s"   bin = ${width}, freq = ${wfreq}")
//   binnedLines.sortBy(_.bounds.top).foreach { bl =>
//     val pstr = Component.renderConnectedComponents(bl)
//     println(s"""    >> ${pstr}""")
//   }
//   println("--------------")
// }

// println("unbinned lines for page")
// lineBins.unBinned.sortBy(_.bounds.top).foreach { ubl =>
//   val pstr = Component.renderConnectedComponents(ubl)
//   println(s"""    >> ${pstr}""")
// }

// println(s" building cols out of lines of width: ${mostFrequentWidthDocwide} w/freq: ${wfreq}")

// val ((mostFrequentWidthDocwide, wfreq), linesWFreq)  = lineBins.widthBin.sortBy(_._1._2).reverse.head

// val candidateLinesBelow = possibleCand
//   .filter(candidateIsBelowBottom(_))
//   .filterNot(candidateIsOutsideLineBounds(_, topLine))
//   .takeWhile({cc =>
//                val colBreak = candidateCrossesLineBounds(cc, topLine)
//                // println(s"""| checking for below bottom line:
//                //             | cand: ${debugFormatLine(cc)}
//                //             | top : ${debugFormatLine(topLine)}
//                //             | breaks col bounds ${colBreak}
//                //             |""".stripMargin)
//                  !colBreak
//              })
// println(s"""| checking for above topline:
//             | cand: ${debugFormatLine(cc)}
//             | top : ${debugFormatLine(topLine)}
//             | breaks col bounds ${colBreak}
//             |""".stripMargin)



// println("examining lines: ")
// colLines.foreach { line =>
//   val pstr = Component.renderConnectedComponents(line)
//   // println(s"""  >> ${pstr}""")
//   println(s"""  >> ${line.chars}""")
// }

// println("--------------")
// println(s"found ${candidateLinesWithin.length} lines within")

// val candidateLinesWithin = possibleCand.filter({cc =>
//   // val fmt=debugFormatLine(cc)
//   // println(s"""| checking for within ${fmt}
//   //             | top: ${topLine.bounds.prettyPrint} / r: ${topLine.bounds.right}
//   //             | bottom: ${bottomLine.bounds.prettyPrint} / r: ${bottomLine.bounds.right}
//   //             |""".stripMargin)
//   topLine.bounds.top < cc.bounds.top && cc.bounds.top < bottomLine.bounds.top
// })

// val debugAboveLines = candidateLinesAbove.map({ cc=> renderConnectedComponents(cc) }).mkString("\n")
// val debugWithin = candidateLinesWithin.map({ cc=> renderConnectedComponents(cc) }).mkString("\n")
// val debugMiddle = ySortedLines.map({ cc=> renderConnectedComponents(cc) }).mkString("\n")
// val debugBelowLines = candidateLinesBelow.map({ cc=> renderConnectedComponents(cc) }).mkString("\n")
// println(s"Candidates Above\n${debugAboveLines}\n")
// println(s"Candidates Within\n${debugWithin}\n")
// println(s"\n\n${debugMiddle}\n\n")
// println(s"Candidates below\n${debugBelowLines}\n")

// val candb = debugFormatLine(cand)
// val lineb = debugFormatLine(line)
// debugReport(
//   candb,
//   lineb,
//   linex0           , // = line.bounds.toWesternPoint.x
//   linex1           , // = line.bounds.toEasternPoint.x
//   candx0           , // = cand.bounds.toWesternPoint.x
//   candx1           , // = cand.bounds.toEasternPoint.x
//   candRightInside  , // = linex0 <= candx1 && candx1 <= linex1
//   candLeftOutside  , // = candx0 < linex0
//   candLeftInside   , // = linex0 <= candx0 && candx0 <= linex1
//   candRightOutside , // = linex1 > candx1
//   crossesLeft        , // = candRightInside && candLeftOutside
//   crossesRight,     // = candLeftInside && candRightOutside
//   crossesLeft || crossesRight
// )


  // /** Groups components into text lines. */
  // def determineLinesNNSearchVersion(
  //   pageId: Int@@PageID,
  //   components: Seq[CharRegion]
  // ): Seq[ConnectedComponents] = {

  //   val readableComponents = components.filterNot(_.isWonky)
  //   val sets = new DisjointSets[CharRegion](readableComponents)


  //   for { component <- readableComponents.sortBy(_.region.bbox.left) } {
  //     // val searchLog = mutable.ArrayBuffer[TB.Box]()
  //     findNeighbors(pageId, component)
  //       .foreach({neighbor =>
  //         val angle = component.region.bbox.toCenterPoint.angleTo(neighbor.region.bbox.toCenterPoint)

  //         val maxWidth = math.max(neighbor.region.bbox.width, component.region.bbox.width)
  //         val dx = neighbor.region.bbox.toCenterPoint.hdist(component.region.bbox.toCenterPoint)
  //         val dy = neighbor.region.bbox.toCenterPoint.vdist(component.region.bbox.toCenterPoint)
  //         val dist = neighbor.region.bbox.toCenterPoint.dist(component.region.bbox.toCenterPoint)

  //         val eastWestDist = component.region.bbox.toEasternPoint.dist(
  //           neighbor.region.bbox.toWesternPoint
  //         )

  //         var joinWith = false
  //         // val maxWidthMult = 2.7
  //         val maxHWidthMult = 2.7
  //         val maxAngleWidthMult = 1.0

  //         if (angle.eqFuzzy(0.01)(0.0) && eastWestDist < maxWidth*maxHWidthMult) {
  //           sets.union(component, neighbor);
  //           joinWith = true
  //         } else if (withinAngle(angle) && dist < maxWidth*maxAngleWidthMult) {
  //           sets.union(component, neighbor);
  //           joinWith = true
  //         }

  //         // { import TB._
  //         //   val topsNotEq = component.region.bbox.top.pp != neighbor.region.bbox.top.pp
  //         //   val angleNotZero = angle.pp != "0.00"
  //         //   searchLog.append(
  //         //     s"   '${neighbor.char}' ${neighbor.wonkyCharCode} #${neighbor.id} ${neighbor.region.bbox.prettyPrint}".box %
  //         //       s"""       ${ if (joinWith && topsNotEq) "!join" else if (joinWith) "join" else "" }""" %
  //         //       s"       angle:${angle.pp} dx:${dx.pp} dy:${dy.pp}" %
  //         //       s"       dist:${dist.pp} e/wi-dist:${eastWestDist.pp}" %
  //         //       s"       maxwidth= ${maxWidth} withinAngle=${withinAngle(angle)}"
  //         //   )
  //         // }
  //       })

  //     // { import TB._
  //     //   println(
  //     //     s"'${component.char} #${component.id} ${component.region.bbox.prettyPrint}".box %
  //     //     vcat(top)(searchLog.toList)
  //     //   )
  //     // }

  //   }

  //   val lines = sets.iterator().toSeq.map{
  //     _.toSeq.sortBy(c => (c.region.bbox.left, c.region.bbox.top)).map(new PageComponent(_, docOrientation))
  //   }


  //   lines
  //     .map{ Component(_, LB.VisualLine) }
  //     .sortBy(b => (b.bounds.top, b.bounds.left))
  // }
