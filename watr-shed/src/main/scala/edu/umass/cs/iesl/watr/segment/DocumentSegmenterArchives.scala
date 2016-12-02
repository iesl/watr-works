
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
  //   components: Seq[CharAtom]
  // ): Seq[ConnectedComponents] = {

  //   val readableComponents = components.filterNot(_.isWonky)
  //   val sets = new DisjointSets[CharAtom](readableComponents)


  //   for { component <- readableComponents.sortBy(_.targetRegion.bbox.left) } {
  //     // val searchLog = mutable.ArrayBuffer[TB.Box]()
  //     findNeighbors(pageId, component)
  //       .foreach({neighbor =>
  //         val angle = component.targetRegion.bbox.toCenterPoint.angleTo(neighbor.targetRegion.bbox.toCenterPoint)

  //         val maxWidth = math.max(neighbor.targetRegion.bbox.width, component.targetRegion.bbox.width)
  //         val dx = neighbor.targetRegion.bbox.toCenterPoint.hdist(component.targetRegion.bbox.toCenterPoint)
  //         val dy = neighbor.targetRegion.bbox.toCenterPoint.vdist(component.targetRegion.bbox.toCenterPoint)
  //         val dist = neighbor.targetRegion.bbox.toCenterPoint.dist(component.targetRegion.bbox.toCenterPoint)

  //         val eastWestDist = component.targetRegion.bbox.toEasternPoint.dist(
  //           neighbor.targetRegion.bbox.toWesternPoint
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
  //         //   val topsNotEq = component.targetRegion.bbox.top.pp != neighbor.targetRegion.bbox.top.pp
  //         //   val angleNotZero = angle.pp != "0.00"
  //         //   searchLog.append(
  //         //     s"   '${neighbor.char}' ${neighbor.wonkyCharCode} #${neighbor.id} ${neighbor.targetRegion.bbox.prettyPrint}".box %
  //         //       s"""       ${ if (joinWith && topsNotEq) "!join" else if (joinWith) "join" else "" }""" %
  //         //       s"       angle:${angle.pp} dx:${dx.pp} dy:${dy.pp}" %
  //         //       s"       dist:${dist.pp} e/wi-dist:${eastWestDist.pp}" %
  //         //       s"       maxwidth= ${maxWidth} withinAngle=${withinAngle(angle)}"
  //         //   )
  //         // }
  //       })

  //     // { import TB._
  //     //   println(
  //     //     s"'${component.char} #${component.id} ${component.targetRegion.bbox.prettyPrint}".box %
  //     //     vcat(top)(searchLog.toList)
  //     //   )
  //     // }

  //   }

  //   val lines = sets.iterator().toSeq.map{
  //     _.toSeq.sortBy(c => (c.targetRegion.bbox.left, c.targetRegion.bbox.top)).map(new AtomicComponent(_, docOrientation))
  //   }


  //   lines
  //     .map{ Component(_, LB.VisualLine) }
  //     .sortBy(b => (b.bounds.top, b.bounds.left))
  // }

  // def groupPageTextBlocks(
  //   pageId: Int@@PageID
  // ): Unit = {

  //   val pageLines: Seq[Component] = visualLineOnPageComponents(pageId.unwrap)

  //   val pageBounds = charBasedPageBounds(pageId)
  //   val pageCenter = pageBounds.toCenterPoint

  //   val lineBins = pageSegAccum.lineDimensionBins.find(_.page == pageId).get

  //   val unusedPageLines = mutable.ArrayBuffer[Component](pageLines:_*)
  //   val usedPageLines = mutable.ArrayBuffer[Component]()

  //   // starting w/most common width, down to least common..
  //   val allBlocks = lineBins.widthBin.sortBy(_._1._2).reverse.map {
  //     case ((mostFrequentWidthDocwide, wfreq), linesWithFreq) =>

  //       val remainingLinesWithFreq = linesWithFreq.diff(usedPageLines)

  //       if (remainingLinesWithFreq.isEmpty) Seq() else {

  //         // divide page-specific most frequent lines into likely columns:
  //         val colCenters = getMostFrequentValues(remainingLinesWithFreq.map(_.bounds.toCenterPoint.x) , resolution=0.2d)

  //         val commonLinesInCols = for {
  //           (colX, cfreq) <- colCenters
  //         } yield {
  //           val candidateLines = remainingLinesWithFreq.filter({ line => line.bounds.toCenterPoint.x.eqFuzzy(0.4)(colX) })
  //           val visBlocks = groupVisualTextBlocks(colX, candidateLines, unusedPageLines)
  //           visBlocks.foreach { vblock =>
  //             zoneIndexer.connectComponents(vblock, LB.TextBlock)
  //             unusedPageLines --= vblock
  //           }
  //         }
  //       }

  //     case _ => Seq()
  //   }

  //   if (unusedPageLines.length >0 ) {
  //     println(s"""Error: Unused page lines in text line grouping""")
  //   }
  // }


  // def sortZonesYX(zones: Seq[Component]): Seq[Component]= {

  //   zones.sortWith({case (cc1, cc2) =>
  //     val ycmp = compareDouble(cc1.bounds.top, cc2.bounds.top, 0.01)

  //     val cmp = if (ycmp == 0) {
  //       compareDouble(cc1.bounds.left, cc2.bounds.left, 0.01)
  //     } else {
  //       ycmp
  //     }

  //     cmp < 0
  //   })
  // }





  // def groupVisualTextRects(colX: Double, textRectCandidates: Seq[Component], remainingPageLines: Seq[Component]): Unit = {
  //   val unusedLines = mutable.ArrayBuffer[Component](remainingPageLines:_*)
  //   val usedLines = mutable.ArrayBuffer[Component]()

  //   val ySortedLines = textRectCandidates.sortBy(_.bounds.top)
  //   val topLine = ySortedLines.head
  //   val bottomLine = ySortedLines.last


  //   def candidateIsBelowBottom(cand: Component) = cand.bounds.top > bottomLine.bounds.top
  //   def candidateIsBelowTop(cand: Component) = cand.bounds.top > topLine.bounds.top
  //   def candidateIsAboveBottom(cand: Component) = cand.bounds.top < bottomLine.bounds.top
  //   def candidateIsAboveTop(cand: Component) = cand.bounds.top < topLine.bounds.top


  //   val possibleCand = unusedLines
  //     .diff(ySortedLines)
  //     .sortBy(_.bounds.top)

  //   val candidateLinesAbove = possibleCand
  //     .reverse
  //     .filter(candidateIsAboveTop(_))
  //     .filterNot(candidateIsOutsideLineBounds(_, topLine))
  //     .takeWhile({cc =>
  //       val colBreak = candidateCrossesLineBounds(cc, topLine)

  //       !colBreak
  //     })


  //   val candidateLinesBelow = possibleCand
  //     .filter(candidateIsBelowBottom(_))
  //     .filterNot(candidateIsOutsideLineBounds(_, topLine))
  //     .takeWhile({cc =>
  //       val colBreak = candidateCrossesLineBounds(cc, topLine)
  //         !colBreak
  //     })


  //   val candidateLinesWithin = possibleCand
  //     .filter(c =>candidateIsAboveBottom(c) && candidateIsBelowTop(c))
  //     .filterNot(candidateIsOutsideLineBounds(_, topLine))
  //     .filterNot(candidateCrossesLineBounds(_, topLine))


  //   val totalLines =  candidateLinesAbove ++ ySortedLines ++ candidateLinesWithin ++ candidateLinesBelow
  //   val totalLineSorted = totalLines.sortBy(_.bounds.top)

  //   unusedLines --= totalLineSorted
  //   usedLines ++= totalLineSorted

  //   zoneIndexer.connectComponents(totalLineSorted, LB.Block)
  // }
