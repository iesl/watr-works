package edu.umass.cs.iesl.watr
package segment


import spindex._

object SpatialIndexOperations {


  implicit class RicherZoneIndex(val mpageIndex: MultiPageIndex) extends AnyVal {

  //   def approximateColumnBins(pageId: Int@@PageID, charBoxes: Seq[CharAtom]): Seq[(CompassDirection, Line)] = {
  //     val leftBins = charBoxes
  //       .groupBy(_.targetRegion.bbox.left.pp)
  //       .toSeq
  //       .filter(_._2.length > 1)
  //       .sortBy(_._1.toDouble)


  //     val leftEdges = leftBins.map({ case (leftXstr, bin) =>
  //       val ysorted = bin.sortBy(_.targetRegion.bbox.bottom)
  //       val colbounds = charBoxesBounds(ysorted)
  //       val leftEdge = colbounds.toLine(CompassDirection.W)

  //       val query = LTBounds(
  //         leftEdge.p1.x, leftEdge.p1.y,
  //         5.0,
  //         leftEdge.p2.y - leftEdge.p1.y
  //       ).translate(-5.1, 0)

  //       val charsToLeft = mpageIndex.getPageIndex(pageId).componentIndex.queryForIntersects(query)

  //       val (splits, leftovers) = charsToLeft
  //         .sortBy(_.targetRegion.bbox.bottom)
  //         .foldLeft((Seq[Seq[CharAtom]](), ysorted)) ({case ((split, remaining), e) =>
  //           val cbottom = e.bounds.bottom.pp

  //           val cleanEdge = remaining
  //             .takeWhile(ys => cbottom != ys.targetRegion.bbox.bottom.pp)

  //           val next = remaining.drop(cleanEdge.length)
  //             .dropWhile(ys => cbottom == ys.targetRegion.bbox.bottom.pp)

  //           (split :+ cleanEdge, next)
  //         })

  //       val allSplits = splits :+ leftovers

  //       println(s"""bin split into \n${splits.map(_.map(_.prettyPrint).mkString("")).mkString("\n")}""")

  //       val splitLines = allSplits
  //         .filter(_.length > 1)
  //         .map({ ss =>
  //           val sbounds = charBoxesBounds(ss)
  //           (CompassDirection.W, sbounds.toLine(CompassDirection.W))
  //         })


  //       splitLines
  //     })

  //     leftEdges.flatten
  //   }

  }

}
