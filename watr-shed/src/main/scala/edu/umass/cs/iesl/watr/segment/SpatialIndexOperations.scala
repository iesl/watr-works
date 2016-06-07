package edu.umass.cs.iesl.watr

package segment

import watrmarks._

import scalaz.@@
import Bounds._

object SpatialIndexOperations {


  implicit class RicherZoneIndex(val index: ZoneIndexer) extends AnyVal {



    def approximateColumnBins(pageId: Int@@PageID, charBoxes: Seq[CharBox]): Seq[(CompassDirection, Line)] = {
      val leftBins = charBoxes
        .groupBy(_.bbox.left.pp)
        .toSeq
        .filter(_._2.length > 1)
        .sortBy(_._1.toDouble)


      val leftEdges = leftBins.map({ case (leftXstr, bin) =>
        val ysorted = bin.sortBy(_.bbox.bottom)
        val colbounds = charBoxesBounds(ysorted)
        val leftEdge = colbounds.toLine(CompassDirection.W)

        val query = LTBounds(
          leftEdge.p1.x, leftEdge.p1.y,
          5.0,
          leftEdge.p2.y - leftEdge.p1.y
        ).translate(-5.1, 0)

        val charsToLeft = index.queryCharsIntersects(pageId, query)

        println(s"ysorted = ${bin.map(_.prettyPrint).mkString(", ")}")
        println(s"charsToLeft = ${charsToLeft.map(_.prettyPrint).mkString(", ")}")


        val (splits, leftovers) = charsToLeft
          .sortBy(_.bbox.bottom)
          .foldLeft((Seq[Seq[CharBox]](), ysorted)) ({case ((split, remaining), e) =>
            val cbottom = e.bbox.bottom.pp

            val cleanEdge = remaining
              .takeWhile(ys => cbottom != ys.bbox.bottom.pp)

            val next = remaining.drop(cleanEdge.length)
              .dropWhile(ys => cbottom == ys.bbox.bottom.pp)

            (split :+ cleanEdge, next)
          })

        val allSplits = splits :+ leftovers

        println(s"""bin split into \n${splits.map(_.map(_.prettyPrint).mkString("")).mkString("\n")}""")

        val splitLines = allSplits
          .filter(_.length > 1)
          .map({ ss =>
            val sbounds = charBoxesBounds(ss)
            (CompassDirection.W, sbounds.toLine(CompassDirection.W))
          })


        splitLines
      })

      leftEdges.flatten
    }

  }

}
