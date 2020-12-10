package org.watrworks
package utils

import ExactFloats._

import scala.{ collection => sc }
import sc.Seq

object QuickNearestNeighbors {
  import TypeTags._
  import utils.SlicingAndDicing._
  import scala.annotation.tailrec

  case class Bin(
    centroid: DataPoint,
    neighbors: Seq[DataPoint]
  ) {

    def size(): Int = neighbors.map(_.len).sum + centroid.len

    def range(): (Int@@FloatRep, Int@@FloatRep) = {
      val values = (centroid.value.unwrap +:
        neighbors.map(_.value.unwrap))

      (FloatRep(values.min), FloatRep(values.max))
    }

    def maxValue(): Int@@FloatRep = {
      val max = (centroid.value.unwrap +:
        neighbors.map(_.value.unwrap)).max

      FloatRep(max)
    }
    def minValue(): Int@@FloatRep = {
      val min = (centroid.value.unwrap +:
        neighbors.map(_.value.unwrap)).min

      FloatRep(min)
    }

    override def toString(): String = {
      val cstr = centroid.toString()
      val nstr = neighbors.map(_.toString()).mkString(", ")
      s"{size=${size()}: ${cstr} + [ ${nstr} ]}"
    }

    def toCentroidRangeString(): String = {
      val r = range()
      s"{n=${size()}: (${r._1.pp()}-${r._2.pp()})}"
    }

  }

  case class DataPoint(value: Int@@FloatRep, len: Int) {
    override def toString(): String = {
      val cpp = value.pp()
      val clen = len
      s"[${cpp} x ${clen}]"
    }

  }

  def qnn(in: Seq[Int@@FloatRep], tolerance: Double = 0.5d): Seq[Bin] = {

    @tailrec
    def loop(init: List[(Int@@FloatRep, Int)], groups: List[Bin]): List[Bin] = {
      if (init.isEmpty) groups else {
        val (dmax, dmaxCount) = init.head
        val (grouped, others) = init.tail.partition { case (d, dcount) =>
          val inRange = d - tolerance < dmax && dmax < d + tolerance
          inRange
        }
        val bin = Bin(
          DataPoint(dmax, dmaxCount),
          grouped.map(g => DataPoint(g._1, g._2))
        )
        loop(others, bin::groups)
      }
    }

    val distsSortByCount: List[(Int@@FloatRep, Int)] = in
      .sorted.toList
      .groupByPairs(_ == _)
      .map(p => (p.head, p.length))
      .sortBy(_._2)
      .reverse
      .toList


    val binned = loop(distsSortByCount, List())

    binned.sortBy(_.size()).reverse
  }

}
