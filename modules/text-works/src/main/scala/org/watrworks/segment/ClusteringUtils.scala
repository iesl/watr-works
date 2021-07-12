package org.watrworks
package segment

import smile.clustering.{hclust, HierarchicalClustering}

// import smile.math.distance.Distance
// import scala.collection.JavaConverters._
import utils.ExactFloats._

// import geometry._
// import geometry.syntax._
import utils.Interval
import Interval._

import TypeTags._
// import utils.IndexedSeqADT._

object clusteringUtils {
  def clusterPoints(
    points: Seq[Int @@ FloatRep],
    maxClusterWidth: Int @@ FloatRep
  ): Seq[FloatExacts] = {

    if (points.isEmpty) List()
    else {
      val data: Array[Array[Double]] = points.map(p => Array(p.asDouble())).to(Array)
      val width                      = maxClusterWidth.asDouble()

      val minp   = points.min
      val maxp   = points.max
      val widthp = maxp - minp
      if (widthp <= maxClusterWidth) {
        List(Interval.FloatExacts(minp, widthp))
      } else {

        // val max = cluster.max.toFloatExact()

        val clust           = hclust(data, "complete")
        val partitions      = clust.partition(width)
        val partitionMap    = partitions.zip(data).groupBy(_._1)
        val clusters        = partitionMap.values.to(List)
        val clusteredPoints = clusters.map(_.map(_._2.head))
        val clusterRanges = clusteredPoints.map { cluster =>
          val min = cluster.min.toFloatExact()
          val max = cluster.max.toFloatExact()
          Interval.FloatExacts(min, max - min)
        }

        clusterRanges
      }
    }
  }

}
