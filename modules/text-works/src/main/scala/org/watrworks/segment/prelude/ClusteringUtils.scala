package org.watrworks
package segment
package prelude

import smile.clustering.{hclust}

import utils.ExactFloats._

import utils.Interval

case class Instance[A](
  a: A,
  vec: Array[Double]
)
case class Cluster[A, Rep](
  rep: Rep,
  instances: List[Instance[A]]
)

object clusteringUtils {

  def clusterDataX[A](
    datain: Seq[A],
    toVec: A => Array[Double],
    maxError: Double
  ): Seq[Cluster[A, Unit]] = {

    val extractToDblArray =
      (t: A) => toVec(t).toArray

    val data = datain
      .map(extractToDblArray)
      .toArray

    val instances = datain.zip(data).map(d => Instance(d._1, d._2)).toList
    if (data.isEmpty) List()
    else {
      val clust     = hclust(data, "complete")
      clust.getTree()
      val maxHeight = clust.getHeight().max

      val clusteredPoints: List[Cluster[A, Unit]] =
        if (maxError >= maxHeight) {
          List(Cluster(rep=(), instances))
        } else {
          val partitions: Array[Int]   = clust.partition(maxError)

          val partitionMap = partitions.zip(instances).groupBy(_._1)
          val clusters = partitionMap.values.to(List)
          val clusters2: List[Array[Instance[A]]] = clusters.map(_.map(_._2))
          clusters2.map(i => Cluster((), i.toList))

        }

      // val clusterRanges = clusteredPoints.map { cluster =>
      //   val trans = cluster.transpose[Double]

      //   trans.toSeq.map(ps => {
      //     val min = ps.min.toFloatExact()
      //     val max = ps.max.toFloatExact()
      //     Interval.FloatExacts(min, max - min)
      //   })
      // }

      // clusterRanges
      clusteredPoints
    }
  }

  def clusterData[T](
    // datain: Seq[Seq[Int @@ FloatRep]],
    datain: Seq[T],
    extractFn: T => Seq[Int @@ FloatRep],
    maxError: Double
  ): Seq[Seq[Interval.FloatExacts]] = {

    val extractToDblArray =
      (t: T) => extractFn(t).map(_.asDouble()).toArray

    val data = datain
      .map(extractToDblArray)
      .toArray

    if (data.isEmpty) List()
    else {
      val clust = hclust(data, "complete")

      // println(s"""Data (${data.length}) = ${data.map(_.mkString("<", ", ", ">")).mkString("\n  ", "\n  ", "")}""")
      // println(s"""Cluster Height = ${clust.getHeight().mkString(", ")}""")
      // val treeStr = clust.getTree().map(_.mkString(" + ")).mkString("(", "; ", ")")
      // println(s"""Cluster Tree = ${treeStr}""")

      val maxHeight = clust.getHeight().max
      val clusteredPoints = if (maxError >= maxHeight) {
        List(data)
      } else {
        val partitions   = clust.partition(maxError)
        val partitionMap = partitions.zip(data).groupBy(_._1)
        val clusters     = partitionMap.values.to(List)
        clusters.map(_.map(_._2))
      }

      val clusterRanges = clusteredPoints.map { cluster =>
        val trans = cluster.transpose[Double]

        trans.toSeq.map(ps => {
          val min = ps.min.toFloatExact()
          val max = ps.max.toFloatExact()
          Interval.FloatExacts(min, max - min)
        })
      }

      clusterRanges
    }
  }

}
