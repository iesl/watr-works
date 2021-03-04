package org.watrworks
package segment

import transcripts.Transcript
import geometry._
import TypeTags._

object ShapeClustering {

  case class RootInit(
    name: String,
    clusters: List[ClusteredIds]
  ) {
    def addCluster(instanceIds: Seq[Int @@ ShapeID]): RootInit = {
      copy(
        clusters = ClusteredIds(instanceIds) :: clusters
      )
    }

    def build(
      shapeDict: Map[Int @@ ShapeID, AnyShape]
      // pageDict: Map[Int @@ ShapeID, Transcript.Page]
    ): Root = {

      val root = Root(name, List())
      clusters.foldLeft(root)({
        case (acc, ids) => {

          val instances = ids.instances.map(id => {
            val shape = shapeDict(id)
            Instance(
              id,
              shape.shape,
              shape.pageNum
            )
          })

          val cluster = ClusteredInstances(instances)

          Root(
            acc.name,
            cluster :: acc.clusters
          )
        }
      })

    }
  }

  def labeledShapeToLabel(shape: AnyShape): Transcript.Label = {
    val labelId = LabelID(shape.id.unwrap)
    Transcript.Label(
      "Trapezoid",
      Some(labelId),
      range = List(
        Transcript.PageRange(at = shape.pageNum),
        Transcript.GeometryRange("shape", at = shape.shape)
      ),
      props = None,
      children = None
    )
  }

  case class ClusteredIds(
    instances: Seq[Int @@ ShapeID]
  )

  def init(name: String): RootInit =
    RootInit(name, List())

  case class Root(
    name: String,
    clusters: List[ClusteredInstances]
  )

  case class ClusteredInstances(
    instances: Seq[Instance]
  )
  case class Instance(
    id: Int @@ ShapeID,
    shape: GeometricFigure,
    pageNum: Int @@ PageNum
  )

  def toTranscriptLabel(clustering: Root): Transcript.Label = {
    val clusterLabels = clustering.clusters.map(c => {
      val instLabels = c.instances.map(inst => {
        Transcript.Label(
          s"${clustering.name}::Instance",
          None,
          List(
            Transcript.LabelRange(at = LabelID(inst.id.unwrap))
          ),
          None, // Attr info
          None
        )
      })
      Transcript.Label(
        s"${clustering.name}::Cluster",
        None,
        List(),
        None,
        Some(instLabels.toList)
      )
    })

    Transcript.Label(
      s"${clustering.name}::ClusteringRoot",
      None,
      List(),
      None,
      Some(clusterLabels)
    )
  }

  def fromTranscriptLabel(
    transcript: Transcript,
    label: Transcript.Label
  ): Root = {
    val clusterName = label.name.takeWhile(c => c != ':')
    val labelMap    = transcript.labels
      .filter(l => l.id.isDefined)
      .map(l => (l.id.get.unwrap, l))
      .toMap

    val clusteredInstances = (for {
      clusters <- label.children.toList
      cluster  <- clusters
    } yield {
      val instanceRecs: List[Instance] = (for {
        instances     <- cluster.children.toList
        instanceLabel <- instances
      } yield {
        val id = instanceLabel.range.headOption
          .map({
            case r: Transcript.LabelRange => r.at.unwrap
            case _                        => 0
          })
          .getOrElse(0)

        val referencedShapeLabel = labelMap(id)
        val shapeId              = ShapeID(id)

        val clusteredInstances: List[Instance] = referencedShapeLabel.range match {
          case List(
                // Transcript.DocumentRange(_, doc @ _),
                Transcript.PageRange(_, pageNum),
                Transcript.GeometryRange(_, shape)
              ) =>
            // val page = transcript.pages(pageNum.unwrap)
            List(Instance(shapeId, shape, pageNum))

          case _ => List()
        }
        clusteredInstances
      }).flatten

      ClusteredInstances(instanceRecs)
    })

    Root(clusterName, clusteredInstances)
  }
}
