package org.watrworks
package segment

import transcripts.Transcript
import geometry._
import rsearch.{Octothorpe => Oct}
import Neighbor._

trait LabelMakers { self: BasePageSegmenter =>

  val createLabel = Transcript.Label.create(_)

  def createLabelOn[Fig <: GeometricFigure](name: String, fig: Fig) =
    Transcript.Label.create(name).onShapes(fig)

  def createLabelsOn[Fig <: GeometricFigure](name: String, figs: Seq[Fig]) =
    Transcript.Label
      .create(s"${name}Root")
      .withChildren(
        figs.zipWithIndex.map({ case (f, i) =>
          Transcript.Label.create(s"${name}#${i}").onShapes(f)
        }): _*
      )

  def createLabelsOnShapes(name: String, shapes: Seq[AnyShape]) = {
    val shapeLabels = shapes.zipWithIndex.map({ case (shape, i) =>
      val ls = shape.labels.mkString(":")
      createLabelOn(s"${ls}@${name}#${i}", shape.shape)
    })

    Transcript.Label
      .create(s"${name}Root")
      .withChildren(
        shapeLabels: _*
      )
  }

  def createOctoSearchLabel[F <: GeometricFigure](
    bySearch: Neighbor.BySearch[F],
    results: Seq[Shape[F]]
  ): Transcript.Label = {

    val BySearch(oct, _, _, _, _, tags) = bySearch

    val searchAreaLabels = oct
      .searchAreas()
      .to(List)
      .map({ case (sbounds, searchArea) =>
        val name = sbounds match {
          case Oct.Bounds.Cell(dir)        => s"Cell:${dir}"
          case Oct.Bounds.CellSpan(d1, d2) => s"Span:${d1}-${d2}"
        }
        createLabel(s"Query/${name}")
          .onShapes(searchArea)
      })

    val resLabels = traceLog.shapesToLabels(results: _*)
    createLabel("OctSearch")
      .withProp("class", ">lazy")
      .withChildren(
        createLabel("Octothorpe")
          .withChildren(
            createLabelOn("FocalRect", oct.focalRect)
              .withProp("class", "=eager")
              .withProp("tags", tags: _*),
            createLabelOn("HorizonRect", oct.horizonRect),
            createLabel("SearchArea")
              .withChildren(searchAreaLabels: _*)
          ),
        createLabel("Found")
          .withChildren(resLabels: _*)
      )
  }
}
