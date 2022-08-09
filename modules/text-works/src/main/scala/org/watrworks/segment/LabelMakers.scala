package org.watrworks
package segment

import transcripts.Transcript
import geometry._
import rsearch.{Octothorpe => Oct}
import Neighbor._
import utils.IndexedSeqADT._
import org.watrworks.utils.IndexedSeqADT.SeqPos.First
import org.watrworks.utils.IndexedSeqADT.SeqPos.Last
import org.watrworks.utils.IndexedSeqADT.SeqPos.Mid
import org.watrworks.utils.IndexedSeqADT.SeqPos.Sole

trait LabelMakers { self: BasePageSegmenter =>

  def mkLabel: LabelMakers = self

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

  def onShapeList[Fig <: GeometricFigure](
    figs: Seq[Fig],
    name: String = "",
    qualifier: String = ""
  ): Transcript.Label = {

    val figLabels = figs.zipWithIndexADT
      .to(List)
      .map({ case (fig, pos) =>
        val mk = (n: Int) =>
          Transcript.Label
            .create(s"${qualifier}${n}")
            .onShapes(fig)

        pos match {
          case First   => mk(0)
          case Last(i) => mk(i)
          case Mid(i)  => mk(i)
          case Sole    => mk(0)
        }
      })

    Transcript.Label
      .create(s"${name}Vec")
      .withChildren(figLabels: _*)
  }

  def onShapeGrid[Fig <: GeometricFigure](
    name: String,
    fmat: Seq[Seq[Fig]]
  ): Transcript.Label = {
    val rows = for {
      (fvec, rown) <- fmat.zipWithIndex
    } yield {
      val rowFigs = for {
        (fig, coln) <- fvec.zipWithIndex
      } yield {
        val cellLabel = Transcript.Label
          .create(s"C(${rown},${coln})")
          .onShapes(fig)

        if (rown == 0 && coln == 0) cellLabel.withProp("role", "icon")
        else cellLabel
      }
      Transcript.Label.create(s"R${rown}").withChildren(rowFigs: _*)
    }

    Transcript.Label
      .create(s"${name}Mat")
      .withProp("schema", "matrix")
      .withProp("display", "icon")
      .withChildren(rows: _*)
  }

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
      .withProp("display", "icon")
      .withChildren(
        createLabel("Octothorpe")
          .withChildren(
            createLabelOn("FocalRect", oct.focalRect)
              .withProp("role", "icon")
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
