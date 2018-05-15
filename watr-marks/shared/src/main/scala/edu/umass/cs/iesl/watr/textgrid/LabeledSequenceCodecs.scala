package edu.umass.cs.iesl.watr
package textgrid

import geometry._
import TypeTags._

import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._

import utils.DoOrDieHandlers._
import watrmarks._

import utils.Cursor

case class LabelSpan(
  label: Label,
  begin: Int,
  length: Int
)

case class LabelingTree(
  labelSpan: LabelSpan,
  children: Seq[LabelingTree]
)

object LabeledSequenceCodecs {
  import LabeledSequenceTreeTransforms._

  def encodeBioLabels[A <: LabelTarget](labeledSequence: LabeledSequence[A]): Json = {
    val labelTree = labeledSequenceToLabelTree(labeledSequence)
    val labelSpanTree = labelTreeToSpanTree(labelTree)
    spanTreeToJson(labelSpanTree)
  }


  def decodeAndApplyBioLabels[A <: LabelTarget](jsonRep: Json, labeledSequence: LabeledSequence[A]): Unit = {
    val labelingTrees = jsonRep.decodeOrDie[Seq[LabelingTree]]()

    def loop(lt: LabelingTree): Unit = {
      labeledSequence.addBioLabel(
        lt.labelSpan.label,
        lt.labelSpan.begin,
        lt.labelSpan.length
      )
      lt.children.foreach(loop(_))
    }
  }

  implicit def decodeLabelSpan: Decoder[LabelSpan] = Decoder.decodeTuple3[Label, Int, Int]
    .map { t => LabelSpan(t._1, t._2, t._3) }

  implicit def encodeLabelSpan: Encoder[LabelSpan] = Encoder.encodeTuple3[Label, Int, Int]
    .contramap{ ls => (ls.label, ls.begin, ls.length) }

  implicit def decodeLabelingTree: Decoder[LabelingTree] = Decoder.decodeTuple2[LabelSpan, Seq[Json]]
    .map{ t =>
      LabelingTree(t._1, t._2.map(_.decodeOrDie[LabelingTree]()))
    }

  implicit def encodeLabelingTree: Encoder[LabelingTree] = Encoder.encodeTuple2[LabelSpan, Seq[Json]]
    .contramap{ lTree =>
      val childs = lTree.children.map{_.asJson}
      (lTree.labelSpan, childs)
    }

}
