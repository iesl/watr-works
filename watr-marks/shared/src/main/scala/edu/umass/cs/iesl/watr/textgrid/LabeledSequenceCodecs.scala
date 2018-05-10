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

    // val dummyPageRegion = PageRegion(StablePage(DocumentID("docX"), PageNum(0)), LTBounds.empty)

    // textGrid.indexedCells().map(_._1).zip(labels)
    //   .foreach { case (cell, labels) =>
    //     cell.pins ++= labels.reverse
    //   }

    // def emptyInlineBIO(len: Int): Seq[TextGrid.GridCell] = {
    //   Array.fill[TextGrid.GridCell](len){
    //     TextGrid.InsertCell('a', dummyPageRegion)
    //   }
    // }

    // def maxLen(lt: LabelingTree): Int = {
    //   val begin = lt.labelSpan.begin
    //   val len = lt.labelSpan.length
    //   val total = begin + len
    //   (total +: lt.children.map(maxLen(_))).max
    // }

    // val totalLen = if (labelingTrees.isEmpty) 0 else labelingTrees.map(maxLen(_)).max
    // val inlineBio = emptyInlineBIO(totalLen)

    def loop(lt: LabelingTree): Unit = {
      labeledSequence.addBioLabel(
        lt.labelSpan.label,
        lt.labelSpan.begin,
        lt.labelSpan.length
      )

      // for {
      //   gridCursor <- Cursor.init(inlineBio)
      //   c3         <- gridCursor.move(begin)
      // } yield {
      //   val window = c3.toWindow()
      //   val winNext = window.widen(len-1).map{ w2 =>
      //     LabeledSequence.addBioLabel(label, w2.cells)
      //     w2
      //   } getOrElse{
      //     window
      //   }
      //   winNext.closeWindow()
      // }
      lt.children.foreach(loop(_))
    }

    labelingTrees.foreach(loop(_))

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
