package edu.umass.cs.iesl.watr
package textgrid


import _root_.io.circe
import circe.syntax._

import LabeledSequenceCodecs._
import LabeledSequencePrinting._
// import LabeledSequenceTreeTransforms._
import org.scalatest._

case class Thing(a: Char) extends LabelTarget
case class Things(labelTargets: Seq[Thing]) extends LabeledSequence[Thing]

trait LabeledSequenceThings {


  def unlabeledThings(len: Int): Things = {
    Things(('a' to 'z').take(len).map(Thing(_)))
  }

}

trait LabeledSequenceTestBasics
    extends FreeSpec
    with Matchers
    with LabeledSequenceThings
    with TextGridTestExamples


class LabeledSequenceTest extends TextGridSpec {

  info("behavior of label sequence labeling/unlabeling")

  "application of labels" in {
    val thingCount = 10

    val things = unlabeledThings(thingCount)

    things.addBioLabel(Journal)
    things.addBioLabel(Author)
    things.addBioLabel(FirstName, 0, 3)
    things.addBioLabel(MiddleName, 4, 1)
    things.addBioLabel(LastName, 6, 3)
    println("added labels")
    println(labeledSequenceBoxFormat(things))
    // println(createRuler(84, 40))

    info("removing labels")
    println("removed labels")
    things.unlabelNear(6, LastName)
    println(labeledSequenceBoxFormat(things))
    things.unlabelNear(4, MiddleName)
    println(labeledSequenceBoxFormat(things))
    things.unlabelNear(2, FirstName)
    println(labeledSequenceBoxFormat(things))
    info("removing all labels")
    things.unlabelNear(6, Journal)
    println("removed all labels")
    println(labeledSequenceBoxFormat(things))
  }


  "it should find the span of cells that have a particular label" in {
    val thingCount = 10

    val things = unlabeledThings(thingCount)
    things.addBioLabel(Journal)
    things.addBioLabel(Author)
    things.addBioLabel(FirstName, 0, 3)

    things.findLabelExtents(2, Author).foreach { case (begin, authorLabeled) =>
      begin shouldEqual 0
      authorLabeled.length shouldEqual thingCount
    }

    things.findLabelExtents(2, FirstName).foreach { case (begin, seq) =>
      begin shouldEqual 0
      seq.length shouldEqual 3
    }
  }



  "it should find the span of cells that have identical labeling (including unlabeled spans)" in {
    val thingCount = 10
    val things = unlabeledThings(thingCount)

    things.findIdenticallyLabeledSiblings(3).foreach{ case (offset, seq) =>
      assert(offset == 0)
      assert(seq.length == 10)
    }

    things.addBioLabel(Journal)
    things.addBioLabel(Author)
    things.addBioLabel(FirstName, 0, 3)

    things.findIdenticallyLabeledSiblings(1).foreach{ case (offset, seq) =>
      assert(offset == 0)
      assert(seq.length == 3)
    }

    things.findIdenticallyLabeledSiblings(4).foreach{ case (offset, seq) =>
      assert(offset == 0)
      assert(seq.length == 10)
    }
  }

  info("behavior of label sequence json serialization")

  "correctness of Labeled Sequence serialization" in {
    //  inline bio <--> label tree <--> json rep
    val things = unlabeledThings(10)
    things.addBioLabel(Journal)
    things.addBioLabel(Author)
    things.addBioLabel(FirstName, 0, 3)

    // val labelTree = gridCellsToLabelTree(inlineBio.cells)

    println(things.labelTargets.map(_.showPinsVert()).mkString)

    // println(labelTree.drawTree)


    // {
    //   println("Marginal Span Tree -----------------------")
    //   val labelSpanTree2 = labelTreeToMarginalSpanTree(labelTree)
    //   println(labelSpanTree2.drawTree)
    //   println("-----------------------")
    // }

    // val labelSpanTree = labelTreeToSpanTree(labelTree)

    // {
    //   println("Label Span Tree -----------------------")
    //   println(labelSpanTree.drawTree)
    //   println("-----------------------")
    // }
    // val jsonSpanTreeRep = spanTreeToJson(labelSpanTree)
    // println(jsonSpanTreeRep.pretty(JsonPrettyPrinter))

    // Json -> inline BIO

    val ls = LabelSpan(LastName, 2, 4)

    val lt = LabelingTree(
      LabelSpan(FirstName, 0, 10), List(
        LabelingTree(
          LabelSpan(LastName, 2, 4), List()
        )
      )
    )

    val lsjs = ls.asJson
    println(lsjs.noSpaces)
    val ltjs = lt.asJson
    println(ltjs.noSpaces)

    // val labelingTree = jsonSpanTreeRep.decodeOrDie[Seq[LabelingTree]]()
    // println(labelingTree)
    // val inlineBioRT = LabeledSequenceCodecs.decodeBioLabels(jsonSpanTreeRep)
    println("inline Bio Round Trip")
    // val rt = inlineBioRT.map(_.mkString).zipWithIndex.mkString("\n  ", "\n  ", "\n")
    // println(rt)

  }
}
