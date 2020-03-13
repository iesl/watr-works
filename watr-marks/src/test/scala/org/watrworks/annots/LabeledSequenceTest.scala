package org.watrworks
package annots

import _root_.io.circe
import circe.syntax._

import LabeledSequenceCodecs._
import LabeledSequencePrinting._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import LabeledSequence.Things
import LabelTarget.Thing


trait LabeledSequenceTestBasics
    extends AnyFlatSpec
    with Matchers
    with LabeledSequenceThings
    with TextGridTestExamples


class LabeledSequenceTest extends TextGridSpec {

  info("behavior of label sequence labeling/unlabeling")

  it should "application of labels" in {
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


  it should "find the span of cells that have a particular label" in {
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



  it should "find the span of cells that have identical labeling (including unlabeled spans)" in {
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

}
