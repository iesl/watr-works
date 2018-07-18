package edu.umass.cs.iesl.watr
package textgrid

import org.scalacheck._
import scalaz.{@@ => _, _}, Scalaz._

import LabeledSequenceCodecs._
import LabeledSequencePrinting._
import LabeledSequence.Things
import LabelTarget.Thing


object ArbitraryStuff extends LabeledSequenceThings {
  import Arbitrary._

  implicit def arbitraryList[A](implicit a: Arbitrary[A]): Arbitrary[List[A]] =
    Arbitrary(listOf(arbitrary[A]))

  def listOf[A](g : => Gen[A]) : Gen[List[A]] =
    Gen.listOf(g).map(_.foldRight(List.empty[A])(_ :: _))

  val genThing = for {
    c <- arbitrary[Char]
  } yield Thing[Char](c)


  def genThings(): Gen[LabeledSequence[Thing[Char]]] = for {
    i <- Gen.choose(0, 20)
    l <- listOf[Thing[Char]](genThing)
  } yield Things(l)

  implicit lazy val arbitraryLabeledSequence: Arbitrary[LabeledSequence[Thing[Char]]] = {
    Arbitrary(genThings())
  }

  def showFailed[A: Equal](a1: A, a2: A): Unit = {
    if (a1 =/= a2) {
      println("mismatch, a1: ")
      println(a1)
      println("a2: ")
      println(a2)
    }
  }

}


object LabeledSequenceCodecChecks extends Properties("LabeledSequenceCodecChecks") with LabeledSequenceThings {
  import ArbitraryStuff._

  property("json <--> LabeledSequence") = Prop.forAll{ (labeledSequence: LabeledSequence[Thing[Char]]) =>
    // TODO
    true
  }

}

class LabeledSequenceCodecTest extends LabeledSequenceTestBasics {


  "Behavior of labeled sequence serialization to/from Json" in {

    val thingCount = 10
    val things = unlabeledThings(thingCount)

    things.addBioLabel(Journal)
    things.addBioLabel(Author)
    things.addBioLabel(FirstName, 0, 3)
    things.addBioLabel(MiddleName, 4, 1)
    things.addBioLabel(LastName, 6, 3)
    val encodedJson = encodeBioLabels(things)

    val things2 = unlabeledThings(thingCount)
    decodeAndApplyBioLabels(encodedJson, things2)

    assert(things === things2)

  }
}
