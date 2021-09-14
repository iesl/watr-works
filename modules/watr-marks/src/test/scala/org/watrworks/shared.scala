package org.watrworks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalacheck._

import scalaz.{@@ => _, _}, Scalaz._

import annots._
import textgrid._
import TypeTags._
import textboxing.{TextBoxing => TB}, TB._

import LabeledSequence.Things
import LabelTarget.Thing

trait LabeledSequenceThings {

  def unlabeledThings(len: Int): Things[Char] = {
    Things(('a' to 'z').take(len).map(Thing[Char](_)))
  }

}

trait WatrSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll


trait TextGridTestExamples extends TextGridConstruction {
  import _root_.io.circe
  import watrmarks._

  val JsonPrettyPrinter = circe.Printer(
    // preserveOrder = true,
    dropNullValues = false,
    indent = " "*4,
    lbraceRight = "",
    rbraceLeft = "\n",
    lbracketRight = "",
    rbracketLeft = "",
    lrbracketsEmpty = "",
    arrayCommaRight = " ",
    objectCommaRight = "\n",
    colonLeft = " ",
    colonRight = " "
  )

  val sample3PageDocs = List(
    List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "rst\nuvw\nxyz"
    ),
    List(
      "exit-\ning\n",
      "cellar\ndoor\n",
      "close-\nup\n"
    )
  )

  val Authors = Label.auto
  val Author = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val LastName = Label.auto
  val Journal = Label.auto
  val RefMarker = Label.auto
  val RefNumber = Label.auto

  val bishopClarkLabelSpans = List(
    ((0, 1),   RefMarker),
    ((0, 0),   RefNumber),
    ((3, 33),  Authors),
    ((3, 17),  Author),
    ((3, 14),  LastName),
    ((17, 17), FirstName),
    ((24, 33), Author),
    ((36, 48), Journal)
  )

  def makeBishopClarkTextGrid(): TextGrid = {

    val unlabeledText = {
      //"0         1         2         3         4         5
      // 012345678901234567890123456789012345678901234567899 """
      """1. Bishop-Clark, C  and Wheeler, D; S.Eng. P-Hall"""
    }

    val documentId = DocumentID("Bishop-Clark")
    var textGrid = stringToPageTextGrid(documentId, unlabeledText,  PageNum(1), None)
    val labeledRow = addLabelsToGridRow(textGrid.rows().head, bishopClarkLabelSpans)
    textGrid = TextGrid.fromRows(documentId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()
    textGrid = textGrid.split(9, 7).get
    textGrid
  }
}


trait TextGridSpec extends AnyFlatSpec with Matchers with TextGridTestExamples with LabeledSequenceThings {
  def infobox(heading: String, b: TB.Box): Unit = {
    info(heading)
    info("\n" + indent(4, b).toString() + "\n")
  }
}

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
