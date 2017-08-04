package edu.umass.cs.iesl.watr
package textreflow

import org.scalacheck._
import scalaz._, Scalaz._

import matryoshka._
import matryoshka.scalacheck.arbitrary._

import scalaz.scalacheck.ScalaCheckBinding._

import watrmarks._
import geometry._
import TextReflowF._
import TypeTags._


trait ArbitraryTextReflows extends ArbitraryGeometries {
  import Arbitrary._



  implicit def arbStablePage: Arbitrary[StablePage] = {
    (arbString |@| arbInt |@| arbInt)({
      case (s, i, i2) =>
        StablePage(DocumentID(s), PageNum(i), Some(PageID(i2)))
    })
  }


  implicit def arbPageRegion: Arbitrary[PageRegion] = {
    (arbInt |@| arbStablePage |@| arbLTBounds)({
      case (id, page, bbox) =>
        PageRegion(page, bbox, Option(RegionID(id)))
    })
  }


  implicit def arbCharAtom: Arbitrary[CharAtom] = {
    (arbInt |@| arbPageRegion |@| arbString |@| arbOption[Int])({
      case (id, tr, cstr, code) =>
        CharAtom(CharID(id), tr, cstr, code)
    })
  }


  implicit def arbLabel: Arbitrary[Label] = {

    (arbString |@| arbString |@| arbOption[String] |@| arbInt)({
      case (ns, key, value, id) =>
        Label(ns, key, value, LabelID(id))
    })
  }
  implicit def arbLabels: Arbitrary[Set[Label]] = {
    Arbitrary(Gen.listOf(arbLabel.arbitrary).map(_.toSet))
  }

  implicit val arbTextReflow: Delay[Arbitrary, TextReflowF] =
    new Delay[Arbitrary, TextReflowF]{
      def apply[A](arbA: Arbitrary[A]): Arbitrary[TextReflowF[A]] =  {

        val gAtom      = arbCharAtom.arbitrary.map(Atom(_))
        val gins       = arbString.arbitrary.map(Insert(_))
        val gRewrite   = (arbA.arbitrary ⊛ arbString.arbitrary)(Rewrite[A](_, _))
        val gFlow      = Gen.listOf(arbA.arbitrary).map(Flow[A](_))
        // val genBracket = (arbString.arbitrary |@| arbString.arbitrary |@| arbA.arbitrary)(Bracket[A](_, _, _))
        val genLabeled = (arbLabels.arbitrary |@| arbA.arbitrary)(Labeled[A](_, _))

        Arbitrary(
          Gen.oneOf(gAtom, gins, gRewrite, gFlow, genLabeled)
        )
      }
    }


}
