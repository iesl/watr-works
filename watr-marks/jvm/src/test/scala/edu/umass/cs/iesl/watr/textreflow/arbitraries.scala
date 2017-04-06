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


trait ArbitraryTextReflows {
  import Arbitrary._


  implicit def arbLTBounds: Arbitrary[LTBounds] = {
    (arbDouble |@| arbDouble |@| arbDouble |@| arbDouble)(
      LTBounds.apply
    )
  }

  implicit def arbStablePageID: Arbitrary[StablePageID] = {
    (arbString |@| arbInt)({
      case (s, i) =>
        StablePageID(DocumentID(s), PageNum(i))
    })
  }

  implicit def arbRecordedPageID: Arbitrary[RecordedPageID] = {
    (arbInt |@| arbStablePageID)({
      case (id, stable) =>
        RecordedPageID(PageID(id), stable)
    })
  }

  implicit def arbPageRegion: Arbitrary[PageRegion] = {
    (arbInt |@| arbRecordedPageID |@| arbLTBounds)({
      case (id, rec, bbox) =>
        PageRegion(rec, bbox)
    })
  }

  implicit def arbTargetRegion: Arbitrary[TargetRegion] = {
    (arbInt |@| arbRecordedPageID |@| arbLTBounds)({
      case (id, rec, bbox) =>
        TargetRegion(RegionID(id), rec, bbox)
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
