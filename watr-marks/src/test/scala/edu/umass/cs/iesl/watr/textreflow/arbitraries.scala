package edu.umass.cs.iesl.watr
package textreflow

import org.scalacheck._
import scalaz._, Scalaz._

import matryoshka._
import matryoshka.scalacheck.arbitrary._

import scalaz.scalacheck.ScalaCheckBinding._

// import spindex._
import watrmarks._
import TextReflowF._

import geometry._
import GeometricFigure._
import EnrichGeometricFigures._

trait ArbitraryTextReflows {
  import Arbitrary._
  import GeometricFigure._
  import TypeTags._

  implicit def arbOps: Arbitrary[TextReflowAtomOps] =
    Arbitrary(arbitrary[String].map(s => new TextReflowAtomOps(s.toList)))

  implicit def arbLTBounds: Arbitrary[LTBounds] = {
    (arbDouble |@| arbDouble |@| arbDouble |@| arbDouble)(
      LTBounds.apply
    )
  }

  implicit def arbTargetRegion: Arbitrary[TargetRegion] = {
    (arbInt |@| arbInt |@| arbLTBounds)({
      case (id, tr, bbox) =>
        TargetRegion(RegionID(id), PageID(tr), bbox)
    })
  }

  implicit def arbCharAtom: Arbitrary[CharAtom] = {
    (arbTargetRegion |@| arbString |@| arbOption[Int])(
      CharAtom(_, _, _)
    )
  }

  implicit def arbImgAtom: Arbitrary[ImgAtom] = {
    arbTargetRegion.map(ImgAtom(_))
  }

  implicit def arbPageAtom: Arbitrary[PageAtom] = {
    arbCharAtom.map(_.asInstanceOf[PageAtom])
    // Arbitrary(Gen.oneOf(Seq(
    //   arbCharAtom.arbitrary,
    //   // arbImgAtom.arbitrary
    // )))
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

        val gAtom      = (arbPageAtom.arbitrary |@| arbOps.arbitrary)(Atom[A](_, _))
        val gins       = arbString.arbitrary.map(Insert[A](_))
        val gRewrite   = (arbA.arbitrary ⊛ arbString.arbitrary)(Rewrite[A](_, _))
        val gFlow      = Gen.listOf(arbA.arbitrary).map(Flow[A](_))
        val genBracket = (arbString.arbitrary |@| arbString.arbitrary |@| arbA.arbitrary)(Bracket[A](_, _, _))
        val genLabeled = (arbLabels.arbitrary |@| arbA.arbitrary)(Labeled[A](_, _))

        Arbitrary(
          Gen.oneOf(gAtom, gins, gRewrite, gFlow, genBracket, genLabeled)
        )
      }
    }


}
