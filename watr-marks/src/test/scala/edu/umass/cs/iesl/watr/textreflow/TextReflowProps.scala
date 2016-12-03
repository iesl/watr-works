package edu.umass.cs.iesl.watr
package textreflow

import org.scalacheck._
import scalaz._, Scalaz._

import org.scalacheck.Prop._

import matryoshka._
import matryoshka.scalacheck.arbitrary._

import scalaz.scalacheck.ScalaCheckBinding._

trait ArbitraryTextReflows {
  import TextReflowF._
  import Arbitrary._
  import spindex._
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

  implicit def arbPageAtom: Arbitrary[PageAtom] = {
    (arbTargetRegion |@| arbString)(
      CharAtom(_, _, None)
    )
  }


  implicit val arbTextReflow: Delay[Arbitrary, TextReflowF] =
    new Delay[Arbitrary, TextReflowF]{
      def apply[A](arbA: Arbitrary[A]): Arbitrary[TextReflowF[A]] =  {

        val gAtom      = (arbPageAtom.arbitrary |@| arbOps.arbitrary)(Atom[A](_, _))
        val gins       = arbString.arbitrary.map(Insert[A](_))
        val gRewrite   = (arbA.arbitrary ⊛ arbString.arbitrary)(Rewrite[A](_, _))
        val gFlow      = Gen.listOf(arbA.arbitrary).map(rs =>Flow[A](Set(), rs))
        val genBracket = (arbString.arbitrary |@| arbString.arbitrary |@| arbA.arbitrary)(Bracket[A](_, _, _))
        val genLabeled = arbA.arbitrary.map(Labeled[A](Set(), _))

        Arbitrary(
          Gen.oneOf(gAtom, gins, gRewrite, gFlow, genBracket, genLabeled)
        )
      }
    }


}



object TextReflowProps extends Properties("TextReflowProps") with ArbitraryTextReflows {
  import TextReflowRendering._

  // def expGen = Gen.resize(1, corecursiveArbitrary[Mu[TextReflowF], TextReflowF].arbitrary)
  // property("arb x") =forAll(expGen){ tr => }


  property("examine examples") = forAll{ (i: TextReflow) =>
    // println(i)
    true
  }

  property("json <--> textReflow isomorphism") = forAll{ (textReflowEx: TextReflow) =>
    val asJson = textReflowEx.toJson()
    val textReflow = fromJson(asJson)

    true
  }


}
