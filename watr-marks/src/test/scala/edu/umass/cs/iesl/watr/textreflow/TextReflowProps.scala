package edu.umass.cs.iesl.watr
package textreflow

import org.scalacheck._
import scalaz._, Scalaz._

import org.scalacheck.Prop._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.scalacheck._
import matryoshka.scalacheck.arbitrary._

import scalaz.scalacheck.ScalaCheckBinding._



object GeneralizedReflowProps extends Properties("TextReflowProps") {
  import TextReflowF._
  // import Arbitrary._


  // implicit def arbTextReflowAtomOps: Arbitrary[TextReflowAtomOps] =
  //   Arbitrary(arbitrary[String].map(s => new TextReflowAtomOps(s.toList)))

  implicit val arbTextReflow: Delay[Arbitrary, TextReflowF] =
    new Delay[Arbitrary, TextReflowF]{
      def apply[A](arb: Arbitrary[A]): Arbitrary[TextReflowF[A]] =  {

        // def ains:Arbitrary[TextReflowF[A]] =
        //   arbString.map(Insert(_))


        // val gAtom:Gen[TextReflowF[A]] =
        //   arbTextReflowAtomOps.arbitrary.map(a => Atom(0, a))

        // val gRewrite:Gen[TextReflowF[A]] =
        //   (arb.arbitrary ⊛ arbString.arbitrary)(Rewrite(_, _))

        // val gFlow:Gen[TextReflowF[A]] =
        //   Gen.listOf(arb.arbitrary).map(rs =>Flow(Set(), rs))

        // val arbTr: Arbitrary[TextReflowF[A]] = Arbitrary(
        //   Gen.oneOf(
        //     gAtom,
        //     gins
        //     // (arb.arbitrary |@| arbString.arbitrary)(Rewrite(_, _)),
        //     // (arbString.arbitrary |@| arbString.arbitrary |@| arb.arbitrary)(Bracket(_, _, _)),
        //     // Gen.listOf(arb.arbitrary).map(rs =>Flow(Set(), rs)),
        //     // arb.arbitrary.map(Labeled(Set(), _))
        //   )
        // )
        // arbTr
        val gins: Gen[TextReflowF[A]] =
          Arbitrary.arbString.arbitrary.map(Insert[A](_))

        println(s"gen. ${gins}")
        println(s"gen ${Arbitrary(gins).arbitrary}")
        // Arbitrary(gins)


        // Arbitrary(arbitrary[String].map(Insert[A](_)))
        // Insert[α]("wer").point[Gen]
        val xxx = Arbitrary(
          Gen.oneOf(
            gins,
            gins
          )
        )

        println(s"xxx ${xxx}")

        Arbitrary(Arbitrary.arbitrary[String].map(Insert[A](_)))

      }
    }



  property("smokescreen test") =
    forAll{ (i: Int) =>
      i >= Int.MinValue
    }

  // def expGen = Gen.resize(1, corecursiveArbitrary[Mu[TextReflowF], TextReflowF].arbitrary)

  // property("arb x") =
  //   forAll(expGen){ i =>
  //     println(i)

  //     true
  //   }


  property("arb x") =
    forAll{ (i: TextReflow) =>
      println(i)
      true
    }

  // // "sdf" >> prop{ (i: Int) => }
  // forAll {(n: Int) =>
  //   n shouldBe > Int.MinValue

  // }

}
