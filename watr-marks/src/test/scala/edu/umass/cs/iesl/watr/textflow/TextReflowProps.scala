package edu.umass.cs.iesl.watr
package textreflow

// import org.scalacheck._
// import scalaz._, Scalaz._

// import org.scalacheck.Properties
// import org.scalacheck.Prop.forAll
// import matryoshka._


// object GeneralizedReflowProps extends Properties("GeneralizedReflowProps") {
//   import GeneralizedReflow._
//   import Reflow._

  // def genFlow[A] = Gen.listOf[Reflow[A]]

  // implicit def arbitraryAtom: Delay[Arbitrary, Atom] = new Delay[Arbitrary, Atom] {
  //   def apply: Arbitrary[Atom[Char]] =
  //     Arbitrary.arbitrary[Char].map(Atom(_))

  // }

  // implicit def arbitraryReflow: Delay[Arbitrary, Reflow] = new Delay[Arbitrary, Reflow] {
  //   def apply[α](arb: Arbitrary[α]): Arbitrary[Reflow[α]] =
  //     Arbitrary(Gen.oneOf(
  //       Arbitrary.arbitrary[Char].map(v => Atom(v)),
  //       Arbitrary.arbitrary[String].map(s => Edit(s)),
  //       Gen.listOfN(20, arb.arbitrary).map(s => Flow(s))
  //     ))
  // }


  // property("myprop") = forAll { l: List[Int] =>
  //   l.reverse.reverse == l
  // }
// }

// class ReflowSpec extends Specification  {
//   // def checkAll(name: String, ruleSet: Laws#RuleSet): Unit = {
//   //   for ((id, prop) ← ruleSet.all.properties)
//   //     test(name + "." + id) {
//   //       check(prop)
//   //     }
//   // }
// implicit val arbitrary: Delay[Arbitrary, Reflow] = new Delay[Arbitrary, Reflow] {
//   def apply[α](arb: Arbitrary[α]): Arbitrary[Reflow[α]] =
//     Arbitrary(Gen.oneOf(


//   implicit def equal[A]: Equal[Reflow[A]] = {
//     import Reflow._
//     Equal.equal[Reflow[A]]{
//       case (Atom(c)                    , Atom(c2)                     ) => c == c2
//       case (Edit(value)                , Edit(value2)                 ) => value == value2
//       case (Flow(atoms)                , Flow(atoms2)                 ) => false
//       case (Contiguous(a)              , Contiguous(a2)               ) => false
//       case (Rewrite(from, to)          , Rewrite(from2, to2)          ) => false
//       case (Labeled(flow, l)           , Labeled(flow2, l2)           ) => false
//       case (Focus(hole, rprevs, nexts) , Focus(hole2, rprevs2, nexts2)) => false
//       case (_, _)                                                       => false
//     }
//   }

//   "Reflow should satisfy relevant laws" >> {


//     traverse.laws[Reflow]
//     // checkAll(equal.laws[Reflow[Int]])
//     // checkAll(traverse.laws[Reflow])
//   }
// }

// class GeneralizedReflowTest extends FunSuite with  Discipline   {
//   // import GeneralizedReflow._

// }
