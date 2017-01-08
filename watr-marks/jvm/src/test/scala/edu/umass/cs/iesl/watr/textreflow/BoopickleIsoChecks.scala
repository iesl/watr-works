package edu.umass.cs.iesl.watr
package textreflow


import org.scalacheck._
import scalaz._, Scalaz._
import org.scalacheck.Prop._

import matryoshka._
import matryoshka.scalacheck.arbitrary._
// import java.nio.ByteBuffer

import TextReflowF._

// object BoopickleIsoChecks extends Properties("BoopickleIsoChecks") with ArbitraryTextReflows with TextReflowBoopicklers {
// object BoopickleIsoChecks extends Properties("BoopickleIsoChecks")
//     with ArbitraryTextReflows
// {

//   def annotateAndPrint(tr: TextReflow): Unit = {
//     // val ranges = tr.annotateCharRanges()
//     val rbox = prettyPrintTree(tr)
//     println(rbox)
//   }
//   // import boopickle._
//   import boopickle.Default._


//   property("boopickle <--> textReflow isomorphism") = forAll{ (tr: TextReflow) =>
//     // implicit def pstate = new PickleState(new EncoderSize)
//     // implicit def ustate: ByteBuffer => UnpickleState = b => new UnpickleState(new DecoderSize(b), true)

//     val bb = Pickle.intoBytes(tr)
//     val unpickled = Unpickle[TextReflow].fromBytes(bb)
//     println("Example")
//     annotateAndPrint(tr)
//     println("Unpickled")
//     annotateAndPrint(unpickled)
//     println()

//     tr === unpickled
//   }
// }

// // val toStr = bb.array().map(_.toInt).mkString(", ")
// // println(s"bb: $toStr")
// // pickler.pickle(tr)(pstate)
// // val bb = pstate.toByteBuffer
// // val ustate = UnpickleState(pstate.toByteBuffer)
// // val unpickled = pickler.unpickle(ustate)
