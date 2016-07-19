package edu.umass.cs.iesl.watr
package utils

import spindex._

import scala.language.existentials
import scala.collection.mutable


object TraceLog {
  import VisualTrace._
  val entries: mutable.MutableList[VisualTrace.DSL[_]] = mutable.MutableList()

  var currPRG = Noop

  def trace(p: VisualTrace.DSL[_]*): Unit = {
    entries ++= p
  }

  // def traces[T](p: => Seq[VisualTrace.DSL[T]]): Unit = {
  //   entries ++= p
  // }


  def getAndClearTraceLog(): Seq[VisualTrace.DSL[_]] = {
    val ret = Seq(entries:_*)
    entries.clear()
    ret
  }
}




object VisualTrace {
  sealed trait DSL[A]

  case object Noop extends DSL[Unit]
  case class SetViewport(b: LTBounds) extends DSL[Unit]
  case class GetViewport() extends DSL[LTBounds]
  case class Show(s: Shape) extends DSL[Unit]
  case class ShowVDiff(d1: Double, d2: Double) extends DSL[Unit]
  case class FocusOn(s: Shape) extends DSL[Unit]
  case class HRuler(s: Double) extends DSL[Shape]
  case class VRuler(s: Double) extends DSL[Shape]
  case class Message(s: String) extends DSL[Unit]

  case class All[A](ts: DSL[A]*) extends DSL[Unit]
  case class And[A](t1: DSL[A], t2: DSL[A]) extends DSL[Unit]
  case class AndThen[A](t1: DSL[A], t2: DSL[A]) extends DSL[Unit]

}




// object interp {
//   import VisualTrace._

//   class EmitTracesInterpreter() extends (VisualTrace.DSL ~> cats.Id) {
//     def apply[A](a: VisualTrace.DSL[A]) = a match {
//       case Noop =>
//       case SetViewport(b: LTBounds) =>
//       case GetViewport() =>
//         println("set!")
//         Bounds.empty
//       case Show(s: Shape) =>
//         println("show!")
//         Json.obj(
//           "" -> ""
//         )

//         ()


//       case ShowVDiff(d1: Double, d2: Double) =>
//       case FocusOn(s: Shape) =>
//       case HRuler(s: Double) =>
//         println("rule!")
//           ???
//       case Message(s: String) =>
//       case And(t1, t2) =>
//       case AndThen(t1, t2) =>
//     }
//   }

