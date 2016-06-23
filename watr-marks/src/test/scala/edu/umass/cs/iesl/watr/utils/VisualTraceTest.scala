package edu.umass.cs.iesl.watr
package utils

import org.scalatest._
import spindex._

// import cats.free.{Free, Trampoline}
// import cats.data.Xor
// import cats.{~>, Id}

// import scala.concurrent._
// import scala.concurrent.duration._

// import cats.derived._
// import cats.functor._
// import cats.legacy._
// import cats.Functor
// import cats.std.future._
// import cats.std.option._
// import cats.std.list._
// import ExecutionContext.Implicits.global

import freek._


class VisualTraceTest extends FlatSpec with Matchers {
  behavior of "visual trace logging"


  import VisualTrace._
  type PRG = VisualTrace.DSL :|: FXNil

  it should "produce a stream of overlays/messages" in {
    // val asdf = SetViewport(Bounds.empty).freek[VisualTrace.PRG]


    val prg = for {
      _ <- SetViewport(Bounds.empty).freek[PRG]
      _ <- Show(Bounds.empty).freek[PRG]
      _ <- HRuler(0d).freek[PRG]
    } yield ()


    // VisualTraceInterpreter.nat
    // val interpreter: Interpreter[PRG, cats.Id] = interp.visualTraceInterpreter
    // val interpreter = interp.visualTraceInterpreter // :&: interp.visualTraceInterpreter

    // prg.interpret(interpreter)
    // prg.interpret(interpreter)

    // val sdf = prg.compile(interpreter.nat) // Iv(VisualTraceInterpreter)
    // val asdf = sdf.run
    // sdf.foldMap(interpreter.nat)

    // val qewr = prg.foldMap(VisualTraceInterpreter.nat)
  }


}
