package edu.umass.cs.iesl.watr
package tracemacros

import scala.reflect.macros.blackbox.Context

sealed trait VisualTraceLevel

object VisualTraceLevel {
  case object Off extends VisualTraceLevel
  case object EnterExit extends VisualTraceLevel
  case object Checkpoint extends VisualTraceLevel
  case object Debug extends VisualTraceLevel
  case object Callback extends VisualTraceLevel

  val all = List(Off, EnterExit, Checkpoint, Debug, Callback)

  def cmp(a: VisualTraceLevel, b:VisualTraceLevel) = all.indexOf(b) - all.indexOf(a)
}


trait EnableTrace[T] {
  def traceLevel(): VisualTraceLevel

  def tracingEnabled(): Boolean
  def runTrace(level: VisualTraceLevel, ts: T*): Unit
  def traceCallbacks: TraceCallbacksT

}

object VisualTraceMacros {
  type VTraceContext[S] = Context { type PrefixType = EnableTrace[S] }


  def printTrace[T](c: VTraceContext[T])(str: c.Expr[String]) = {
    import c.universe._
    q"""
    if (${c.prefix}.tracingEnabled()) {
       import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
       ${c.prefix}.traceLevel() match {
         case L.Print  => println($str)
         case _  =>
       }
    }
    """
  }

  // def sideEffectIfEnabled[T](c: VTraceContext[T])(vtl: c.Expr[VisualTraceLevel])(body: => c.Expr[Unit]) = {
  // println("sideEffectIfEnabled = " + doTrace + "   " + ${c.prefix}.traceLevel() + "   " +   ..$vtl)

  def sideEffectIfEnabled[T](c: VTraceContext[T])(vtl: c.Expr[VisualTraceLevel])(body: c.Tree): c.Tree = {
    import c.universe._
    q"""
    if (${c.prefix}.tracingEnabled()) {
       import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}

       val doTrace: Boolean = L.cmp(..$vtl, ${c.prefix}.traceLevel()) >= 0

       if (doTrace) {
          val _ = $body
       }
    }
    """
  }

  // def materializeCallback[T](c: VTraceContext[T])(name: c.Expr[String])(value: c.Expr[Any]) = {
  // def materializeCallback[T](c: VTraceContext[T])(name: c.Expr[String])(value: c.Expr[Any]): c.Expr[Unit] = {

  def checkpointImpl[T](c: VTraceContext[T])
    (loc: c.Expr[String], msg: c.Expr[String], args: c.Expr[Seq[Any]]) = {

    import c.universe._

    q"""
      val callbacks = ${c.prefix}.traceCallbacks()
      callbacks

    ; ()
    """

  }

  // def sideEffectIfEnabled[T](c: VTraceContext[T])(vtl: c.Expr[VisualTraceLevel])(body: c.Expr[Unit]) = {
  //   import c.universe._
  //   q"""
  //   if (${c.prefix}.tracingEnabled()) {
  //      import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
  //      ${c.prefix}.traceLevel() match {
  //        case L.Off          => // noop
  //        case _              => ..$body
  //      }
  //   }
  //   """
  // }

}

// import scala.language.dynamics
// val callbacks = new tracing.TraceCallbacks {
//   //  '(visualize hash-line bins)'
//   def segment_LineFinder__approximateLineBins(pageIndex: PageIndex): Unit = {
//     println(s"Callback to approximateLineBins")
//   }
// }

// trait TraceCallbacksT extends Dynamic { self =>
trait TraceCallbacksT  { self =>
  // foo.field           ~~> foo.selectDynamic("field")
  // foo.arr(10) = 13    ~~> foo.selectDynamic("arr").update(10, 13)
  // foo.method("blah")      ~~> foo.applyDynamic("method")("blah")
  // def applyDynamic[T](name: String)(value: Any): T = { ??? }
  // def applyDynamic[T](name: String)(value: Any): T

  // def selectDynamic(name: String): Any = {}
  // // foo.varia = 10      ~~> foo.updateDynamic("varia")(10)
  // def updateDynamic(name: String)(value: Any) = {}
  // // foo.method(x = "blah")  ~~> foo.applyDynamicNamed("method")(("x", "blah"))
  // def applyDynamicNamed[T](name: String)(values: (String, Any)*): T = {
  //   ???
  // }

}


object TraceCallbackMacros {
  type CallbackContext = Context { type PrefixType = TraceCallbacksT }

  // (implicit fTag: WeakTypeTag[F[_]]): Tree = {
  def _applyDynamic[T](c: CallbackContext)(name: c.Expr[String])(value: c.Expr[Any]): c.Expr[T] = {
    // import c.universe._
    // val proxyNames = proxyTpe.members.filter(_.isImplicit).map(_.name)

    ???
  }




  def _selectDynamic[T](c: CallbackContext)(name: c.Expr[String]): c.Expr[T] = {
    ???
  }

  def _updateDynamic(c: CallbackContext)(name: c.Expr[String])(value: c.Expr[Any]) = {
  }


  def _applyDynamicNamed(c: CallbackContext)(name: c.Expr[String])(values: (c.Expr[String], c.Expr[Any])*) = {
  }

}
