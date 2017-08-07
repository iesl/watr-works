package edu.umass.cs.iesl.watr
package utils



object Debugging {

  // pprint.pprintln(t: T, width: Integer, height: Integer, indent: Integer, colors: Colors)

  def log[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Name) = {
    println(enclosing.value + " [" + value.source + "]: " + value.value)
  }

  def die(t: Throwable)(implicit enclosing: sourcecode.File, line: sourcecode.Line): Unit = {
    val message = s"""error ${enclosing.value}:${line.value}: ${t}: ${t.getCause}: ${t.getMessage} """
    println(s"ERROR: ${message}")
    t.printStackTrace()
    throw t
  }

}

object PrintDeshugared {
  import scala.reflect.runtime.universe._
  import scala.reflect.macros.Context
  // import scala.reflect.macros.blackbox.Context
  import scala.language.experimental.macros

  def desugarImpl(c : Context)(expr : c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    println(show(expr.tree))
    reify {}
  }

  def desugar(expr : Any): Unit = macro desugarImpl


}
