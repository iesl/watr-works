package edu.umass.cs.iesl.watr
package utils

import scala.scalajs.js
import scala.scalajs.js.annotation._
import js.JSConverters._

@JSExportTopLevel("scOptions")
object Options {
  @JSExport
  def orOriginal[A](f: A => Option[A]): A => A =
    expr => f(expr).getOrElse(expr)

  @JSExport
  def orDefault[A, B](default: B)(f: A => Option[B]): A => B =
    expr => f(expr).getOrElse(default)

  @JSExport
  def getOrElse[A](a: Option[A], default: A): A = a.getOrElse(default)

  @JSExport
  def fold[A, B](
    a: Option[A],
    ifEmpty: js.Function0[B],
    f: js.Function1[A, B]
  ): B = { a.fold(ifEmpty())(f(_)) }

}

@JSExportTopLevel("Labels")
object Labels {
  import watrmarks._

  @JSExport
  def forString(s: String): Label = Label(s)

}

@JSExportTopLevel("JsArray")
object JsArray {

  @JSExport
  def fromScalaIterable(seq: Iterable[Any]): js.Array[Any] = seq.toJSArray

  @JSExport
  def fromScala(seq: Seq[Any]): js.Array[Any] = seq.toJSArray

}
