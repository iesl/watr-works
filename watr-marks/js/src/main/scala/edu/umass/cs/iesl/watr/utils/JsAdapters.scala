package edu.umass.cs.iesl.watr
package utils

import scala.scalajs.js
import scala.scalajs.js.annotation._
import js.JSConverters._

@JSExportTopLevel("watr.utils.Options")
object Options {
  @JSExport
  def orOriginal[A](f: A => Option[A]): A => A =
    expr => f(expr).getOrElse(expr)

  @JSExport
  def orDefault[A, B](default: B)(f: A => Option[B]): A => B =
    expr => f(expr).getOrElse(default)

  @JSExport
  def getOrElse[A](a: Option[A], default: A): A = a.getOrElse(default)

}

@JSExportTopLevel("watr.watrmarks.Labels")
object Labels {
  import watrmarks._

  @JSExport
  def forString(s: String): Label = Label(s)

}

@JSExportTopLevel("watr.utils.JsArray")
object JsArray {

  @JSExport
  def fromScala(seq: Seq[Any]): js.Array[Any] = seq.toJSArray

}
