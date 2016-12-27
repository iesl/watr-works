package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.js.annotation.JSExport

// import native.mousetrap._

import textreflow._


object TextReflowExamples extends PlainTextReflow {
  import TextReflowF._
  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._

  def example1(): TextReflow = {
    val p0 = stringToTextReflow("To be or not to be.")

    p0
  }

  def renderToHtml(t: TextReflowF[(TextReflow, String)]): String = t match {
    case Atom    (ac)                    => ac.char
    case Insert  (value)                 => value
    case Rewrite ((from, attr), to)      => to
    case Bracket (pre, post, (a, attr))  => s"$pre${attr}$post"
    case Mask    (mL, mR, (a, attr))     => attr.drop(mL).dropRight(mR).mkString
    case Flow    (atomsAndattrs)         => atomsAndattrs.map(_._2).mkString
    case Labeled (labels, (a, attr))     => attr
    case CachedText((a, attr), text)     => text
  }

  def renderHtml(tr: TextReflow): String = {
    val res = tr.cata(attributePara(renderToHtml))
    res.toPair._1
  }
}


@JSExport
object DevClient {

  import TextReflowExamples._

  // construct a text reflow widget

  @JSExport
  def main(): Unit = {
    println("Dev Client started (yet again)")
  }

}
