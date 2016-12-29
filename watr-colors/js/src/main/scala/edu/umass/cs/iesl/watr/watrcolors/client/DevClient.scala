package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.js.annotation.JSExport

import textreflow._
import geometry._
import GeometricFigure._


trait TextReflowExamples extends PlainTextReflow with FabricCanvasOperations {
  import TextReflowF._
  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._

  override lazy val fabricCanvas =  {
    initFabric("canvas")
    getFabric("canvas")
  }

  def example1(): TextReflow = {
    val p0 = stringToTextReflow("To be or not to be.")

    p0
  }


  def renderHtml(tr: TextReflow): String = {
    def render(t: TextReflowF[(TextReflow, String)]): String = t match {
      case Atom      (charAtom)              =>
        // find the visual line associated with this CharAtom

        charAtom.char
      case Insert    (value)                 => value
      case Rewrite   ((from, attr), to)      => to
      case Bracket   (pre, post, (a, attr))  => s"$pre${attr}$post"
      case Mask      (mL, mR, (a, attr))     => attr.drop(mL).dropRight(mR).mkString
      case Flow      (atomsAndattrs)         => atomsAndattrs.map(_._2).mkString
      case Labeled   (labels, (a, attr))     => attr
      case CachedText((a, attr), text)     => text
    }

    tr.cata(attributePara(render))
      .toPair._1
  }

  def displayBasicCanvasShapes(): Unit = {
    fabricCanvas.add(createShape(Point(240, 240), "black", "blue", 0.5f))
    fabricCanvas.add(createShape(LBBounds(240, 240, 100, 200), "red", "black", 0.5f))
    fabricCanvas.add(createShape(LTBounds(240, 240, 100, 200), "black", "yellow", 0.5f))
    fabricCanvas.add(createShape(Line(Point(240, 40), Point(340, 440)), "black", "green", 0.5f))
  }
}


@JSExport
class DevClient extends TextReflowExamples {

  @JSExport
  def main(): Unit = {
    println("Dev Client started (yet again)")
    val html = renderHtml(example1)

    jQuery("#main").append(html)

  }

}
