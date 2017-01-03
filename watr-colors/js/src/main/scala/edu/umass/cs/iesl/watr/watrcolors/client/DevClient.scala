package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.js.annotation.JSExport

import textreflow._
import geometry._
import GeometricFigure._
import watrmarks.{StandardLabels => LB}

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
    stringToTextReflow("""|To be or not to be,
                          |That is the question.
                          |""")
  }


  def extractVisualLineIds(tr: TextReflow): Seq[String] = {
    def render(t: TextReflowF[(TextReflow, Seq[String])]): Seq[String] = t match {
      case Rewrite    ((from, attr), to)      => attr
      case Bracket    (pre, post, (a, attr))  => attr
      case Flow       (atomsAndattrs)         => atomsAndattrs.flatMap(_._2)
      case Labeled    (labels, (a, attr))     =>
        attr ++ labels.
          filter(_ == LB.VisualLine)
          .flatMap(_.value)
          .toSeq

      case _ => Seq()
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
    val vlineIds = extractVisualLineIds(example1)
    val qwer = vlineIds.mkString("\n")

    jQuery("#main").append("Starting")
    jQuery("#main").append(qwer)
    jQuery("#main").append("Done")

  }

}
