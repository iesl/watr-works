package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.js.annotation.JSExport

import textreflow._
import geometry._

import ComponentTypeEnrichments._

import native.fabric
import native.fabric._

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
                          |""".stripMargin)
  }



  def extractVisualLineTargetRegions(tr: TextReflow): Seq[TargetRegion] = {
    def render(t: TextReflowF[(TextReflow, Seq[TargetRegion])]): Seq[TargetRegion] = t match {
      case Rewrite    ((from, attr), to)      => attr
      case Bracket    (pre, post, (a, attr))  => attr
      case Flow       (atomsAndattrs)         => atomsAndattrs.flatMap(_._2)
      case Labeled    (labels, (a, attr))     =>
        val trs = for {
          l <- labels if l == LB.VisualLine
          value <- l.value
        } yield {
          TargetRegion.fromUri(value)
        }

        attr ++ trs

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


  def makePlaceholderImgs(trs: Seq[TargetRegion]): Seq[FabricObject] = {
    val objs = trs.zipWithIndex.map({case (tr, i) =>
      val shape = createShape(tr.bbox.copy(
        left=20, top=((i+1)*20).toDouble
      ), "black", "yellow", 0.5f)

      val targetRegionURI = tr.uriString
      Image.fromURL(s"/api/tr/${targetRegionURI}", {(img) =>
        shape.setFill("green")
      }, {() =>
        shape.setFill("red")
      })

      shape
    })
    fabricCanvas.renderAll()
    objs
  }


  def createAnnotWidget(textReflow: TextReflow): fabric.Group = {
    val text = textReflow.toText()
    println(s"createAnnotWidget: got ${textReflow}")
    println(s"widget for ${text}")
    val vlineIds = extractVisualLineTargetRegions(textReflow)
    println(s"got line ids ${vlineIds}")
    val ftext = fabric.Text(text)
    ftext.setFontSize(15)

    val placeholders = makePlaceholderImgs(vlineIds)
    println("created placeholders")
    val widgetGroup = fabric.Group(
      ftext +: placeholders
    )

    println("created placeholder group")
    widgetGroup
  }


  def vcatWidgets(trs: Seq[TextReflow]): Unit = {
    var currTop: Int = 0
    trs.foreach { tr =>
      val widget = createAnnotWidget(tr)
      widget.setTop(currTop)
      currTop = (currTop + widget.height.intValue())
      fabricCanvas.add(widget)
    }
  }
}


@JSExport
class DevClient extends TextReflowExamples {

  @JSExport
  def main(): Unit = {
    val widgets = for (i <- 1 to 10) yield {
      example1()
    }

    vcatWidgets(widgets)
  }

}
