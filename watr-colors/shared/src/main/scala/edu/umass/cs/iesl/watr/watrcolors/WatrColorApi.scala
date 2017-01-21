package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import textreflow._
import display._
import watrmarks.Label

trait SvgOverviewApi {
  def createView(artifactId: String): List[HtmlUpdate]
  def getLabelOverlay(artifactId: String): List[TraceLog]
  // def getTextOverlay(artifactId: String): (Seq[PageGeometry], Seq[Seq[Component]])
  def onSelectLTBounds(artifactId: String, bbox: LTBounds): List[HtmlUpdate]
  def onDrawPath(artifactId: String, path: Seq[Point]): List[HtmlUpdate]
}


trait WatrTableApi {
  def clear(): Unit
  def print(level: String, msg: String): Unit
  def echoTextReflows(textReflows: List[TextReflow]): Unit
  def echoLabeler(lwidget: LabelWidget): Unit
}
