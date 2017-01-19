package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import textreflow._
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
  def echoTextReflow(textReflow: TextReflow): Unit
  def echoTextReflows(textReflows: List[TextReflow]): Unit
  def echoCharAtom(charAtom: CharAtom): Unit
  def echoTargetRegion(tr: TargetRegion): Unit
  def echoLTBounds(bbox: LTBounds): Unit
  def echoDouble(d: Double): Unit
  def showTargetRegion(targetRegion: TargetRegion, label: Label): Unit
}
