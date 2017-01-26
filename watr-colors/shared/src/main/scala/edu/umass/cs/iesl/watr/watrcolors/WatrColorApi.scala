package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import textreflow.data._
import display.data._

trait WatrShellApi {
  def helloShell(msg: String): Unit
  def onSelectLTBounds(artifactId: String, bbox: LTBounds): Unit
  def onDrawPath(artifactId: String, path: Seq[Point]): Unit
}

trait WatrColorsApi {
  def helloColors(msg: String): Unit
  def clear(): Unit
  def print(level: String, msg: String): Unit
  def echoTextReflows(textReflows: List[TextReflow]): Unit
  def echoLabeler(lwidget: LabelWidget): Unit
}
