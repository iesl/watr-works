package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import textreflow.data._
import labeling._
import scala.concurrent.Future

trait WatrShellApi {
  def helloShell(msg: String): Unit
  def onSelectLTBounds(artifactId: String, bbox: LTBounds): Future[List[LTBounds]]
  def onDrawPath(artifactId: String, path: Seq[Point]): Unit
}

trait WatrColorsApi {
  def helloColors(msg: String): Unit
  def clear(): Unit
  def print(level: String, msg: String): Unit
  def echoTextReflows(textReflows: List[TextReflow]): Unit
  def echoLabeler(lwidget: List[PosAttr]): Unit
}
