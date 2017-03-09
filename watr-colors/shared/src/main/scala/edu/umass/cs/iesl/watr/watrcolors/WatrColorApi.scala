package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import labeling._
import scala.concurrent.Future


trait WatrShellApi {
  def helloShell(msg: String): Unit
  def uiRequest(r: UIRequest): Future[UIResponse]
  def onDrawPath(artifactId: String, path: Seq[Point]): Unit
}

trait WatrColorsApi {
  def helloColors(msg: String): Unit
  def clear(): Unit
  def print(level: String, msg: String): Unit
  def echoLabeler(lwidget: Seq[WidgetPositioning], labelOptions: LabelOptions): Unit
}

final case class RemoteCall(
  path: List[String], args: List[(String, String)]
)
