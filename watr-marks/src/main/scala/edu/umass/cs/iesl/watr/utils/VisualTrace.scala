package edu.umass.cs.iesl.watr
package utils

import spindex._
import watrmarks.Label

sealed trait TraceLog

object TraceLog {

  case object Noop                                   extends TraceLog
  case class SetPageGeometries(b: Seq[PageGeometry]) extends TraceLog
  case class Show(s: Seq[TargetRegion])              extends TraceLog
  case class ShowZone(s: Zone)                       extends TraceLog
  case class ShowComponent(s: Component)             extends TraceLog
  case class ShowLabel(l:Label)                      extends TraceLog
  case class ShowVDiff(d1: Double, d2: Double)       extends TraceLog
  case class FocusOn(s: GeometricFigure)             extends TraceLog
  case class HRuler(s: Double)                       extends TraceLog
  case class VRuler(s: Double)                       extends TraceLog
  case class Message(s: String)                      extends TraceLog
  case class All(ts: Seq[TraceLog])                  extends TraceLog
  case class Link(ts: Seq[TraceLog])                 extends TraceLog


}

import scala.collection.mutable

class VisualTracer() {

  val vtrace = mutable.MutableList[TraceLog]()

  import TraceLog._

  def trace(tls: TraceLog*): Unit = tls.foreach(vtrace += _)

  def setPageGeometries(b: Seq[PageGeometry]): TraceLog = SetPageGeometries(b)
  def showRegion(s: TargetRegion): TraceLog             = Show(Seq(s))
  def showRegions(s: Seq[TargetRegion]): TraceLog       = Show(s)
  def showZone(s: Zone): TraceLog                       = ShowZone(s)
  def showComponent(s: Component): TraceLog             = ShowComponent(s)
  def showLabel(s: Label): TraceLog                     = ShowLabel(s)
  def showVDiff(d1: Double, d2: Double): TraceLog       = ShowVDiff(d1, d2)
  def focusOn(s: GeometricFigure): TraceLog             = FocusOn(s)
  def hRuler(s: Double): TraceLog                       = HRuler(s)
  def vRuler(s: Double): TraceLog                       = VRuler(s)
  def message(s: String): TraceLog                      = Message(s)
  def all(ts: Seq[TraceLog]): TraceLog                  = All(ts)
  def link(ts: TraceLog*): TraceLog                     = Link(ts)


  def getAndResetTrace(): List[TraceLog] = {
    val t = vtrace.toList
    vtrace.clear()
    t
  }
}
