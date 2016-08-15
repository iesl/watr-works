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
  case class FocusOn(s: TargetRegion)                extends TraceLog
  case class Indicate(figure: TargetFigure)          extends TraceLog
  case class Message(s: String)                      extends TraceLog
  case class All(ts: Seq[TraceLog])                  extends TraceLog { override val toString = s"all#${ts.length}"}
  case class Link(ts: Seq[TraceLog])                 extends TraceLog { override val toString = s"link#${ts.length}"}
  case class Group(name: String, ts: Seq[TraceLog])  extends TraceLog { override val toString = s"group:${name}#${ts.length}"}
  case class GroupEnd(name: String)                  extends TraceLog

}

import scala.collection.mutable

object VisualTracer {
  import TraceLog._
  import scala.language.implicitConversions


  implicit def Figure2Trace(f: TargetFigure): TraceLog = indicate(f)

  implicit class RicherString(val s: String) extends AnyVal {
    def withTrace(t: TraceLog): TraceLog = link(
      message(s), t
    )
  }

  implicit class RicherTraceLog(val trace: TraceLog) extends AnyVal {

  }

  def setPageGeometries(b: Seq[PageGeometry]): TraceLog = SetPageGeometries(b)
  def showRegion(s: TargetRegion): TraceLog             = Show(Seq(s))
  def showRegions(s: Seq[TargetRegion]): TraceLog       = Show(s)
  def showZone(s: Zone): TraceLog                       = ShowZone(s)
  def showComponent(s: Component): TraceLog             = ShowComponent(s)
  def showLabel(s: Label): TraceLog                     = ShowLabel(s)
  def focusOn(s: TargetRegion): TraceLog                = FocusOn(s)
  def indicate(s: TargetFigure): TraceLog               = Indicate(s)
  def message(s: String): TraceLog                      = Message(s)
  def all(ts: Seq[TraceLog]): TraceLog                  = All(ts)
  def link(ts: TraceLog*): TraceLog                     = Link(ts)

  def begin(name: String) = Group(name, Seq())
  def end(name: String) = GroupEnd(name)

}

class VisualTracer {

  val vtraceStack = mutable.Stack[TraceLog.Group](TraceLog.Group("root", Seq()))

  import TraceLog._

  def closeTopGroup(): Unit = {
    val group1 = vtraceStack.pop()
    // println(s"pop ${group1}")
    val group2 = vtraceStack.pop()
    // println(s"pop ${group2}")
    val group12 = group2.copy(ts = group2.ts :+ group1)
    vtraceStack.push(group12)
    // println(s"push ${group12}")
  }

  def trace(tls: TraceLog*): Unit = {
    tls.foreach { trace =>
      trace match {
        case g:Group =>
          vtraceStack.push(g)
          // println(s"push ${g}")

        case g:GroupEnd =>
          if (!vtraceStack.exists(_.name == g.name)) {
            // println(s"""vtrace stack = ${vtraceStack.mkString("\n  ", "\n  ", "---\n")}""")
            sys.error(s"visual trace end(${g.name}) has no open() statement")
          }
          while (vtraceStack.top.name != g.name) {
            closeTopGroup()
          }
          closeTopGroup()

        case _ =>
          val group = vtraceStack.pop()
          // println(s"pop ${group}")
          val g2 = group.copy(ts = group.ts :+ trace)
          vtraceStack.push(g2)
          // println(s"push ${g2}")
      }
    }
  }


  def getAndResetTrace(): List[TraceLog] = {
    val t = vtraceStack.toList
    vtraceStack.clear()
    vtraceStack.push(Group("root", Seq()))
    t
  }
}
