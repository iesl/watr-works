package edu.umass.cs.iesl.watr
package tracing

import scala.language.experimental.macros

import geometry._
// import watrmarks.Label

import textboxing.{TextBoxing => TB}
import TB._

sealed trait TraceLog

object TraceLog {


  case object Noop                                   extends TraceLog
  case class SetPageGeometries(b: Seq[PageGeometry]) extends TraceLog
  case class Show(s: Seq[TargetRegion])              extends TraceLog { override val toString = s"""${s.map(_.toString).mkString(",")}"""}
  case class ShowZone(s: Zone)                       extends TraceLog { override val toString = s"""${s}"""}
  // case class ShowComponent(s: Component)             extends TraceLog { override val toString = s"""${s}"""}
  // case class ShowLabel(l:Label)                      extends TraceLog { override val toString = s"""${l}"""}
  case class FocusOn(s: TargetRegion)                extends TraceLog { override val toString = s"""focus: ${s}"""}
  case class Indicate(figure: TargetFigure)          extends TraceLog { override val toString = s"""indicate: ${figure}"""}
  case class Message(s: TB.Box)                      extends TraceLog { override val toString = s"""${s}""" }
  case class All(ts: Seq[TraceLog])                  extends TraceLog { override val toString = s"""all: ${ts.map(_.toString).mkString(" ")}"""}
  case class Link(ts: Seq[TraceLog])                 extends TraceLog { override val toString = s"""link: ${ts.map(_.toString()).mkString(" ")}"""}
  case class Group(name: String, ts: Seq[TraceLog])  extends TraceLog { override val toString = s"""group:${name} (l=${ts.length}) """}
  case class GroupEnd(name: String)                  extends TraceLog

}


object VisualTracer {
  import TraceLog._
  import scala.language.implicitConversions


  implicit def Figure2Trace(f: TargetFigure): TraceLog = indicate(f)

  implicit class RicherString(val s: String) extends AnyVal {
    def withTrace(t: TraceLog): TraceLog = link(
      message(s), t
    )

    def withInfo(t: TB.Box): TraceLog = link(
      message(s), message(t)
    )
  }

  // implicit class RicherTraceLog(val trace: TraceLog) extends AnyVal {}

  def noop                                              = Noop
  def setPageGeometries(b: Seq[PageGeometry]): TraceLog = {SetPageGeometries(b)}
  def showRegion(s: TargetRegion): TraceLog             = {Show(Seq(s))}
  def showRegions(s: Seq[TargetRegion]): TraceLog       = {Show(s)}
  def showZone(s: Zone): TraceLog                       = {ShowZone(s)}
  // def showComponent(s: Component): TraceLog             = {ShowComponent(s)}
  // def showComponents(cs: Seq[Component]): TraceLog      = {all(cs.map(ShowComponent(_)))}
  // def showLabel(s: Label): TraceLog                     = {ShowLabel(s)}
  def focusOn(s: TargetRegion): TraceLog                = {FocusOn(s)}
  def indicate(s: TargetFigure): TraceLog               = {Indicate(s)}
  def message(s: Box): TraceLog                         = {Message(s)}
  def all(ts: Seq[TraceLog]): TraceLog                  = {All(ts)}
  def link(ts: TraceLog*): TraceLog                     = {Link(ts)}

  def begin(name: String) = Group(name, Seq())
  def end(name: String) = GroupEnd(name)
  import scala.collection.mutable


  // TODO replace this global w/config
  var visualTraceLevel: VisualTraceLevel = VisualTraceLevel.Off
  val visualTraceFilters  = mutable.Stack[String]()
  val vtraceStack = mutable.Stack[TraceLog.Group](TraceLog.Group("root", Seq()))

  def getFilters(): Seq[String] = visualTraceFilters

  def addFilter(s: String): Unit = {
    visualTraceFilters.push(s)
  }

  def clearFilters(): Unit = {
    visualTraceFilters.clear()
  }

  def traceIsUnfiltered(): Boolean = {
    getFilters.isEmpty || vtraceStack.exists({ group =>
      getFilters.exists({ filter =>
        group.name.toLowerCase().contains(filter.toLowerCase())
      })
    })

  }
}

class VisualTracer() extends EnableTrace[TraceLog] {
  import VisualTracer._
  import TraceLog._

  override def traceLevel(): VisualTraceLevel = visualTraceLevel
  override def tracingEnabled(): Boolean = {
    traceLevel() != VisualTraceLevel.Off && traceIsUnfiltered()
  }


  def closeTopGroup(): Unit = {
    val group1 = vtraceStack.pop()
    val group2 = vtraceStack.pop()
    val group12 = group2.copy(ts = group2.ts :+ group1)
    vtraceStack.push(group12)
  }
  def printlnIndent(s: Box, force: Boolean = false) = {
    if (traceIsUnfiltered()) {
      val leftIndent = vtraceStack.length * 4
      val ibox = indent(leftIndent)(s)
      println(ibox.toString)
    }
  }

  def traceIf(cond: Boolean)(exprs: TraceLog*): Unit =  macro VisualTraceMacros.runIfEnabledWithCondition[TraceLog]

  def trace(exprs: TraceLog*): Unit = macro VisualTraceMacros.runIfEnabled[TraceLog]

  def ifTrace(body: Unit): Unit = macro VisualTraceMacros.sideEffectIfEnabled[TraceLog]

  def formatTrace(trace: TraceLog): Option[Box] = {
    trace match {
      case g:Group                => None
      case g:GroupEnd             => None
      case Noop                   => None
      case SetPageGeometries(pgs) => "SetPageGeometries".box.some
      case ShowZone(zone)         => "ShowZone".box.some
      case All(ts)                => ("all" besideS vjoins()(ts.map(formatTrace(_)).flatten)).some
      // case ShowLabel(l)           => l.toString.box.some
      // case ShowComponent(c)       => c.toString.box.some
      case Show(targetRegions)    => vjoins()(targetRegions.map(_.toString.box)).some
      case Link(ts)               => hjoins(sep=" ")(ts.map(formatTrace(_)).flatten).some
      case Message(m)             => ("-" besideS m).some
      case FocusOn(targetRegion)  => ("Focus" besideS targetRegion.toString).some
      case Indicate(targetRegion) => ("Indicate" besideS targetRegion.toString).some
    }
  }

  def runTrace(level: VisualTraceLevel, tlogs: TraceLog*): Unit = {
    tlogs.foreach { trace =>
      trace match {
        case g:Group =>
          if (level == VisualTraceLevel.Print)  {
            printlnIndent(s"begin:${g.name}", true)
          }
          vtraceStack.push(g)

        case g:GroupEnd =>
          if (!vtraceStack.exists(_.name == g.name)) {
            sys.error(s"visual trace end(${g.name}) has no open() statement")
          }
          while (vtraceStack.top.name != g.name) {
            closeTopGroup()
            if (level == VisualTraceLevel.Print)  {
              printlnIndent(s"/end:${g.name}", true)
            }
          }
          closeTopGroup()
          if (level == VisualTraceLevel.Print)  {
            printlnIndent(s"/end:${g.name}", true)
          }

        case _ =>
          val group = vtraceStack.pop()
          val g2 = group.copy(ts = group.ts :+ trace)
          vtraceStack.push(g2)

          if (level == VisualTraceLevel.Print)  {
            formatTrace(trace).foreach{t =>
              printlnIndent(t)
            }
          }
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