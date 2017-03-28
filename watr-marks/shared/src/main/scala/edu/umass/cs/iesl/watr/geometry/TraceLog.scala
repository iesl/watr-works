package edu.umass.cs.iesl.watr
package geometry

import textboxing.{TextBoxing => TB}

sealed trait TraceLog

object TraceLog {

  case object Noop                                   extends TraceLog
  case class SetPageGeometries(b: Seq[PageGeometry]) extends TraceLog
  case class Show(s: Seq[TargetRegion])              extends TraceLog { override val toString = s"""${s.map(_.toString).mkString(",")}"""}
  // case class ShowZone(s: Zone)                       extends TraceLog { override val toString = s"""${s}"""}
  case class FocusOn(s: TargetRegion)                extends TraceLog { override val toString = s"""focus: ${s}"""}
  case class Message(s: TB.Box)                      extends TraceLog { override val toString = s"""${s}""" }
  case class All(ts: Seq[TraceLog])                  extends TraceLog { override val toString = s"""all: ${ts.map(_.toString).mkString(" ")}"""}
  case class Link(ts: Seq[TraceLog])                 extends TraceLog { override val toString = s"""link: ${ts.map(_.toString()).mkString(" ")}"""}
  case class Group(name: String, ts: Seq[TraceLog])  extends TraceLog { override val toString = s"""group:${name} (l=${ts.length}) """}
  case class GroupEnd(name: String)                  extends TraceLog

}
