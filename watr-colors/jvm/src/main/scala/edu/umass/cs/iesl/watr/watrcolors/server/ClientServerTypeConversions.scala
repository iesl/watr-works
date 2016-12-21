package edu.umass.cs.iesl.watr
package watrcolors
package server

// import textboxing.{TextBoxing => TB}

// object TypeConverters {

//   import SharedTypeConversions._

//   def convertComponent(cc1: spindex.Component): watrcolors.Component = {
//     ???
//   }

//   def convertVisualTraceTypes(cc1: utils.TraceLog): TraceLog = {
//     cc1 match {
//       case tracing.TraceLog.Noop                                => TraceLog.Noop
//       case tracing.TraceLog.SetPageGeometries(pgs)              => TraceLog.SetPageGeometries(pgs.map(_.convert()))
//       case tracing.TraceLog.Show(s: Seq[spindex.TargetRegion])  => TraceLog.Show(s.map(_.convert))
//       case tracing.TraceLog.ShowZone(s: spindex.Zone)           => TraceLog.ShowZone(s.convert)
//       case tracing.TraceLog.ShowComponent(s: spindex.Component) => TraceLog.ShowComponent(s.convert)
//       case tracing.TraceLog.ShowLabel(l: watrmarks.Label)       => TraceLog.ShowLabel(l.convert)
//       case tracing.TraceLog.FocusOn(s: spindex.TargetRegion)    => TraceLog.FocusOn(s.convert)
//       case tracing.TraceLog.Indicate(s: spindex.TargetFigure)   => TraceLog.Indicate(s.convert)
//       case tracing.TraceLog.Message(s: TB.Box)                  => TraceLog.Message(s.toString)
//       case tracing.TraceLog.All(ts)                             => TraceLog.All(ts.map(convertVisualTraceTypes(_)))
//       case tracing.TraceLog.Link(ts)                            => TraceLog.Link(ts.map(convertVisualTraceTypes(_)))
//       case tracing.TraceLog.Group(name, ts)                     => TraceLog.Group(name, ts.map(convertVisualTraceTypes(_)))
//       case tracing.TraceLog.GroupEnd(name)                      => TraceLog.GroupEnd(name)

//     }
//   }
// }
