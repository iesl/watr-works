package edu.umass.cs.iesl.watr
package watrcolors
package server

object TypeConverters {

  import SharedTypeConversions._

  def convertVisualTraceTypes(cc1: utils.TraceLog): TraceLog = {
    cc1 match {
      case utils.TraceLog.Noop                                => TraceLog.Noop
      case utils.TraceLog.SetPageGeometries(pgs)              => TraceLog.SetPageGeometries(pgs.map(_.convert()))
      case utils.TraceLog.Show(s: Seq[spindex.TargetRegion])  => TraceLog.Show(s.map(_.convert))
      case utils.TraceLog.ShowZone(s: spindex.Zone)           => TraceLog.ShowZone(s.convert)
      case utils.TraceLog.ShowComponent(s: spindex.Component) => TraceLog.ShowComponent(s.convert)
      case utils.TraceLog.ShowLabel(l: watrmarks.Label)       => TraceLog.ShowLabel(l.convert)
      case utils.TraceLog.ShowVDiff(d1: Double, d2: Double)   => TraceLog.ShowVDiff(d1, d2)
      case utils.TraceLog.FocusOn(s: spindex.TargetRegion)    => TraceLog.FocusOn(s.convert)
      case utils.TraceLog.HRuler(s: Double)                   => TraceLog.HRuler(s)
      case utils.TraceLog.VRuler(s: Double)                   => TraceLog.VRuler(s)
      case utils.TraceLog.Message(s: String)                  => TraceLog.Message(s)
      case utils.TraceLog.All(ts)                             => TraceLog.All(ts.map(convertVisualTraceTypes(_)))
      case utils.TraceLog.Link(ts)                            => TraceLog.Link(ts.map(convertVisualTraceTypes(_)))

    }
  }
}
