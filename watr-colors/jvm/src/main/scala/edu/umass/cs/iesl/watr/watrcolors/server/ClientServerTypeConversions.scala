package edu.umass.cs.iesl.watr
package watrcolors
package server

object TypeConverters {

  implicit class RicherShape(val shape: spindex.Shape) extends AnyVal {
    def toOverlay(): Overlay = shape match {
      case v: spindex.LTBounds => v.toBBox
      case v: spindex.Point => v.toPoint
      case v: spindex.Line => v.toLine

      case spindex.LBBounds(left, bottom, width, height) =>
        BBox(left, bottom-height, width, height)
    }
  }

  implicit class RicherLine(val ltb: spindex.Line) extends AnyVal {
    def toLine(): Line = Line(ltb.p1.toPoint, ltb.p2.toPoint)
  }

  implicit class RicherPoint(val ltb: spindex.Point) extends AnyVal {
    def toPoint(): Point = Point(ltb.x, ltb.y)
  }

  implicit class RicherLTBounds(val ltb: spindex.LTBounds) extends AnyVal {
    def toBBox(): BBox = BBox(ltb.left, ltb.top, ltb.width, ltb.height)
  }


  def convertVisualTraceTypes(cc1: utils.VisualTrace.DSL[_]): VisualTrace.DSL = {
    import spindex._
    cc1 match {
      case utils.VisualTrace.Noop                              => VisualTrace.Noop
      case utils.VisualTrace.SetViewport(b: LTBounds)          => VisualTrace.SetViewport(b.toBBox)
      case utils.VisualTrace.GetViewport()                     => VisualTrace.GetViewport()
      case utils.VisualTrace.Show(s: Shape)                    => VisualTrace.Show(s.toOverlay)
      case utils.VisualTrace.ShowVDiff(d1: Double, d2: Double) => VisualTrace.ShowVDiff(d1, d2)
      case utils.VisualTrace.FocusOn(s: Shape)                 => VisualTrace.FocusOn(s.toOverlay)
      case utils.VisualTrace.HRuler(s: Double)                 => VisualTrace.HRuler(s)
      case utils.VisualTrace.VRuler(s: Double)                 => VisualTrace.VRuler(s)
      case utils.VisualTrace.Message(s: String)                => VisualTrace.Message(s)
      case utils.VisualTrace.And(t1, t2)                       =>  ???
        // VisualTrace.And(convertVisualTraceTypes(t1),convertVisualTraceTypes(t2))
      case utils.VisualTrace.AndThen(t1, t2)                   => ??? // VisualTrace.AndThen(t1, t2)
      case utils.VisualTrace.All(t1, t2)                   => ??? // VisualTrace.AndThen(t1, t2)
      // case utils.VisualTrace.All(t1, t2)                   => ??? // VisualTrace.AndThen(t1, t2)
    }
  }
}
