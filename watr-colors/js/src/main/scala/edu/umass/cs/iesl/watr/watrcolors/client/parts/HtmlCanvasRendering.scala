package edu.umass.cs.iesl.watr
package watrcolors
package client
package parts

import geometry._
import native.fabric
import utils.Color
import utils.Colors

trait HtmlCanvasRendering {
  def initFabric(elemId: String): fabric.StaticCanvas = {
    val c = new fabric.Canvas(elemId, fabric.CanvasOptions)
    // Store a ref to the fabric js object for future use
    jQuery(s"#$elemId").prop("fabric", c)
    c.uniScaleTransform = true
    c.defaultCursor = "crosshair"
    c.hoverCursor = "crosshair"
    c
  }

  def getFabric(elemId: String): fabric.Canvas = {
    jQuery(s"#$elemId").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  def createLTBoundsRect(bbox: LTBounds, color: String, bg: String, fgOpacity: Float): fabric.FabricObject = {
    val rect = fabric.Rect()
    rect.top         = bbox.top
    rect.left        = bbox.left
    rect.width       = bbox.width
    rect.height      = bbox.height
    rect.stroke      = color
    rect.strokeWidth = 1
    rect.fill        = bg
    rect.hasControls = false
    rect.hasBorders  = false
    rect.selectable  = false
    rect.opacity = fgOpacity

    rect
  }

  def withControls(shape: fabric.FabricObject): Unit = {
    shape.hasControls = true
    shape.hasBorders  = true
    shape.selectable  = true
  }

  def noControls(shape: fabric.FabricObject): Unit = {
    shape.hasControls = false
    shape.hasBorders  = false
    shape.selectable  = false
  }

  def createShape(shape: GeometricFigure, fgColor: String, bgColor: String, fgOpacity: Float=1.0f, bgOpacity: Float=1.0f): fabric.FabricObject = {
    shape match {
      case p: Point =>
        val radius = 4

        val c = new fabric.Circle()
        c.left = p.x - radius
        c.top = p.y - radius
        c.width  = radius * 2
        c.height = radius * 2
        c.radius = radius
        c.startAngle = 0
        c.endAngle = math.Pi * 2
        c.stroke      = fgColor
        c.strokeWidth = 1
        c.fill        = bgColor
        noControls(c)
        c.opacity = fgOpacity
        c

      case Line(p1: Point, p2: Point) =>

        val l = new fabric.Line()
        noControls(l)

        l.left = p1.x
        l.top = p1.y
        l.width  = p2.x - p1.x
        l.height = p2.y - p1.y

        l.x1 = p1.x
        l.y1 = p1.y
        l.x2 = p2.x
        l.y2 = p2.y
        l.stroke      = fgColor
        l.strokeWidth = 1
        l.fill        = bgColor
        l.opacity = fgOpacity

        l

      case bbox:LTBounds =>
        val rect = fabric.Rect()
        noControls(rect)
        rect.top         = bbox.top
        rect.left        = bbox.left
        rect.width       = bbox.width
        rect.height      = bbox.height
        rect.stroke      = fgColor
        rect.strokeWidth = 1
        rect.fill        = bgColor
        rect.opacity = fgOpacity

        rect

      case b:LBBounds =>
        val bbox = LTBounds(b.left, b.bottom-b.height, b.width, b.height)
        val rect = fabric.Rect()
        noControls(rect)
        rect.top         = bbox.top
        rect.left        = bbox.left
        rect.width       = bbox.width
        rect.height      = bbox.height
        rect.stroke      = fgColor
        rect.strokeWidth = 1
        rect.fill        = bgColor
        rect.opacity = fgOpacity

        rect

      case g @ GeometricGroup(bounds, figs) =>
        val shapes = figs.map(createShape(_, Colors.Black.cssHash(), bgColor, 0.2f))
        val bs = createShape(bounds, Colors.Black.cssHash(), bgColor, 0.08f)
        val group = fabric.Group(bs :: shapes)
        noControls(group)
        group

      case g @ Colorized(fig: GeometricFigure, fg: Color, bg: Color, fgOpacity: Float, bgOpacity: Float) =>
        val s = createShape(fig, fg.cssHash, bg.cssHash, fgOpacity)
        noControls(s)
        s

    }
  }

}
