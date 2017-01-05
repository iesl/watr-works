package edu.umass.cs.iesl.watr
package watrcolors
package client

import geometry._

import native.fabric
// import scala.scalajs.js.annotation.JSExport

trait HtmlCanvasRendering {
  def initFabric(elemId: String): fabric.Canvas = {
    val c = new fabric.Canvas(elemId, fabric.CanvasOptions)
    // Store a ref to the fabric js object for future use
    jQuery(s"#$elemId").prop("fabric", c)
    c.uniScaleTransform = true
    c
  }

  def getFabric(elemId: String): fabric.Canvas = {
    jQuery(s"#$elemId").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  def createLTBoundsRect(bbox: LTBounds, color: String, bg: String, opacity: Float): fabric.FabricObject = {
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
    rect.opacity = opacity

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

  def createShape(shape: GeometricFigure, color: String, bg: String, opacity: Float): fabric.FabricObject = {
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
        c.stroke      = color
        c.strokeWidth = 2
        c.fill        = bg
        noControls(c)
        c.opacity = opacity
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
        l.stroke      = color
        l.strokeWidth = 2
        l.fill        = bg
        l.opacity = opacity

        l

      case bbox:LTBounds =>
        // createLTBoundsRect(b, color, bg, opacity)
        val rect = fabric.Rect()
        noControls(rect)
        rect.top         = bbox.top
        rect.left        = bbox.left
        rect.width       = bbox.width
        rect.height      = bbox.height
        rect.stroke      = color
        rect.strokeWidth = 1
        rect.fill        = bg
        rect.opacity = opacity

        rect

      case b:LBBounds =>
        val bbox = LTBounds(b.left, b.bottom-b.height, b.width, b.height)
        val rect = fabric.Rect()
        noControls(rect)
        rect.top         = bbox.top
        rect.left        = bbox.left
        rect.width       = bbox.width
        rect.height      = bbox.height
        rect.stroke      = color
        rect.strokeWidth = 1
        rect.fill        = bg
        rect.opacity = opacity

        rect
        // createLTBoundsRect(lt, color, bg, opacity)

    }
  }



}
