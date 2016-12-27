package edu.umass.cs.iesl.watr
package watrcolors
package client

import geometry._
import GeometricFigure._
import native.fabric

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

  def createShape(shape: GeometricFigure, color: String, bg: String, opacity: Float): fabric.FabricObject = {
    shape match {
      case p: Point =>
        val radius = 20

        val c = new fabric.Circle()
        c.left = p.x - radius
        c.top = p.y - radius
        c.radius = radius
        c.stroke      = color
        c.strokeWidth = 2
        c.fill        = bg
        c.hasControls = false
        c.hasBorders  = false
        c.selectable  = false
        // c.opacity = opacity

        c

      case Line(p1: Point, p2: Point) =>
        val l = new fabric.Line()
        l.x1 = p1.x
        l.y1 = p1.y
        l.x2 = p2.x
        l.y2 = p2.y
        l.stroke      = color
        l.strokeWidth = 2
        l.fill        = bg
        // l.opacity = opacity

        l

      case b:LTBounds => addLTBoundsRect(b, color, bg, opacity)

      case b:LBBounds =>
        val lt = LTBounds(b.left, b.bottom-b.height, b.width, b.height)
        addLTBoundsRect(lt, color, bg, opacity)

    }
  }

  def addLTBoundsRect(bbox: LTBounds, color: String, bg: String, opacity: Float): fabric.FabricObject = {
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

}
