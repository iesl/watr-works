package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSName


@js.native
object fabric extends js.Object {
  def version: String = js.native
  def isTouchSupported: Boolean = js.native
  def isLikelyNode: Boolean = js.native
  def DPI: Int = js.native
  def devicePixelRatio: Float = js.native

  // def loadSVGFromURL(url: String, cb: js.Function2[]): Unit = {
  // }

  // def on(event: String, f:js.Function): Event = js.native
}

@js.native @JSName("fabric.Object")
trait FabricObject extends js.Object {
  var hasControls: Boolean = js.native
  var hasBorders: Boolean = js.native
  var selectable: Boolean = js.native
  // var evented: Boolean = js.native
  var transparentCorners: Boolean = js.native

  def setFill(color: String): FabricObject = js.native
  def setStroke(color: String): FabricObject = js.native

  @JSName("type")
  def otype: String = js.native
}

@js.native
trait Event extends js.Object {
  def clientX: Int = js.native
  def clientY: Int = js.native
  def pageX: Int = js.native
  def pageY: Int = js.native
}


@js.native
trait Options extends js.Object {
  val target: FabricObject = js.native
  val e: Event = js.native
}


@js.native
trait Rect extends FabricObject {
  // var hasRotatingPoint: Boolean = js.native
  // var visible: Boolean = js.native
  // var centeredScaling: Boolean = js.native
  // var centeredRotation: Boolean = js.native
  // var padding
  // var cornerSize
  // var rotatingPointOffset
  // var borderColor
  // var cornerColor

}

@js.native @JSName("fabric.Rect")
class RectNative(
  opts: RectOpts
) extends Rect



object Rect {
  def apply(
    top: Double, left: Double, width: Double, height: Double,
    fill: String = "",
    stroke: String = "",
    strokeWidth: Int = 1
  ): Rect = {
    new RectNative(RectOpts(top, left, width, height, fill, strokeWidth, stroke))
  }
}

@js.native
trait RectOpts extends js.Object {
  def top         : Double = js.native
  def left        : Double = js.native
  def width       : Double = js.native
  def height      : Double = js.native
  def fill        : String = js.native
  def strokeWidth : Int = js.native
  def stroke      : String = js.native

}

object RectOpts {
  def apply(
    top: Double,
    left: Double,
    width: Double,
    height: Double,
    fill: String = "",
    strokeWidth: Int = 1,
    stroke: String = "blue"
  ): RectOpts =
    js.Dynamic.literal(
      top = top,
      left = left,
      width = width,
      height = height,
      fill = fill,
      strokeWidth = strokeWidth,
      stroke = stroke
    ).asInstanceOf[RectOpts]
}


@js.native @JSName("fabric.Canvas")
class CanvasNative(
  el: String,
  options: CanvasOpts
) extends Canvas

@js.native
trait Canvas extends js.Object {
  def add(r: Rect): Unit = js.native
  var renderOnAddRemove: Boolean = js.native
  def renderAll(allOnTop: Boolean=false): Canvas = js.native
  // def on(event: String, f:(Event) => Boolean): Event = js.native
  // def on(event: String, f:js.ThisFunction): Event = js.native
  def on(event: String, f:js.Function): Event = js.native
  def off(event: String, f:js.Function): Event = js.native

  // def forEachObject: js.Function(callback, context)
  def forEachObject(cb: js.Function): Unit = {

  }

}


object Canvas {
  def apply(el: String, opts: CanvasOpts): Canvas = {
    new CanvasNative(el, CanvasOpts())
  }

}

@js.native trait CanvasOpts extends js.Object {
  // uniScaleTransform:      false,
  // centeredScaling:        false,
  // centeredRotation:       false,
  // interactive:            true,
  // selection:              true,
  // selectionColor:         'rgba(100, 100, 255, 0.3)', // blue
  // selectionDashArray:     [ ],
  // selectionBorderColor:   'rgba(255, 255, 255, 0.3)',
  // selectionLineWidth:     1,
  // hoverCursor:            'move',
  // moveCursor:             'move',
  // defaultCursor:          'default',
  // freeDrawingCursor:      'crosshair',
  // rotationCursor:         'crosshair',
  // containerClass:         'canvas-container',
  // perPixelTargetFind:     false,
  // targetFindTolerance:    0,
  // skipTargetFind:         false,
  // backgroundColor: '',
  // backgroundImage: null,
  // overlayColor: '',
  // overlayImage: null,
  // includeDefaultValues: true,
  // stateful: true,
  def renderOnAddRemove: Boolean  = js.native
  // clipTo: null,
  // controlsAboveOverlay: false,
  // allowTouchScrolling: false,
  // imageSmoothingEnabled: true,
  // preserveObjectStacking: false,
}

object CanvasOpts {
  def apply(
    // uniScaleTransform:      false,
    // centeredScaling:        false,
    // centeredRotation:       false,
    // interactive:            true,
    // selection:              true,
    // selectionColor:         'rgba(100, 100, 255, 0.3)', // blue
    // selectionDashArray:     [ ],
    // selectionBorderColor:   'rgba(255, 255, 255, 0.3)',
    // selectionLineWidth:     1,
    // hoverCursor:            'move',
    // moveCursor:             'move',
    // defaultCursor:          'default',
    // freeDrawingCursor:      'crosshair',
    // rotationCursor:         'crosshair',
    // containerClass:         'canvas-container',
    // perPixelTargetFind:     false,
    // targetFindTolerance:    0,
    // skipTargetFind:         false,
    // backgroundColor: '',
    // backgroundImage: null,
    // overlayColor: '',
    // overlayImage: null,
    // includeDefaultValues: true,
    // stateful: true,
    renderOnAddRemove: Boolean = true
    // clipTo: null,
    // controlsAboveOverlay: false,
    // allowTouchScrolling: false,
    // imageSmoothingEnabled: true,
    // preserveObjectStacking: false,
  ): CanvasOpts =
    js.Dynamic.literal(
      // uniScaleTransform      = uniScaleTransform,
      // centeredScaling        = centeredScaling,
      // centeredRotation       = centeredRotation,
      // interactive            = interactive,
      // selection              = selection,
      // selectionColor         = selectionColor,
      // selectionDashArray     = selectionDashArray,
      // selectionBorderColor   = selectionBorderColor,
      // selectionLineWidth     = selectionLineWidth,
      // hoverCursor            = hoverCursor,
      // moveCursor             = moveCursor,
      // defaultCursor          = defaultCursor,
      // freeDrawingCursor      = freeDrawingCursor,
      // rotationCursor         = rotationCursor,
      // containerClass         = containerClass,
      // perPixelTargetFind     = perPixelTargetFind,
      // targetFindTolerance    = targetFindTolerance,
      // skipTargetFind         = skipTargetFind,
      // backgroundColor        = backgroundColor,
      // backgroundImage        = backgroundImage,
      // overlayColor           = overlayColor,
      // overlayImage           = overlayImage,
      // includeDefaultValues   = includeDefaultValues,
      // stateful               = stateful,
      renderOnAddRemove      = renderOnAddRemove
      // clipTo                 = clipTo,
      // controlsAboveOverlay   = controlsAboveOverlay,
      // allowTouchScrolling    = allowTouchScrolling,
      // imageSmoothingEnabled  = imageSmoothingEnabled,
      // preserveObjectStacking = preserveObjectStacking

    // /**
    //  * When true, objects can be transformed by one side (unproportionally)
    //  * @type Boolean
    //  * @default
    //  */
    // uniScaleTransform:      false,

    // /**
    //  * When true, objects use center point as the origin of scale transformation.
    //  * <b>Backwards incompatibility note:</b> This property replaces "centerTransform" (Boolean).
    //  * @since 1.3.4
    //  * @type Boolean
    //  * @default
    //  */
    // centeredScaling:        false,

    // /**
    //  * When true, objects use center point as the origin of rotate transformation.
    //  * <b>Backwards incompatibility note:</b> This property replaces "centerTransform" (Boolean).
    //  * @since 1.3.4
    //  * @type Boolean
    //  * @default
    //  */
    // centeredRotation:       false,

    // /**
    //  * Indicates that canvas is interactive. This property should not be changed.
    //  * @type Boolean
    //  * @default
    //  */
    // interactive:            true,

    // /**
    //  * Indicates whether group selection should be enabled
    //  * @type Boolean
    //  * @default
    //  */
    // selection:              true,

    // /**
    //  * Color of selection
    //  * @type String
    //  * @default
    //  */
    // selectionColor:         'rgba(100, 100, 255, 0.3)', // blue

    // /**
    //  * Default dash array pattern
    //  * If not empty the selection border is dashed
    //  * @type Array
    //  */
    // selectionDashArray:     [ ],

    // /**
    //  * Color of the border of selection (usually slightly darker than color of selection itself)
    //  * @type String
    //  * @default
    //  */
    // selectionBorderColor:   'rgba(255, 255, 255, 0.3)',

    // /**
    //  * Width of a line used in object/group selection
    //  * @type Number
    //  * @default
    //  */
    // selectionLineWidth:     1,

    // /**
    //  * Default cursor value used when hovering over an object on canvas
    //  * @type String
    //  * @default
    //  */
    // hoverCursor:            'move',

    // /**
    //  * Default cursor value used when moving an object on canvas
    //  * @type String
    //  * @default
    //  */
    // moveCursor:             'move',

    // /**
    //  * Default cursor value used for the entire canvas
    //  * @type String
    //  * @default
    //  */
    // defaultCursor:          'default',

    // /**
    //  * Cursor value used during free drawing
    //  * @type String
    //  * @default
    //  */
    // freeDrawingCursor:      'crosshair',

    // /**
    //  * Cursor value used for rotation point
    //  * @type String
    //  * @default
    //  */
    // rotationCursor:         'crosshair',

    // /**
    //  * Default element class that's given to wrapper (div) element of canvas
    //  * @type String
    //  * @default
    //  */
    // containerClass:         'canvas-container',

    // /**
    //  * When true, object detection happens on per-pixel basis rather than on per-bounding-box
    //  * @type Boolean
    //  * @default
    //  */
    // perPixelTargetFind:     false,

    // /**
    //  * Number of pixels around target pixel to tolerate (consider active) during object detection
    //  * @type Number
    //  * @default
    //  */
    // targetFindTolerance:    0,

    // /**
    //  * When true, target detection is skipped when hovering over canvas. This can be used to improve performance.
    //  * @type Boolean
    //  * @default
    //  */
    // skipTargetFind:         false,

    // /**
    //  * Background color of canvas instance.
    //  * Should be set via {@link fabric.StaticCanvas#setBackgroundColor}.
    //  * @type {(String|fabric.Pattern)}
    //  * @default
    //  */
    // backgroundColor: '',

    // /**
    //  * Background image of canvas instance.
    //  * Should be set via {@link fabric.StaticCanvas#setBackgroundImage}.
    //  * <b>Backwards incompatibility note:</b> The "backgroundImageOpacity"
    //  * and "backgroundImageStretch" properties are deprecated since 1.3.9.
    //  * Use {@link fabric.Image#opacity}, {@link fabric.Image#width} and {@link fabric.Image#height}.
    //  * @type fabric.Image
    //  * @default
    //  */
    // backgroundImage: null,

    // /**
    //  * Overlay color of canvas instance.
    //  * Should be set via {@link fabric.StaticCanvas#setOverlayColor}
    //  * @since 1.3.9
    //  * @type {(String|fabric.Pattern)}
    //  * @default
    //  */
    // overlayColor: '',

    // /**
    //  * Overlay image of canvas instance.
    //  * Should be set via {@link fabric.StaticCanvas#setOverlayImage}.
    //  * <b>Backwards incompatibility note:</b> The "overlayImageLeft"
    //  * and "overlayImageTop" properties are deprecated since 1.3.9.
    //  * Use {@link fabric.Image#left} and {@link fabric.Image#top}.
    //  * @type fabric.Image
    //  * @default
    //  */
    // overlayImage: null,

    // /**
    //  * Indicates whether toObject/toDatalessObject should include default values
    //  * @type Boolean
    //  * @default
    //  */
    // includeDefaultValues: true,

    // /**
    //  * Indicates whether objects' state should be saved
    //  * @type Boolean
    //  * @default
    //  */
    // stateful: true,

    // /**
    //  * Indicates whether {@link fabric.Collection.add}, {@link fabric.Collection.insertAt} and {@link fabric.Collection.remove} should also re-render canvas.
    //  * Disabling this option could give a great performance boost when adding/removing a lot of objects to/from canvas at once
    //  * (followed by a manual rendering after addition/deletion)
    //  * @type Boolean
    //  * @default
    //  */
    // renderOnAddRemove: true,

    // /**
    //  * Function that determines clipping of entire canvas area
    //  * Being passed context as first argument. See clipping canvas area in {@link https://github.com/kangax/fabric.js/wiki/FAQ}
    //  * @type Function
    //  * @default
    //  */
    // clipTo: null,

    // /**
    //  * Indicates whether object controls (borders/controls) are rendered above overlay image
    //  * @type Boolean
    //  * @default
    //  */
    // controlsAboveOverlay: false,

    // /**
    //  * Indicates whether the browser can be scrolled when using a touchscreen and dragging on the canvas
    //  * @type Boolean
    //  * @default
    //  */
    // allowTouchScrolling: false,

    // /**
    //  * Indicates whether this canvas will use image smoothing, this is on by default in browsers
    //  * @type Boolean
    //  * @default
    //  */
    // imageSmoothingEnabled: true,

    // /**
    //  * Indicates whether objects should remain in current stack position when selected. When false objects are brought to top and rendered as part of the selection group
    //  * @type Boolean
    //  * @default
    //  */
    // preserveObjectStacking: false,

    ).asInstanceOf[CanvasOpts]
}
