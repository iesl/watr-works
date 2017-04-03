package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSGlobal

@js.native
trait StaticCanvasProperties extends js.Object {


  // backgroundColor:{(String|fabric.Pattern)} = '',
  def backgroundColor: String                  =  js.native // ''
  // backgroundImage:fabric.Image              = null,
  // overlayColor:{(String|fabric.Pattern)}    = '',
  // overlayImage:fabric.Image                 = null,
  var includeDefaultValues:Boolean             = js.native // true
  var stateful:Boolean                         = js.native // true
  var renderOnAddRemove: Boolean               = js.native
  // clipTo:Function                           = js.native // null,
  var controlsAboveOverlay:Boolean             = js.native // false
  var allowTouchScrolling:Boolean              = js.native // false
  var imageSmoothingEnabled:Boolean            = js.native // true
  var preserveObjectStacking:Boolean           = js.native // false
  var viewportTransform:Array[Int]             = js.native // [1, 0, 0, 1, 0, 0]
  var backgroundVpt:Boolean                    = js.native // true
  var overlayVpt:Boolean                       = js.native // true
  var svgViewportTransformation:Boolean        = js.native // true

}

@js.native
trait CanvasProperties extends StaticCanvasProperties {
  var uniScaleTransform:Boolean      = js.native //  false
  var uniScaleKey:String             = js.native //  "shiftKey"
  var centeredScaling:Boolean        = js.native //  false
  var centeredRotation:Boolean       = js.native //  false
  var centeredKey:String             = js.native //  "altKey"
  var altActionKey:String            = js.native //  "shiftKey"
  var lastRenderedKey:String         = js.native //  "altKey"
  var interactive:Boolean            = js.native //  true
  var selection:Boolean              = js.native //  true
  var selectionKey:String            = js.native //  "shiftKey"
  var selectionColor:String          = js.native //  "rgba(100, 100, 255, 0.3)" // blue
  var selectionDashArray: Array[Int] = js.native //  Array()
  var selectionBorderColor:String    = js.native //  "rgba(255, 255, 255, 0.3)"
  var selectionLineWidth:Number      = js.native //  1
  var hoverCursor:String             = js.native //  "move"
  var moveCursor:String              = js.native //  "move"
  var defaultCursor:String           = js.native //  "default"
  var freeDrawingCursor:String       = js.native //  "crosshair"
  var rotationCursor:String          = js.native //  "crosshair"
  var containerClass:String          = js.native //  "canvas-container"
  var perPixelTargetFind:Boolean     = js.native //  false
  var targetFindTolerance:Number     = js.native //  0
  var skipTargetFind:Boolean         = js.native //  false
  var isDrawingMode:Boolean          = js.native //  false
}



//// A bunch of methods/properties that all js object have
@js.native
trait JsElement extends js.Object {

  // def on(event: String, f:(Event) => Boolean): Event = js.native
  // def on(event: String, f:js.ThisFunction): Event = js.native
  def on(event: String, f:js.Function): Event = js.native
  def off(event: String, f:js.Function): Event = js.native
}


@js.native @JSGlobal("fabric.StaticCanvas")
class StaticCanvas(
  el: String,
  options: StaticCanvasProperties
) extends FabricCollection with StaticCanvasProperties with JsElement {

  // @param {HTMLElement | String} el &lt;canvas> element to initialize instance on
  // @param {Object} [options] Options object
  // @return {Object} thisArg
  // def initialize(el, options) : XXX = js.native


  // def onBeforeScaleRotate() : XXX = js.native



  // @param {HTMLElement | String} el &lt;canvas> element to initialize instance on
  // @param {Object} [options] Options object
  // @return {fabric.Canvas} instance
  // def calcOffset() : XXX = js.native


  // @param {(fabric.Image|String)} image fabric.Image instance or URL of an image to set overlay to
  // @param {Function} callback callback to invoke when image is loaded and set as an overlay
  // @param {Object} [options] Optional options to set for the {@link fabric.Image|overlay image}.
  // @return {fabric.Canvas} thisArg
  // def setOverlayImage(image, callback, options) : XXX = js.native


  // @param {(fabric.Image|String)} image fabric.Image instance or URL of an image to set background to
  // @param {Function} callback Callback to invoke when image is loaded and set as background
  // @param {Object} [options] Optional options to set for the {@link fabric.Image|background image}.
  // @return {fabric.Canvas} thisArg
  // def setBackgroundImage(image, callback, options) : XXX = js.native


  // @param {(String|fabric.Pattern)} overlayColor Color or pattern to set background color to
  // @param {Function} callback Callback to invoke when background color is set
  // @return {fabric.Canvas} thisArg
  def setOverlayColor(overlayColor: String, callback: js.Function): Canvas = js.native


  // @param {(String|fabric.Pattern)} backgroundColor Color or pattern to set background color to
  // @param {Function} callback Callback to invoke when background color is set
  // @return {fabric.Canvas} thisArg
  def setBackgroundColor(backgroundColor: String, callback: js.Function): Canvas = js.native


  // @param {String} property Property to set ({@link fabric.StaticCanvas#backgroundImage|backgroundImage}
  // @param {(fabric.Image|String|null)} image fabric.Image instance, URL of an image or null to set background or overlay to
  // @param {Function} callback Callback to invoke when image is loaded and set as background or overlay
  // @param {Object} [options] Optional options to set for the {@link fabric.Image|image}.
  // @param {String} property Property to set ({@link fabric.StaticCanvas#backgroundColor|backgroundColor}
  // @param {(Object|String|null)} color Object with pattern information, color value or null
  // @param {Function} [callback] Callback is invoked when color is set
  // @param {HTMLElement} element
  // @param {Object} [options] Options object
  // @param {HTMLElement} [canvasEl]

  def getWidth(): Int = js.native
  def getHeight() : Int = js.native



  // @param {Number|String} value                         Value to set width to
  // @param {Object}        [options]                     Options object
  // @param {Boolean}       [options.backstoreOnly=false] Set the given dimensions only as canvas backstore dimensions
  // @param {Boolean}       [options.cssOnly=false]       Set the given dimensions only as css dimensions
  def setWidth(value: Int, options: js.Object = js.Dynamic.literal(backstoreOnly=false, cssOnly=false)): Canvas = js.native



  // @param {Number|String} value                         Value to set height to
  // @param {Object}        [options]                     Options object
  // @param {Boolean}       [options.backstoreOnly=false] Set the given dimensions only as canvas backstore dimensions
  // @param {Boolean}       [options.cssOnly=false]       Set the given dimensions only as css dimensions
  def setHeight(value: Int, options: js.Object = js.Dynamic.literal(backstoreOnly=false, cssOnly=false)): Canvas = js.native


  // @param {Object}        dimensions                    Object with width/height properties
  // @param {Number|String} [dimensions.width]            Width of canvas element
  // @param {Number|String} [dimensions.height]           Height of canvas element
  // @param {Object}        [options]                     Options object
  // @param {Boolean}       [options.backstoreOnly=false] Set the given dimensions only as canvas backstore dimensions
  // @param {Boolean}       [options.cssOnly=false]       Set the given dimensions only as css dimensions
  def setDimensions(
    width: js.Object = js.Dynamic.literal(width=10, height=10),
    options: js.Object = js.Dynamic.literal(backstoreOnly=false, cssOnly=false)
  ): Canvas = js.native


  // @param {String} prop property (width|height)
  // @param {Number} value value to set property to
  // @return {fabric.Canvas} instance
  // @param {String} prop property (width|height)
  // @param {String} value value to set property to
  // @return {fabric.Canvas} instance
  // @return {Number}
  // def getZoom() : XXX = js.native


  // @param {Array} vpt the transform in the form of context.transform
  // @return {fabric.Canvas} instance
  // def setViewportTransform(vpt) : XXX = js.native


  // @param {fabric.Point} point to zoom with respect to
  // @param {Number} value to set zoom to, less than 1 zooms out
  // @return {fabric.Canvas} instance
  // def zoomToPoint(point, value) : XXX = js.native


  // @param {Number} value to set zoom to, less than 1 zooms out
  // @return {fabric.Canvas} instance
  // def setZoom(value) : XXX = js.native


  // @param {fabric.Point} point to move to
  // @return {fabric.Canvas} instance
  // def absolutePan(point) : XXX = js.native


  // @param {fabric.Point} point (position vector) to move by
  // @return {fabric.Canvas} instance
  // def relativePan(point) : XXX = js.native


  // @return {HTMLCanvasElement}
  // def getElement() : XXX = js.native


  // @return {fabric.Object}
  // def getActiveObject() : XXX = js.native


  // @return {fabric.Group}
  // def getActiveGroup() : XXX = js.native


  // @param {fabric.Object} obj Object that was added
  // @param {fabric.Object} obj Object that was removed
  // @param {CanvasRenderingContext2D} ctx Context to clear
  // @return {fabric.Canvas} thisArg
  // def clearContext(ctx) : XXX = js.native


  // @return {CanvasRenderingContext2D}
  // def getContext() : XXX = js.native


  // @return {fabric.Canvas} thisArg
  def clear() : Canvas = js.native


  // @param {Boolean} [allOnTop] Whether we want to force all images to be rendered on the top canvas
  // @return {fabric.Canvas} instance
  def renderAll(allOnTop: Boolean=false): Canvas = js.native


  // @param {CanvasRenderingContext2D} ctx Context to render on
  // @param {Array} objects to render
  // @param {CanvasRenderingContext2D} ctx Context to render on
  // @param {string} property 'background' or 'overlay'
  // @param {CanvasRenderingContext2D} ctx Context to render on
  // @param {CanvasRenderingContext2D} ctx Context to render on
  // @return {fabric.Canvas} thisArg
  // def renderTop() : XXX = js.native


  // @return {Object} object with "top" and "left" number values
  // def getCenter() : XXX = js.native


  // @param {fabric.Object} object Object to center horizontally
  // @return {fabric.Canvas} thisArg
  // def centerObjectH(object) : XXX = js.native


  // @param {fabric.Object} object Object to center vertically
  // @return {fabric.Canvas} thisArg
  // def centerObjectV(object) : XXX = js.native


  // @param {fabric.Object} object Object to center vertically and horizontally
  // @return {fabric.Canvas} thisArg
  // def centerObject(object) : XXX = js.native


  // @param {fabric.Object} object Object to center vertically and horizontally
  // @return {fabric.Canvas} thisArg
  // def viewportCenterObject(object) : XXX = js.native


  // @param {fabric.Object} object Object to center vertically and horizontally
  // @return {fabric.Canvas} thisArg
  // def viewportCenterObjectH(object) : XXX = js.native


  // @param {fabric.Object} object Object to center vertically and horizontally
  // @return {fabric.Canvas} thisArg
  // def viewportCenterObjectV(object) : XXX = js.native


  // @return {fabric.Point} vpCenter, viewport center
  // def getVpCenter() : XXX = js.native


  // @param {fabric.Object} object Object to center
  // @param {fabric.Point} center Center point
  // @return {fabric.Canvas} thisArg
  // @param {Array} [propertiesToInclude] Any properties that you might want to additionally include in the output
  // @return {String} json string
  // def toDatalessJSON(propertiesToInclude) : XXX = js.native


  // @param {Array} [propertiesToInclude] Any properties that you might want to additionally include in the output
  // @return {Object} object representation of an instance
  // def toObject(propertiesToInclude) : XXX = js.native


  // @param {Array} [propertiesToInclude] Any properties that you might want to additionally include in the output
  // @return {Object} object representation of an instance
  // def toDatalessObject(propertiesToInclude) : XXX = js.native



  // @param {fabric.Object} [instance] the object to transform (gets mutated)
  // @returns the original values of instance which were changed
  // @param {fabric.Object} [instance] the object to un-transform (gets mutated)
  // @param {Object} [originalValues] the original values of instance, as returned by _realizeGroupTransformOnObject
  // @param {Object} [options] Options object for SVG output
  // @param {Boolean} [options.suppressPreamble=false] If true xml tag is not included
  // @param {Object} [options.viewBox] SVG viewbox object
  // @param {Number} [options.viewBox.x] x-cooridnate of viewbox
  // @param {Number} [options.viewBox.y] y-coordinate of viewbox
  // @param {Number} [options.viewBox.width] Width of viewbox
  // @param {Number} [options.viewBox.height] Height of viewbox
  // @param {String} [options.encoding=UTF-8] Encoding of SVG output
  // @param {String} [options.width] desired width of svg with or without units
  // @param {String} [options.height] desired height of svg with or without units
  // @param {Function} [reviver] Method for further parsing of svg elements, called after each fabric object converted into svg representation.
  // @return {String} SVG string
  // def toSVG(options, reviver) : XXX = js.native


  def sendToBack(o: FabricObject): Canvas = js.native
  def bringToFront(o: FabricObject): Canvas = js.native


  // @param {fabric.Object} object Object to send
  // @param {Boolean} [intersecting] If `true`, send object behind next lower intersecting object
  def sendBackwards(o: FabricObject, intersecting: Boolean): Canvas = js.native


  // @param {fabric.Object} object Object to send
  // @param {Boolean} [intersecting] If `true`, send object in front of next upper intersecting object
  // @return {fabric.Canvas} thisArg
  def bringForward(o: FabricObject, intersecting: Boolean): Canvas = js.native


  // @param {fabric.Object} object Object to send
  // @param {Number} index Position to move to
  def moveTo(o: FabricObject, index: Int): Canvas = js.native


  // @return {fabric.Canvas} thisArg
  def dispose(): Canvas = js.native


  // @param {String} methodName Method to check support for;
  // @return {Boolean | null} `true` if method is supported (or at least exists),
  // def supports(methodName) : XXX = js.native

}

@js.native @JSGlobal("fabric.Canvas")
class Canvas(
  el: String,
  options: CanvasProperties
) extends StaticCanvas(el, options) with CanvasProperties {

  // @param {HTMLElement | String} el &lt;canvas> element to initialize instance on
  // @param {Object} [options] Options object
  // @return {Object} thisArg
  // def initialize(el, options)  = js.native


  // @param {Event} e Event object fired on mousemove
  // @param {Event} e Event object
  // @param {fabric.Object} target Object to test against
  // @param {Object} [point] x,y object of point coordinates we want to check.
  // @return {Boolean} true if point is contained within an area of given object
  // def containsPoint(e, target, point)  = js.native


  // @param {fabric.Object} target Object to check
  // @param {Number} x Left coordinate
  // @param {Number} y Top coordinate
  // @return {Boolean}
  // def isTargetTransparent(target, x, y)  = js.native


  // @param {Event} e Event object
  // @param {fabric.Object} target
  // @param {fabric.Object} target
  // @param {Event} e Event object
  // @param {fabric.Object} target
  // @param {Number} x pointer's x coordinate
  // @param {Number} y pointer's y coordinate
  // @return {Boolean} true if the translation occurred
  // @param {Number} x pointer's x coordinate
  // @param {Number} y pointer's y coordinate
  // @param {String} by Either 'x' or 'y'
  // @return {Boolean} true if the skewing occurred
  // @return {Boolean} true if the skewing occurred
  // @param {Number} x pointer's x coordinate
  // @param {Number} y pointer's y coordinate
  // @param {String} by Either 'x' or 'y' - specifies dimension constraint by which to scale an object.
  // @return {Boolean} true if the scaling occurred
  // @return {Boolean} true if the scaling occurred
  // @return {Boolean} true if the scaling occurred
  // @param {Number} x pointer's x coordinate
  // @param {Number} y pointer's y coordinate
  // @return {Boolean} true if the rotation occurred

  // @param {String} value Cursor type of the canvas element.
  def setCursor(value: String): Unit  = js.native


  // @param {Event} e mouse event
  // @param {Boolean} skipGroup when true, activeGroup is skipped and only objects are traversed through
  // def findTarget(e, skipGroup)  = js.native


  // @param {Event} e
  // @return {Object} object with "x" and "y" number values
  // def getPointer(e, ignoreZoom, upperCanvasEl)  = js.native


  // @param {HTMLElement} element canvas element to apply styles on
  // @param {Element} fromEl Element style is copied from
  // @param {Element} toEl Element copied style is applied to
  // @return {CanvasRenderingContext2D}
  // def getSelectionContext()  = js.native


  // @return {HTMLCanvasElement}
  // def getSelectionElement()  = js.native


  // @param {Object} object
  // @param {fabric.Object} object Object to set as an active one
  // @param {Event} [e] Event (passed along when firing "object:selected")
  // @return {fabric.Canvas} thisArg
  // def setActiveObject(object, e)  = js.native


  // @return {fabric.Object} active object
  // def getActiveObject()  = js.native


  // @return {fabric.Canvas} thisArg
  // def discardActiveObject(e)  = js.native


  // @param {fabric.Group} group
  // @param {fabric.Group} group Group to set as a current one
  // @return {fabric.Canvas} thisArg
  // def setActiveGroup(group, e)  = js.native


  // @return {fabric.Group} Current group
  // def getActiveGroup()  = js.native


  // @return {fabric.Canvas} thisArg
  // def discardActiveGroup(e)  = js.native


  // @return {fabric.Canvas} thisArg
  // def deactivateAll()  = js.native


  // @return {fabric.Canvas} thisArg
  // def deactivateAllWithDispatch(e)  = js.native


  // @return {fabric.Canvas} thisArg
  // def dispose()  = js.native


  // @param {CanvasRenderingContext2D} ctx Context to render controls on
  // def drawControls(ctx)  = js.native



  // @param {fabric.Object} obj Object that was removed
  // @return {fabric.Canvas} thisArg
  // def clear(): Canvas  = js.native


}


object Canvas {
  def apply(element: String, options: CanvasProperties): Canvas = new Canvas(
    element, options
  )

  def static(element: String, options: StaticCanvasProperties): StaticCanvas = new StaticCanvas(
    element, options
  )

}

@js.native @JSGlobal
object CanvasOptions extends CanvasProperties//  {}

// @js.native @JSGlobal
// object StaticCanvasOptions extends CanvasProperties//  {}
