package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSGlobal

object Text{
  def apply(text: String): Text = new Text(
    text,
    js.Dynamic.literal()
  )
}

@js.native @JSGlobal("fabric.Text")
class Text(text:String, options: js.Object) extends FabricObject {

  def getText(): String = js.native

  def setText(text:String): Text  = js.native

  // @return {String} Font size (in pixels)
  def getFontSize(): String  = js.native

  // // @param {Number} fontSize Font size (in pixels)
  def setFontSize(fontSize: Int): Text = js.native

  // def getFontWeight()
  // def @return {(String|Number)} Font weight

  // def setFontWeight
  // def @param {(Number|String)} fontWeight Font weight
  // def @return {fabric.Text}

  // def getFontFamily
  // def @return {String} Font family

  // def setFontFamily
  // def @param {String} fontFamily Font family
  // def @return {fabric.Text}


  // def getTextDecoration
  // def @return {String} Text decoration

  // def setTextDecoration
  // def @param {String} textDecoration Text decoration
  // def @return {fabric.Text}

  // def getFontStyle
  // def @return {String} Font style

  // def setFontStyle
  // def @param {String} fontStyle Font style
  // def @return {fabric.Text}

  // def @return {Number} Line height
  def getLineHeight(): Int = js.native

  // def @param {Number} lineHeight Line height
  def setLineHeight(h: Float): Text = js.native

  // def getTextAlign
  // def @return {String} Text alignment

  // def setTextAlign
  // def @param {String} textAlign Text alignment
  // def @return {fabric.Text}

  // def getTextBackgroundColor
  // def @return {String} Text background color

  // def setTextBackgroundColor
  // def @param {String} textBackgroundColor Text background color
  // def @return {fabric.Text}

  // def @param {String} text Text string
  // def @param {Object} [options] Options object
  // def @return {fabric.Text} thisArg

}
