package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js

@js.native
trait FabricCollection extends js.Object {

  // @param {...fabric.Object} object Zero or more fabric instances
  // @return {Self} thisArg
  def add(objs: FabricObject*): this.type = js.native

  // @param {Object} object Object to insert
  // @param {Number} index Index to insert object at
  // @param {Boolean} nonSplicing When `true`, no splicing (shifting) of objects occurs
  // @return {Self} thisArg
  // def insertAt(object, index, nonSplicing)  = js.native

  // @param {...fabric.Object} object Zero or more fabric instances
  // @return {Self} thisArg
  def remove(fs: FabricObject*): Unit = js.native

  // @param {Function} callback
  // @param {Object} context Context (aka thisObject)
  // @return {Self} thisArg
  // def forEachObject(callback, context)  = js.native
  def forEachObject(cb: js.Function): Unit = js.native

  // @param {String} [type] When specified, only objects of this type are returned
  // @return {Array}
  // def getObjects(type)  = js.native

  // @param {Number} index
  // @return {Self} thisArg
  // def item(index)  = js.native

  // @return {Boolean} true if collection is empty
  // def isEmpty()  = js.native

  // @return {Number} Collection size
  // def size()  = js.native

  // @param {Object} object Object to check against
  // @return {Boolean} `true` if collection contains an object
  // def contains(object)  = js.native

  // @return {Number} complexity
  // def complexity()  = js.native

}
