package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSName


@js.native @JSName("fabric.Image")
class Image(
  options: js.Object
) extends FabricObject {}

object Image {
  def fromURL(
    url: String,
    cb: (Image)=> Unit,
    err: ()=> Unit,
    options: js.Object = js.Dynamic.literal()
  ): Unit = {

    val jscb: js.Function = (image: Image) =>  {
      if (image!=null) {
        val image = new Image(options)
        cb(image)
      } else {
        err()
      }
    }

    util.loadImage(url, jscb) 
  }
}

// fabric.util.loadImage('https://s3-eu-west-1.amazonaws.com/kienzle.dev.cors/img/image2.png', function(img) {
//     if(img == null) {
//       alert("Error!");
//     }else {
//       var image = new fabric.Image(img);
//       canvas.add(image).setActiveObject(image);
//       canvas.renderAll();
//     }
// }, { crossOrigin: 'anonymous' });
