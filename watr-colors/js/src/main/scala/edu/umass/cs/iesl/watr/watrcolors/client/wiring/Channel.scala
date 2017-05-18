package edu.umass.cs.iesl.watr
package watrcolors
package client
package wiring

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent.Promise

class Channel[T](init: (T => Unit) => Unit){

  init(update)

  private[this] var value: Promise[T] = null

  def apply(): Future[T] = {
    value = Promise[T]()
    value.future
  }

  def update(t: T): Unit = {
    if (value != null && !value.isCompleted) {
      val _ = value.success(t)
    }
  }

  def |(other: Channel[T]): Future[T] = {
    val p = Promise[T]()
    for{
      f <- Seq(other(), this())
      t <- f
    } p.trySuccess(t)
    p.future
  }
}

// import scala.scalajs.js
// import scala.scalajs.js.`|`
import org.scalajs.dom
// import org.singlespaced.d3js.d3


trait D3MouseChannels {
  type ME = dom.MouseEvent

  def elem: dom.html.Element

  val mousemove = new Channel[ME](elem.onmousemove = _)
  val mouseup = new Channel[ME](elem.onmouseup = _)
  val mousedown = new Channel[ME](elem.onmousedown = _)


  // get coords relative to target
  // d3.mouse(container: EventTarget)
  // val mousemove = new Channel[ME](canvas.on("mouse:move", _))
  // val mouseup = new Channel[ME](canvas.on("mouse:up", _))
  // val mousedown = new Channel[ME](canvas.on("mouse:down", _))
  // val mouseover = new Channel[ME](canvas.on("mouse:over", _))
  // val mouseout = new Channel[ME](canvas.on("mouse:out", _))
}

object D3MouseChannels {
  def apply(elem0: dom.html.Element) = new D3MouseChannels {
    override def elem = elem0
  }
}

// import org.scalajs.dom
// import org.scalajs.dom.html

// object ElemMouseChannels {
//   def apply(elem: html.Element) = new ElemMouseChannels {
//     override def elem = elem
//   }
//   // async
//   // def rect = canvas.getBoundingClientRect()


//   // // Disabled due to scala-js#1469
//   // async{
//   //   while(true){
//   //     val start = await(mousedown())
//   //     renderer.beginPath()
//   //     renderer.moveTo(
//   //       start.clientX - rect.left,
//   //       start.clientY - rect.top
//   //     )

//   //     var res = await(mousemove | mouseup)
//   //     while(res.`type` == "mousemove"){
//   //       renderer.lineTo(
//   //         res.clientX - rect.left,
//   //         res.clientY - rect.top
//   //       )
//   //       renderer.stroke()
//   //       res = await(mousemove | mouseup)
//   //     }

//   //     renderer.fill()
//   //     await(mouseup())
//   //     renderer.clearRect(0, 0, 1000, 1000)
//   //   }
//   // }

// }
