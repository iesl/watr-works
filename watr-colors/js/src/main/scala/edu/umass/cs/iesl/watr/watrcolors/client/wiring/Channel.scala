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


// trait CanvasMouseChannels {
//   type ME = fabric.Options

//   def canvas: fabric.Canvas

//   val mousemove = new Channel[ME](canvas.on("mouse:move", _))
//   val mouseup = new Channel[ME](canvas.on("mouse:up", _))
//   val mousedown = new Channel[ME](canvas.on("mouse:down", _))
//   // val mouseover = new Channel[ME](canvas.on("mouse:over", _))
//   // val mouseout = new Channel[ME](canvas.on("mouse:out", _))
// }


// object CanvasMouseChannels {
//   def apply(c: fabric.Canvas) = new CanvasMouseChannels {
//     override def canvas = c
//   }

// }


// import org.scalajs.dom
// import org.scalajs.dom.html

// trait ElemMouseChannels {
//   type ME = dom.MouseEvent

//   def elem: html.Element

//   val mousemove = new Channel[ME](elem.onmousemove = _)
//   val mouseup = new Channel[ME](elem.onmouseup = _)
//   val mousedown = new Channel[ME](elem.onmousedown = _)


// }

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
