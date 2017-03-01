package edu.umass.cs.iesl.watr
package watrcolors
package client

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

import native.fabric

trait CanvasMouseChannels {
  type ME = fabric.Options

  def canvas: fabric.Canvas


  val mousemove = new Channel[ME](canvas.on("mouse:move", _))
  val mouseup = new Channel[ME](canvas.on("mouse:up", _))
  val mousedown = new Channel[ME](canvas.on("mouse:down", _))
  val mouseover = new Channel[ME](canvas.on("mouse:over", _))
  val mouseout = new Channel[ME](canvas.on("mouse:out", _))


  def teardown(): Unit = {
    // TODO remove listeners when no longer needed
    // not sure if this is leaking resources

  }

}


object CanvasMouseChannels {
  def apply(c: fabric.Canvas) = new CanvasMouseChannels {
    override def canvas = c

  }


  // def handlerTemlate(c: fabric.Canvas): Unit = {
  //   val _ = async {
  //     val chan = CanvasMouseChannels(c)

  //     while(true){
  //       val start = await(chan.mousedown())
  //       var res = await(chan.mousemove | chan.mouseup)
  //       while(res.`type` == "mousemove"){
  //         res = await(chan.mousemove | chan.mouseup)
  //       }
  //       await(chan.mouseup())
  //     }
  //   }
  // }
}

