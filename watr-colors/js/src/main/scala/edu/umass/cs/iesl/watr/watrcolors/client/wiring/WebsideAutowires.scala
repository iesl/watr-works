package edu.umass.cs.iesl.watr
package watrcolors
package client
package wiring

import upickle.{default => UPickle}
import UPickle._
import TypeTagPicklers._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.concurrent.Future
import org.scalajs.dom


class WebsideClient(prefix: String) extends autowire.Client[String, UPickle.Reader, UPickle.Writer] {
  override def doCall(req: Request): Future[String] = {
    dom.ext.Ajax
      .post(url = s"""/api/${prefix}/${req.path.mkString("/")}""",
        data = write(req.args),
        headers = Map("Content-Type" -> "text/plain"))
      .map(_.responseText)

  }

  override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)
  override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
}


class WebsideServer(serverImpl: WatrColorsApi) extends autowire.Server[String, UPickle.Reader, UPickle.Writer] {
  def routes: autowire.Core.Router[String] =
    route[WatrColorsApi](serverImpl)

  def wire(incoming: String): Unit = {

    val remoteCalls = UPickle.read[List[RemoteCall]](incoming)

    remoteCalls.foreach { case RemoteCall(callPath, callArgs) =>
      val req = new Request(callPath, callArgs.toMap)
      routes.apply(req)
    }
  }

  override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)
  override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
}
