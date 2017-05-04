package edu.umass.cs.iesl.watr
package watrcolors
package client
package wiring

import upickle.{default => UPickle}
import UPickle._
import UPicklers._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.concurrent.Future
import org.scalajs.dom


class WebsideClient(prefix: String) extends autowire.Client[String, UPickle.Reader, UPickle.Writer] {
  override def doCall(req: Request): Future[String] = {
    dom.ext.Ajax
      .post(url = s"""/autowire/api/${prefix}/${req.path.mkString("/")}""",
        data = write(req.args),
        headers = Map("Content-Type" -> "text/plain"))
      .map(_.responseText)

  }

  override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)
  override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
}


