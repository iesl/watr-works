package edu.umass.cs.iesl.watr
package watrcolors
package client

// import java.nio.ByteBuffer
// import org.scalajs.dom
// import scala.scalajs.js.typedarray.{ ArrayBuffer, TypedArrayBuffer }
// import scala.concurrent.Future
// import scalajs.concurrent.JSExecutionContext.Implicits.queue
// import scalatags.JsDom.all._

// import boopickle.Default._

// case class ServerWire(prefix: String) extends autowire.Client[ByteBuffer, Pickler, Pickler] {
//   override def doCall(req: Request): Future[ByteBuffer] = {
//     dom.ext.Ajax.post(
//       url = s"""/api/${prefix}/${req.path.mkString("/")}""",
//       data = Pickle.intoBytes(req.args),
//       responseType = "arraybuffer",
//       headers = Map("Content-Type" -> "application/octet-stream")
//     ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
//   }

//   override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)
//   override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)
// }
