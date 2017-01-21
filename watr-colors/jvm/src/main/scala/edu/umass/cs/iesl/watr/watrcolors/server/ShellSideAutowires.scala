package edu.umass.cs.iesl.watr
package watrcolors
package server


import upickle.{default => UPickle}
import UPickle._

import scala.concurrent.Future

import akka.actor.ActorRef
import akka.actor.ActorDSL._


class ShellsideClient(pollActor: ActorRef) extends autowire.Client[String, UPickle.Reader, UPickle.Writer] {

  override def doCall(req: Request)= {
    val reqArgs = req.args.toList.map({case (param, arg) =>
      (param -> arg)
    })

    val rc = RemoteCall(req.path.toList, reqArgs)

    pollActor ! rc

    Future.successful("Ok")
  }

  override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
  override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)
}

// trait ServerWire {
//   def wire(incoming: String): Unit
// }

object ShellsideServer extends autowire.Server[String, UPickle.Reader, UPickle.Writer] {
  // def routes: autowire.Core.Router[String] =
  //   route[WatrColorsApi](serverImpl)
  // def routes = route[WatrShellApi](serverImpl)

  // def wire(incoming: String): Unit = {
  //   val remoteCalls = UPickle.read[List[RemoteCall]](incoming)
  //   remoteCalls.foreach { case RemoteCall(callPath, callArgs) =>
  //     val req = new Request(callPath, callArgs.toMap)
  //     println(s"Web: WatrTable Call recv'd ${req}")
  //     // println(s"Web: WatrTable Call complete")
  //     routes.apply(req)
  //   }
  // }

  override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)
  override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
}
