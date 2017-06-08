package edu.umass.cs.iesl.watr
package watrcolors
package server

import akka.actor._
import akka.util.Timeout
import akka.pattern.{ ask, pipe }
import akka.event.Logging
import concurrent.duration._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import corpora._
import docstore._

import upickle.{default => UPickle}
import UPicklers._

case class RoutingRequest(
  user: UserData, path: List[String], body: String
)
case class RoutingResponse(
  response: Future[String]
)

object UserSessionActor {
  def props(user: UserData, bioArxivServer: BioArxivServer): Props =
    Props(new UserSessionActor(user, bioArxivServer))
}

class UserSessionActor(
  user: UserData,
  bioArxivServer: BioArxivServer
) extends Actor {
  val log = Logging(context.system, this)

  import TypeTagPicklers._
  val router = ShellsideServer.route[WatrShellApi](bioArxivServer)

  def route(path: List[String], msgBody: String): Future[String] = {
    router(
      autowire.Core.Request(
        path,
        UPickle.read[Map[String, String]](msgBody)
      )
    )
  }

  def receive = {
    case req@ RoutingRequest(user, path, body) =>
      sender() ! RoutingResponse(route(path, body))

  }
}
object SessionsActor {
  def props(reflowDB: TextReflowDB, corpus: Corpus): Props =
    Props(new SessionsActor(reflowDB, corpus))
}


class SessionsActor(
  reflowDB: TextReflowDB,
  corpus: Corpus
    // Add ref to workflows here??
) extends Actor {
  val log = Logging(context.system, this)

  implicit val timeout = Timeout(20.seconds)

  def receive = {
    case req@ RoutingRequest(user, path, body) =>

      val userSessionActor = context.child(user.emailAddr.unwrap).getOrElse {
        println(s"Sessions: creating new actor for ${user.emailAddr}")
        context.actorOf(UserSessionActor.props(user, new BioArxivServer(user, reflowDB, corpus)), user.emailAddr.unwrap)
      }
      println(s"Sessions: using actor for ${user.emailAddr}")

      val resp = userSessionActor ? req

      resp.pipeTo(sender())

  }
}
