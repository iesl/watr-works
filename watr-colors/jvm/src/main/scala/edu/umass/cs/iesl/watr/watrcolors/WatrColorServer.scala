package edu.umass.cs.iesl.watr
package watrcolors

import upickle.default._
import upickle.Js
import spray.routing.SimpleRoutingApp
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext.Implicits.global
import spray.http.{MediaTypes, HttpEntity}


object AutowireServer extends autowire.Server[Js.Value, Reader, Writer]{
  def read[Result: Reader](p: Js.Value) = upickle.default.readJs[Result](p)
  def write[Result: Writer](r: Result) = upickle.default.writeJs(r)
}


object WatrColorServer extends SimpleRoutingApp with WatrColorApiServer {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    val _ = startServer("0.0.0.0", port = 8080) {
      get{
        pathSingleSlash {
          complete{
            HttpEntity(
              MediaTypes.`text/html`,
              html.Template.txt.toString()
            )
          }
        } ~
        pathPrefix("webjars") {
          get {
            getFromResourceDirectory("META-INF/resources/webjars")
          }
        } ~
        getFromResourceDirectory("")
      } ~
      post {
        path("api" / Segments){ s =>
          extract(_.request.entity.asString) { e =>
            complete {
              AutowireServer.route[WatrColorApi](WatrColorServer)(
                autowire.Core.Request(
                  s,
                  upickle.json.read(e).asInstanceOf[Js.Obj].value.toMap
                )
              ).map(upickle.json.write)
            }
          }
        }
      }
    }
  }

}
