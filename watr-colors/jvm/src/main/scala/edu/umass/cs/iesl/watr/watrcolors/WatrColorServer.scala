package edu.umass.cs.iesl.watr
package watrcolors

import akka.util.ByteString
import java.nio.ByteBuffer
import scala.concurrent.Future
import scala.reflect.ClassTag
import spray.http.HttpData
import spray.http.Uri.Path
import spray.routing.SimpleRoutingApp
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext.Implicits.global
import spray.http.{MediaTypes, HttpEntity}
import better.files._

import boopickle.DefaultBasic._
import Picklers._

object AutowireServer extends autowire.Server[ByteBuffer, Pickler, Pickler] {
  override def read[R: Pickler](p: ByteBuffer) = Unpickle[R].fromBytes(p)
  override def write[R: Pickler](r: R) = Pickle.intoBytes(r)
}

object WatrColorServer extends SimpleRoutingApp {

  val svgRepoPath = "../svg-repo"

  def jsonResponse(resp: String) = {
    HttpEntity(MediaTypes.`application/json`, resp)
  }

  def httpResponse(resp: String) = {
    HttpEntity(MediaTypes.`text/html`, resp)
  }

  def webjarResources = pathPrefix("webjars") {
    getFromResourceDirectory("META-INF/resources/webjars")
  }

  def rootResources =
    getFromResourceDirectory("")

  def svgRepo = pathPrefix("svg-repo") {
    getFromDirectory(svgRepoPath)
  }

  // Helper function for constructing Autowire routes
  def apiRoute(
    prefix: String,
    router: autowire.Core.Router[java.nio.ByteBuffer] // (which expands to)  PartialFunction[autowire.Core.Request[java.nio.ByteBuffer],scala.concurrent.Future[java.nio.ByteBuffer]]
  ) = {
    pathPrefix("api") {
      path(prefix / Segments) { segs =>
        extract(_.request.entity.data) { requestData => ctx =>
          ctx.complete(
            router(
              autowire.Core.Request(segs,
                Unpickle[Map[String, ByteBuffer]].fromBytes(requestData.toByteString.asByteBuffer)
              )
            ).map(responseData =>
              HttpEntity(HttpData(ByteString(responseData)))
            )
          )
        }
      }
    }
  }


  def appendToPathx(path: Path, ext: String) = path match {
    case s @ Path.Segment(head, tail) if head.startsWith("456") =>
      val newHead = head.drop(3)
      if (newHead.isEmpty) tail
      else s.copy(head = head.drop(3))
    case _ => path
  }

  def appendToPath(path: Path, ext: String): Path = path match {
    case s @ Path.Segment(head, tail) =>
      s

    case s @ Path.Slash(tail) =>
      val rewrite = tail.toString().dropRight(4) + ".json"
      // val rewrite = tail.dropChars(4) + ".json"
      println(s"got default path ${s}, rewrote to ${rewrite}, tail was ${tail} ")

      s.copy(tail = Path(rewrite))
    case s => s
  }

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem()
    val _ = startServer("0.0.0.0", port = 8080) {
      get {
        pathSingleSlash {
          complete { httpResponse(html.Frame().toString()) }
        } ~ pathPrefix("annotations") {
          rewriteUnmatchedPath({ unmatched =>
            println("rewriting" + unmatched)
            appendToPath(unmatched, ".json")
          }) {
            path(Rest) { annot =>
              val annotPath = svgRepoPath / annot
              getFromFile(annotPath.toJava, MediaTypes.`application/json`)
            }
          }
        } ~
          webjarResources ~
          rootResources ~
          svgRepo
      } ~ post {
        apiRoute("explorer", AutowireServer.route[CorpusExplorerApi](CorpusExplorerServer)) ~
        apiRoute("svg", AutowireServer.route[SvgOverviewApi](SvgOverviewServer))
      } ~ put {
        pathPrefix("annotations") {
          rewriteUnmatchedPath(appendToPath(_, ".json")) {
            path(Rest) { annot =>
              extract(_.request.entity.asString) { e =>
                complete {
                  println(s"writing annotation file ${annot}")

                  // req.request.entity.data
                  println(s"put request was ${e}")

                  "ok"

                  // getFromDirectory(annotPath.toString)
                  // val annotPath = svgRepoPath /  annot
                  // getFromFile(annotPath.toJava, MediaTypes.`application/json`)
                }
              }
            }
          }
        }
      }
    }
  }


  // /**
  //  * routing methods
  //  */

  // public static Result showIndex() throws IOException {
  //     List<Document> docs = svgFileDAO.getDocuments();
  //     return ok(views.html.index.render(JavaConversions.asScalaBuffer(docs)));
  // }

  // public static Result editDocument(String fileName) throws IOException {
  //     Document doc = svgFileDAO.getDocument(fileName);
  //     if (doc == null) {
  //         return notFound("fileName not found: '" + fileName + "'");
  //     }
  //     return ok(views.html.edit.render(doc));
  // }

  // /**
  //  * API methods
  //  */

  // /**
  //  * Receives GET requests to retrieve a JSON list of lines (strings) corresponding to the passed rect.
  //  * <p>
  //  * TODO!
  //  */
  // public static Result getDocRectText(String fileName, String x, String y, String width, String height) {
  //     String postfix = " (" + x + "," + y + "," + width + "," + height + ")";
  //     List<String> textLines = Arrays.asList("fake line 1/2" + postfix, "fake line 2/2" + postfix);
  //     return ok(Json.toJson(textLines));
  // }

  // /**
  //  * Receives PUT requests to save a list of annotations on fileName. the PUT body is a JSON list of serialized
  //  * Annotation objects of the form: [{"label":"title", "rects":[{"x":100, "y":100, "width":400, "height":50}]}, ...]
  //  *   ...
  //  * ]
  //  */
  // public static Result saveDocument(String fileName) throws IOException {
  //     Document doc = svgFileDAO.getDocument(fileName);
  //     if (doc == null) {
  //         return notFound("fileName not found: '" + fileName + "'");
  //     }

  //     JsonNode annotationsJsonNode = request().body().asJson();
  //     System.out.println("saveDocument(): " + fileName + ": " + annotationsJsonNode);
  //     List<Annotation> annotations = Document.deserializeAnnotationListFromJson(annotationsJsonNode);
  //     doc.clearAnnotations();
  //     doc.addAnnotations(annotations);
  //     svgFileDAO.writeDocument(doc);
  //     return play.mvc.Results.ok("");     // NB: ok() causes JQuery $.ajax() error - 'no element found'
  // }

}
