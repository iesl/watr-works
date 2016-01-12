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


object WatrColorServer extends SimpleRoutingApp  {


  def httpResponse(resp: html.TextTag) = {
    // val page= html.Frame(resp).to
    HttpEntity(
      MediaTypes.`text/html`,
      html.Frame().toString
    )
  }


  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    val _ = startServer("0.0.0.0", port = 8080) {
      get{
        pathSingleSlash {
          // complete{ httpResponse(html.Template.txt) }
          complete{ httpResponse(html.CorpusExplorerPane.init()) }
        } ~
        pathPrefix("webjars") {
          get {
            getFromResourceDirectory("META-INF/resources/webjars")
          }
        } ~
        getFromResourceDirectory("") // contents of src/main/resources
      } ~
      post {
        pathPrefix("api") {

          path("explorer" / Segments){ s =>
            extract(_.request.entity.asString) { e =>
              complete {
                AutowireServer.route[CorpusExplorerApi](CorpusExplorerServer)(
                  autowire.Core.Request(
                    s,
                    upickle.json.read(e).asInstanceOf[Js.Obj].value.toMap
                  )
                ).map(upickle.json.write)
              }
            }
          }
          // path("api" / Segments){ s =>
          //   extract(_.request.entity.asString) { e =>
          //     complete {
          //       AutowireServer.route[WatrColorApi](WatrColorServer)(
          //         autowire.Core.Request(
          //           s,
          //           upickle.json.read(e).asInstanceOf[Js.Obj].value.toMap
          //         )
          //       ).map(upickle.json.write)
          //     }
          //   }
          // }


        }
      }
    }
  }





    // static SvgFileDAO svgFileDAO;
    // static {
    //     File publicImagesDir = Play.application().getFile("/public/svg-repo");  // full path
    //     String svgDirectory = publicImagesDir.getAbsolutePath();
    //     svgFileDAO = new SvgFileDAO(svgDirectory);
    //     System.out.println("SvgFileDAO initialized on directory: '" + svgDirectory + "'");
    // }


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
    //  * Receives GET requests to retrieve a JSON dict of annotation objects for fileName.
    //  */
    // public static Result getAnnotations(String fileName) throws IOException {
    //     Document doc = svgFileDAO.getDocument(fileName);
    //     if (doc == null) {
    //         return notFound("fileName not found: '" + fileName + "'");
    //     }

    //     JsonNode annotsJson = doc.serializeAnnotationsToJson();
    //     return ok(annotsJson.toString());
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
