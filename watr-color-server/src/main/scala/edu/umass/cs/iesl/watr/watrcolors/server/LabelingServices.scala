package edu.umass.cs.iesl.watr
package watrcolors
package server


import org.http4s._
import org.http4s.{headers => H}
import org.http4s.dsl._
import org.http4s.server._
  // import org.http4s.server.syntax._
import org.http4s.server.staticcontent._
import org.http4s.server.blaze._
import org.http4s.circe._
import _root_.io.circe
import circe._
import circe.generic._
  // import circe.generic.auto._
  // import circe.syntax._
import circe.literal._



case class LabelerReqForm(
  labels: Seq[String],
  description: String
)

object LabelerReqForm {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelerReqForm] = deriveEncoder
  implicit val decoder: Decoder[LabelerReqForm] = deriveDecoder
}


case class LTarget(
  page: Int,
  bbox: Seq[Int]
)

object LTarget {

  import circe.generic.semiauto._
  implicit val encoder: Encoder[LTarget] = deriveEncoder
  implicit val decoder: Decoder[LTarget] = deriveDecoder
}
case class LabelingSelection(
  annotType: String,
  targets: Seq[LTarget]
)
object LabelingSelection {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelingSelection] = deriveEncoder
  implicit val decoder: Decoder[LabelingSelection] = deriveDecoder
}

case class LabelingReqForm(
  // form: String, Seq[String]],
  selection: LabelingSelection
)
object LabelingReqForm {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelingReqForm] = deriveEncoder
  implicit val decoder: Decoder[LabelingReqForm] = deriveDecoder
}

trait LabelingServices { self =>
  lazy val labelingServices = self

  // Mounted at /api/v1xx/..
  val labelingServiceEndpoints = HttpService {
    case req @ POST -> Root / "ui" / "labeler" =>

      println(s"POST /ui/labeler/ : req: ${req}")

      for {
        labels <- req.as(jsonOf[LabelerReqForm]).attemptFold(t => {
          println(s"Error: ${t}")
          println(s"Error: ${t.getCause}")
          println(s"Error: ${t.getMessage}")
          LabelerReqForm(List("Error"), s"Server Error ${t}")
        }, ss => ss)
        panel = html.Parts.labelingPanel(labels.labels)
        resp <- Ok(panel.toString).putHeaders(H.`Content-Type`(MediaType.`text/html`))
      } yield { resp }

    case req @ POST -> Root / "label"  =>
      println(s"req: ${req}")
      for {
        labels <- req.as(jsonOf[LabelingReqForm]).attemptFold(t => {
          println(s"Error: ${t}")
          println(s"Error: ${t.getCause}")
          println(s"Error: ${t.getMessage}")
          LabelingReqForm(LabelingSelection("Error", Seq()))
        }, ss => ss)
        resp <- Ok("<span>Successfully labeled!</span>").putHeaders(H.`Content-Type`(MediaType.`text/html`))
      } yield { resp }
  }
}
