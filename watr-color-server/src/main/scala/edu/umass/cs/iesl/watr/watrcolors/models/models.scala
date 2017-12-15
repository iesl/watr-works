package edu.umass.cs.iesl.watr
package watrcolors
package models

import cats.effect._
import io.circe.generic.JsonCodec
import org.http4s.circe._
import io.circe
import circe._
// import circe.syntax._
import circe.literal._
import org.http4s.EntityDecoder
// import java.util.UUID

// import cats.implicits._, cats.data._
import tsec.passwordhashers.imports.SCrypt


import watrmarks._
import geometry._

import circe.generic.auto._
import corpora.{RelationModel => R}

object users {

  case class User(
    id       : Int@@UserID,
    email    : String@@EmailAddr,
  )

  case class AuthInfo(
    userId: Int@@UserID,
    username : String@@Username,
    password: SCrypt
    // password: IO[PasswordHash[SCrypt]]
  )


  // object User {
  //   implicit def authRole[F[_]](implicit F: MonadError[F, Throwable]): AuthorizationInfo[F, Role, User] =
  //     new AuthorizationInfo[F, Role, User] {
  //       def fetchInfo(u: User): F[Role] = F.pure(u.role)
  //     }
  // }

  // sealed abstract case class Role(roleRepr: String)

  // object Role extends SimpleAuthEnum[Role, String] {
  //   implicit object Administrator  extends Role("Administrator")
  //   implicit object Curator        extends Role("Curator")
  //   implicit object Labeler        extends Role("Labeler")

  //   implicit object CorruptedData  extends Role("CorruptedData")

  //   implicit val E: Eq[Role]      = Eq.fromUniversalEquals[Role]
  //   val getRepr: (Role) => String = _.roleRepr

  //   protected val values: AuthGroup[Role] = AuthGroup(Administrator, Curator, Labeler)
  //   val orElse: Role                      = CorruptedData
  // }
}


object formdata {
  case class LoginForm(
    email: String,
    password: String
  )

  object LoginForm {
    object LoginError extends Exception {
      override def getMessage: String = "Login Error"

      override def fillInStackTrace(): Throwable = this
    }

    implicit def decoder[F[_]: Effect]: EntityDecoder[F, LoginForm] = jsonOf[F, LoginForm]

  }

  case class SignupForm(
    email: String,
    username: String,
    password: String
  )

  object SignupForm {
    final object SignupError extends Exception {
      override def getMessage: String = "Signup Error"

      override def fillInStackTrace(): Throwable = this
    }

    implicit def entityD[F[_]: Effect]: EntityDecoder[F, SignupForm] = jsonOf[F, SignupForm]
  }


}

// case class LabelerReqForm(
//   labels: Seq[Label],
//   description: String
// )

// object LabelerReqForm extends CirceJsonCodecs {
//   implicit def entityD[F[_]: Effect]: EntityDecoder[F, LabelerReqForm] = jsonOf[F, LabelerReqForm]
// }


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

case class LabelSpanReq(
  labelChoice: Label,
  gridJson: Json
)

object LabelSpanReq extends CirceJsonCodecs {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelSpanReq] = deriveEncoder
  implicit val decoder: Decoder[LabelSpanReq] = deriveDecoder
}


case class LabelingReqForm(
  stableId: String,
  labelChoice: Label,
  selection: LabelingSelection
)

object LabelingReqForm extends CirceJsonCodecs {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelingReqForm] = deriveEncoder
  implicit val decoder: Decoder[LabelingReqForm] = deriveDecoder
}

case class LabelsRequest(
  stableId: String
)

object LabelsRequest {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelsRequest] = deriveEncoder
  implicit val decoder: Decoder[LabelsRequest] = deriveDecoder
}
case class DeleteZoneRequest(
  stableId: String,
  zoneIds: Seq[Int]
)

object DeleteZoneRequest {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[DeleteZoneRequest] = deriveEncoder
  implicit val decoder: Decoder[DeleteZoneRequest] = deriveDecoder
}

trait TypeTagCodecs {

  implicit def Enc_IntTypeTags[T]: Encoder[Int@@T] = Encoder.encodeInt.contramap(_.unwrap)
  implicit def Enc_StringTypeTags[T]: Encoder[String@@T] = Encoder.encodeString.contramap(_.unwrap)

}

trait CirceJsonCodecs extends TypeTagCodecs {
  import circe.generic.semiauto._

  implicit val Enc_LTBounds: Encoder[LTBounds] = deriveEncoder

  implicit val Enc_StablePage: Encoder[StablePage] = deriveEncoder
  implicit val Enc_PageRegion: Encoder[PageRegion] = deriveEncoder


  // implicit val Enc_XX: Encoder[XX] = deriveEncoder
  implicit val Enc_Label: Encoder[Label] = Encoder.encodeString.contramap(_.fqn)
  implicit val Dec_Label: Decoder[Label] = Decoder.decodeString.map(Label(_))

  implicit lazy val Enc_Zone: Encoder[Zone] = deriveEncoder

}

case class WorkflowForm(
  workflow     : String,
  description  : String,
  targetLabel  : String,
  curatedLabels: Seq[String]
)

@JsonCodec sealed trait Mod
case class StatusUpdate(status: String) extends Mod
case class Unassign() extends Mod

@JsonCodec case class WorkflowMod(
  update: Mod
)

trait WorkflowCodecs extends CirceJsonCodecs {
  import circe.generic.semiauto._

  implicit val encoder: Encoder[WorkflowForm] = deriveEncoder
  implicit val decoder: Decoder[WorkflowForm] = deriveDecoder

  implicit val encoder2: Encoder[R.WorkflowDef] = deriveEncoder

  implicit def WorkflowForm_EntityDecoder[F[_]: Effect]: EntityDecoder[F, WorkflowForm] = jsonOf[F, WorkflowForm]

  // implicit def WorkflowMod_EntityDecoder[F[_]: Effect]: EntityDecoder[F, WorkflowMod] = jsonOf[F, WorkflowMod]
  // implicit def WorkflowUpdate_EntityDecoder[F[_]: Effect]: EntityDecoder[F, StatusUpdate] = jsonOf[F, StatusUpdate]

}
