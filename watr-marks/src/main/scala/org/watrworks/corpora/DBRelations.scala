package edu.umass.cs.iesl.watr
package corpora

import edu.umass.cs.iesl.watr.{geometry => G}
import watrmarks._

// import io.circe.generic.JsonCodec
import io.circe._
import io.circe.generic._
import io.circe.generic.auto._
import geometry._

// import TypeTags._


import java.time.Instant

object RelationModel extends GeometricFigureCodecs {

  implicit val Enc_Int_Instant: Encoder[Instant] = Encoder.encodeLong.contramap(_.toEpochMilli())
  implicit val Dec_Int_Instant: Decoder[Instant] = Decoder.decodeLong.map(l => Instant.ofEpochMilli(l))

  // implicit val Enc_Int_XX: Encoder[Int@@XX] = Encoder.encodeInt.contramap(_.unwrap)
  // implicit val Dec_Int_XX: Decoder[Int@@XX] = Decoder.decodeInt.map(XX(_))

  @JsonCodec
  case class Document(
    prKey   : Int@@DocumentID,
    stableId: String@@DocumentID
  )

  @JsonCodec
  case class Page(
    prKey      : Int@@PageID,
    document   : Int@@DocumentID,
    pagenum    : Int@@PageNum,
    bounds     : G.LTBounds
  )

  @JsonCodec
  case class Person(
    prKey     : Int@@UserID,
    email     : String@@EmailAddr
  )

  @JsonCodec
  case class WorkflowDef(
    workflow      : String@@WorkflowID,
    labelSchemas  : LabelSchemas,
    targetPath    : String@@CorpusPath,
  )

  @JsonCodec
  case class CorpusLock(
    id         : Int@@LockID,
    holder     : Option[Int@@UserID],
    document   : Int@@DocumentID,
    lockPath   : String@@CorpusPath,
    status     : String@@StatusCode
  )

  case class AnnotationRec_(
    id             : Int@@AnnotationID,
    document       : Int@@DocumentID,
    owner          : Option[Int@@UserID],
    annotPath      : Option[String@@CorpusPath],
    created        : java.time.Instant,
    label          : String,
    location       : String,
    body           : Option[String]
  )

  @JsonCodec
  case class AnnotationRec(
    id             : Int@@AnnotationID,
    document       : Int@@DocumentID,
    owner          : Option[Int@@UserID],
    annotPath      : Option[String@@CorpusPath],
    created        : java.time.Instant,
    label          : Label,
    location       : AnnotatedLocation,
    body           : Option[AnnotationBody]
  )

}
