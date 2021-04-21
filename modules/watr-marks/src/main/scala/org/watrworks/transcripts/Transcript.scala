package org.watrworks
package transcripts

import io.circe
import _root_.io.circe, circe.syntax._, circe._
import io.circe.generic._
import circe.generic.semiauto._
import cats.syntax.functor._

import geometry._

@JsonCodec
case class Transcript(
  documentId: String @@ DocumentID,
  pages: List[Transcript.Page],
  labels: List[Transcript.Label],
  stanzas: List[Transcript.Stanza]
)

object Transcript {
  import GeometryCodecs._

  @JsonCodec
  case class Page(
    page: Int @@ PageNum,
    bounds: LTBounds,
    glyphs: List[Glyph],
    labels: List[Transcript.Label]
  )

  case class Glyph(
    str: String,
    id: Int @@ GlyphID,
    rect: LTBounds,
    props: Option[GlyphProps]
  )

  implicit val Enc_Glyph: Encoder[Glyph] = new Encoder[Glyph] {
    def apply(glyph: Glyph): Json = {
      val id   = glyph.id.asJson
      val str  = glyph.str.asJson
      val rect = glyph.rect.asJson

      glyph.props
        .map(props => Json.arr(str, id, rect, props.asJson))
        .getOrElse(Json.arr(str, id, rect))
    }
  }

  def GlyphDec0: Decoder[Glyph] =
    Decoder[(String, Int @@ GlyphID, LTBounds)].map(rp => Glyph(rp._1, rp._2, rp._3, None))

  def GlyphDec1: Decoder[Glyph] =
    Decoder[(String, Int @@ GlyphID, LTBounds, GlyphProps)].map(rp =>
      Glyph(rp._1, rp._2, rp._3, Some(rp._4))
    )

  implicit def Dec_Glyph: Decoder[Glyph] = GlyphDec1.or(GlyphDec0)

  sealed trait GlyphProps

  object GlyphProps {
    case class Rewrite(gs: List[Glyph]) extends GlyphProps

    implicit val decodeRewrite: Decoder[Rewrite] = new Decoder[Rewrite] {
      final def apply(c: HCursor): Decoder.Result[Rewrite] =
        for {
          kind <- c.downField("kind").as[String]
          gs   <- c.downField("gs").as[List[Glyph]]
        } yield Rewrite(gs)
    }

    implicit lazy val encodeRewrite: Encoder[Rewrite] = new Encoder[Rewrite] {
      def apply(rewrite: Rewrite): Json = {
        Json.obj(
          "kind" := "rewrite",
          "gs" := rewrite.gs
        )
      }
    }

    implicit lazy val encodeGlyphProps: Encoder[GlyphProps] =
      Encoder.instance { case r: Rewrite =>
        r.asJson
      }

    implicit def GlyphPropsDec: Decoder[GlyphProps] =
      decodeRewrite.widen
  }

  implicit val labelIdCodec  = TypeTagCodecs.intCodec[LabelID]
  implicit val stanzaIdCodec = TypeTagCodecs.intCodec[StanzaID]

  sealed trait GlyphRef
  object GlyphRef {
    case class S(v: String) extends GlyphRef
    case class I(v: Int)    extends GlyphRef

    implicit def encodeGlyphRef: Encoder[GlyphRef] = Encoder.instance {
      case r: S => r.v.asJson
      case r: I => r.v.asJson
    }

    val decI = Decoder.decodeInt.map(I(_)).widen[GlyphRef]
    val decS = Decoder.decodeString.map(S(_)).widen[GlyphRef]

    implicit def decodeGlyphRef: Decoder[GlyphRef] = decI or decS
  }

  @JsonCodec
  case class StanzaLine(
    text: String,
    glyphs: List[GlyphRef]
  )

  @JsonCodec
  case class Stanza(
    id: Int @@ StanzaID,
    lines: List[StanzaLine],
    labels: List[Label]
  )

  case class Label(
    name: String,
    id: Option[Int @@ LabelID],
    range: List[Range],
    // props: Option[Json],
    props: Option[Map[String, List[String]]],
    children: Option[List[Label]]
  )

  implicit lazy val LabelDecoder: Decoder[Label] = deriveDecoder
  val LabelEncoder: Encoder[Label]               = deriveEncoder

  implicit def LabelEncoderNonNull: Encoder[Label] = LabelEncoder.mapJson(json => {
    val hc = HCursor.fromJson(json)

    // TODO abstract out this null-dropping json code
    val nonNullKVs = for {
      keys <- hc.keys.toList
      key  <- keys
      v    <- hc.downField(key).focus
      if !v.isNull
    } yield Json.obj(key := v)

    nonNullKVs.foldLeft(Json.obj())(_ deepMerge _)
  })

  sealed case class Span(begin: Int, length: Int)

  implicit val SpanDecoder: Decoder[Span] =
    Decoder[(Int, Int)].map(t => Span(t._1, t._2))

  implicit val SpanEncoder: Encoder[Span] =
    Encoder[(Int, Int)].contramap(span => (span.begin, span.length))

  sealed trait Range {
    def unit: String;
  }

  object Range {
    // case class Uri(segments: List[Label])
    // implicit class RicherRange(val self: Boolean) {
  }

  implicit val RangeDecoder: Decoder[Range] = new Decoder[Range] {
    def apply(c: HCursor): Decoder.Result[Range] = {
      for {
        foo <- c.downField("unit").as[String]
        range <- {
          val d: Decoder[Range] =
            if (foo.startsWith("text")) Decoder[TextRange].widen
            else if (foo.startsWith("shape")) Decoder[GeometryRange].widen
            else if (foo.startsWith("document")) Decoder[DocumentRange].widen
            else if (foo.startsWith("page")) Decoder[PageRange].widen
            else if (foo.startsWith("stanza")) Decoder[StanzaRange].widen
            else Decoder[LabelRange].widen

          d(c)
        }
      } yield range
    }
  }

  implicit val RangeEncoder: Encoder[Range] = Encoder.instance {
    case v: TextRange     => v.asJson
    case v: GeometryRange => v.asJson
    case v: DocumentRange => v.asJson
    case v: PageRange     => v.asJson
    case v: LabelRange    => v.asJson
    case v: StanzaRange   => v.asJson
  }

  @JsonCodec
  case class TextRange(unit: String, at: Span) extends Range

  object TextLabelUnit {
    val Line = "text:line"
    val Char = "text:char"
  }

  @JsonCodec
  case class GeometryRange(unit: String, at: GeometricFigure) extends Range

  @JsonCodec
  case class DocumentRange(unit: String = "document", at: String @@ DocumentID) extends Range

  @JsonCodec
  case class PageRange(unit: String = "page", at: Int @@ PageNum) extends Range

  @JsonCodec
  case class LabelRange(unit: String = "label", at: Int @@ LabelID) extends Range

  @JsonCodec
  case class StanzaRange(unit: String = "stanza", at: Int @@ StanzaID) extends Range

}
