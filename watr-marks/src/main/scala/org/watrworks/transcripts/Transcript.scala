package org.watrworks
package transcripts

import TypeTags._
import utils.ExactFloats._
import io.circe
import circe.generic.semiauto._
import _root_.io.circe, circe.syntax._, circe._
import io.circe.generic._
import circe.generic.semiauto._
import cats.syntax.functor._

import utils.DoOrDieHandlers._
import geometry._
import GeometryCodecs._
import scalaz.Tag.TagOf


/**
  * Serialization Format for text extracted from PDFs
  *   {
  *     description: "desc",
  *     documentId: "doc-25-id",
  *     pages: [{
  *       pdfPageBounds: [0, 0, 61200, 79200],
  *       lines: [{
  *         text: "I Ã ffi",
  *         glyphs: [
  *           [1, 2, 3, 4],
  *           [[59, 2, 3, 4], {}],
  *           [[3, 2, 3, 4], {
  *             "gs": [
  *               [[1, 2, 3, 4], { "g": "A" }],
  *               [[1, 2, 3, 4], { "g": "~" }]
  *             ]
  *           }],
  *         ]
  *       }, {
  *         text: "Fe_{3}",
  *         glyphs: [
  *           [11, 2, 3, 4],
  *           [22, 2, 3, 4],
  *           [[53, 2, 3, 4], { "os": [-2, -1, 1] }],
  *           [[54, 2, 3, 4], { "o": 1 }]
  *         ]
  *       }]
  *     }],
  *     labels: [
  *       { name: "HasRefs", id: "L#2", range: [{ unit: "page", at: [7, 2] }] },
  *       { name: "IsGoldLabled", id: "L#3", range: [{ unit: "document" }] },
  *     ]
  *   }
  *
  */


/** */

@JsonCodec
case class Transcript(
  description: String,
  documentId: String@@DocumentID,
  pages: List[Transcript.Page]
)

object Transcript {
  import GeometryCodecs._

  @JsonCodec
  case class Page(
    pdfPageBounds: LTBounds,
    lines: List[Line],
    // labels: List[Label]
  )

  @JsonCodec
  case class Line(
    text: String,
    glyphs: List[Glyph]
  )

  case class Glyph(
    rect: LTBounds,
    props: Option[GlyphProps]
  )
  implicit val Enc_Glyph: Encoder[Glyph] = new Encoder[Glyph] {
    def apply(glyph: Glyph): Json = {
      val rect = glyph.rect.asJson

      glyph.props
        .map(props => Json.arr(rect, props.asJson))
        .getOrElse(rect)
    }
  }
  val GlyphDec0: Decoder[Glyph] = Decoder[LTBounds].map(rect => Glyph(rect, None))
  lazy val GlyphDec1: Decoder[Glyph] = Decoder[(LTBounds, GlyphProps)].map(rp => Glyph(rp._1, Some(rp._2)))
  implicit lazy val Dec_Glyph: Decoder[Glyph] = GlyphDec1.or(GlyphDec0)

  case class GlyphProps(
    g: Option[String] = None,
    gs: Option[List[Glyph]] = None,
    o: Option[Int] = None,
    os: Option[List[Int]] = None,
  )

  implicit val Dec_GlyphProps: Decoder[GlyphProps] = deriveDecoder

  implicit val GlyphPropsEncoder: Encoder[GlyphProps] = new Encoder[GlyphProps] {
    def apply(ps: GlyphProps): Json = {
      val g = ps.g.map(v => Json.obj("g" := v)).getOrElse(Json.obj())
      val gs = ps.gs.map(v => Json.obj("gs" := v)).getOrElse(Json.obj())
      val o = ps.o.map(v => Json.obj("o" := v)).getOrElse(Json.obj())
      val os = ps.os.map(v => Json.obj("os" := v)).getOrElse(Json.obj())

      g.deepMerge(gs)
        .deepMerge(o)
        .deepMerge(os)
    }
  }


  implicit val labelIdCodec = TypeTagCodecs.strCodec[LabelID]

  @JsonCodec
  case class Label(
    name: String,
    id: String @@ LabelID,
    range: List[Range],
    props: JsonObject
  )


  sealed case class Span(
    begin: Int,
    length: Int
  )

  implicit val SpanDecoder: Decoder[Span] = Decoder[(Int, Int)]
    .map(t => Span(t._1, t._2))

  implicit val SpanEncoder: Encoder[Span] = Encoder[(Int, Int)]
    .contramap(span => (span.begin, span.length))

  sealed trait Range {
    def unit: String;
  }

  implicit val RangeDecoder: Decoder[Range] = new Decoder[Range] {
    def apply(c: HCursor): Decoder.Result[Range] = {
      for {
        foo <- c.downField("unit").as[String]
        range <- {
          val d: Decoder[Range] =
            if (foo.startsWith("text")) {
              Decoder[TextRange].widen
            } else if (foo.startsWith("shape")) {
              Decoder[GeometryRange].widen
            } else if (foo.startsWith("document")) {
              Decoder[DocumentRange].widen
            } else if (foo.startsWith("page")) {
              Decoder[PageRange].widen
            } else {
              Decoder[LabelRange].widen
            }
          d(c)
        }
      } yield range
    }
  }
  implicit val RangeEncoder: Encoder[Range] = Encoder.instance {
    case v: TextRange => v.asJson
    case v: GeometryRange => v.asJson
    case v: DocumentRange => v.asJson
    case v: PageRange => v.asJson
    case v: LabelRange => v.asJson
  }


  @JsonCodec
  case class TextRange(
    unit: String,
    page: Int,
    at: Span
  ) extends Range

  object TextLabelUnit {
    val Line = "text:line"
    val Char = "text:char"
  }

  @JsonCodec
  case class GeometryRange(
    unit: String,
    page: Int,
    at: GeometricFigure
  ) extends Range

  object GeometryLabelUnit {
    val Point = "shape:point"
    val Line = "shape:line"
    val Rect = "shape:rect"
    val Circle = "shape:circle"
    val Triangle = "shape:triangle"
    val Trapezoid = "shape:trapezoid"
  }

  @JsonCodec
  case class DocumentRange(
    unit: String = "document"
  ) extends Range {
  }

  @JsonCodec
  case class PageRange(
    unit: String = "page",
    at: Span
  ) extends Range

  @JsonCodec
  case class LabelRange(
    unit: String = "label"
  ) extends Range

}
