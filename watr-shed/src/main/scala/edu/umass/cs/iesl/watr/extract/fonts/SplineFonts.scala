package edu.umass.cs.iesl.watr
package extract
package fonts

import ammonite.{ops => fs}

// object SplineFonts {

//   def loadProps(sfdir: fs.Path): SplineFont.Dir = {
//     val propSeq = fs.ls(sfdir)
//       .filter(_.ext=="props")
//       .map({ propFile =>
//         val propStr = fs.read(propFile)

//         SplineQuickParser.readFontProps(propStr)
//       })

//     val props = propSeq.headOption.getOrElse(sys.error("no font.props found"))
//     SplineFont.Dir(props, glyphs=Seq(), sfdir)
//   }

//   def addGlyph(dir: SplineFont.Dir, glyphPath: fs.Path): SplineFont.Dir = {
//     // println("adding glyph")
//     val glyphStr = fs.read(glyphPath)
//     val glyph = SplineQuickParser.parser(glyphStr)
//       .copy(path=Some(glyphPath))

//     dir.copy(glyphs = dir.glyphs :+ glyph)
//   }

//   def loadSfdir(sfdir: fs.Path): SplineFont.Dir = {
//     val dir = loadProps(sfdir)

//     fs.ls(sfdir)
//       .filter(_.ext=="glyph")
//       .foldLeft(dir)({case (acc, e) =>
//         addGlyph(acc, e)
//       })
//   }


// }


object SplineFont {
  import scala.reflect._

  case class Glyph(
    props: Seq[GlyphProp],
    path: Option[fs.Path] = None
  ) {

    def prop[P <: GlyphProp](implicit ct: ClassTag[P]): P = {
      props.collectFirst({
        case pr if ct.unapply(pr).isDefined => pr.asInstanceOf[P]
      }).headOption.getOrElse(sys.error(s"glyph prop ${ct.runtimeClass.getSimpleName} not found"))
    }

    def get[P <: GlyphProp](implicit ct: ClassTag[P]): Option[P] = {
      props.collectFirst({
        case pr if ct.unapply(pr).isDefined => pr.asInstanceOf[P]
      })
    }
  }

  case class Dir(
    props: Seq[FontProp],
    glyphs: Seq[Glyph],
    path: fs.Path
  ) {

    def prop[P <: FontProp](implicit ct: ClassTag[P]): P = {
      props.collectFirst({
        case pr if ct.unapply(pr).isDefined => pr.asInstanceOf[P]
      }).headOption.getOrElse(sys.error(s"font prop ${ct.runtimeClass.getSimpleName} not found"))
    }

    def get[P <: FontProp](implicit ct: ClassTag[P]): Option[P] = {
      props.collectFirst({
        case pr if ct.unapply(pr).isDefined => pr.asInstanceOf[P]
      })
    }

  }
}


sealed trait GlyphProp

object GlyphProp {

  case class SplineSet(splines: Seq[Spline]) extends GlyphProp
  case class StartChar(v: String)            extends GlyphProp
  case class Encoding(v: String)             extends GlyphProp
  case class Width(v: String)                extends GlyphProp
  case class Flags(v: String)                extends GlyphProp
  case class HStem(v: String)                extends GlyphProp
  case class VStem(v: String)                extends GlyphProp
  case class LayerCount(v: String)           extends GlyphProp
  case class Err(key: String)                extends GlyphProp

  case class Spline(ns: Seq[Float], code: Char, flags: String)


  def splineSetHash(sset: SplineSet): String@@SHA1String = {
    val splineStr = sset.splines.map({s =>
      s"""${s.ns.mkString(" ")} ${s.code} ${s.flags}"""
    }).mkString("|")

    Hash.toSHA1String(splineStr)
  }

}


sealed trait FontProp

object FontProp {

  case class SplineFontDB(v: String             ) extends FontProp
  case class FontName(v: String                 ) extends FontProp
  case class FullName(v: String                 ) extends FontProp
  case class FamilyName(v: String               ) extends FontProp
  case class Weight(v: String                   ) extends FontProp
  case class Copyright(v: String                ) extends FontProp
  case class Version(v: String                  ) extends FontProp
  case class ItalicAngle(v: Double              ) extends FontProp
  case class UnderlinePosition(v: Int           ) extends FontProp
  case class UnderlineWidth(v: Int              ) extends FontProp
  case class Ascent(v: Int                      ) extends FontProp
  case class Descent(v: Int                     ) extends FontProp
  case class InvalidEm(v: Int                   ) extends FontProp
  case class sfntRevision(v: String             ) extends FontProp
  case class LayerCount(v: Int                  ) extends FontProp
  case class Layer(v: String                    ) extends FontProp
  case class StyleMap(v: String                 ) extends FontProp
  case class FSType(v: String                   ) extends FontProp
  case class OS2Version(v: String               ) extends FontProp
  case class OS2_WeightWidthSlopeOnly(v: String ) extends FontProp
  case class OS2_UseTypoMetrics(v: String       ) extends FontProp
  case class CreationTime(v: String             ) extends FontProp
  case class ModificationTime(v: String         ) extends FontProp
  case class OS2TypoAscent(v: String            ) extends FontProp
  case class OS2TypoAOffset(v: String           ) extends FontProp
  case class OS2TypoDescent(v: String           ) extends FontProp
  case class OS2TypoDOffset(v: String           ) extends FontProp
  case class OS2TypoLinegap(v: String           ) extends FontProp
  case class OS2WinAscent(v: String             ) extends FontProp
  case class OS2WinAOffset(v: String            ) extends FontProp
  case class OS2WinDescent(v: String            ) extends FontProp
  case class OS2WinDOffset(v: String            ) extends FontProp
  case class HheadAscent(v: String              ) extends FontProp
  case class HheadAOffset(v: String             ) extends FontProp
  case class HheadDescent(v: String             ) extends FontProp
  case class HheadDOffset(v: String             ) extends FontProp
  case class DEI(v: String                      ) extends FontProp
  case class Encoding(v: String                 ) extends FontProp
  case class UnicodeInterp(v: String            ) extends FontProp
  case class NameList(v: String                 ) extends FontProp
  case class DisplaySize(v: String              ) extends FontProp
  case class AntiAlias(v: String                ) extends FontProp
  case class FitToEm(v: String                  ) extends FontProp
  case class BeginPrivate(v: String             ) extends FontProp
  case class BlueValues(v: String               ) extends FontProp
  case class BlueScale(v: String                ) extends FontProp
  case class StdHW(v: String                    ) extends FontProp
  case class StdVW(v: String                    ) extends FontProp
  case class StemSnapH(v: String                ) extends FontProp
  case class StemSnapV(v: String                ) extends FontProp
  case object EndPrivate extends FontProp
  case object EndSplineFont extends FontProp

  case class ErrProp(key: String) extends FontProp

}
