package edu.umass.cs.iesl.watr
package extract
package fonts

import org.slf4j.LoggerFactory
import scala.concurrent._
import ammonite.ops._


import slick.driver.H2Driver.api._

import scala.concurrent._
import scala.concurrent.duration._


class FontDatabaseApi(dir: Path) extends AbstractDatabase(dir)  {
  val log = LoggerFactory.getLogger(this.getClass)
  // only used for chaining Futures, not for any IO
  import ExecutionContext.Implicits.global

  import FontDatabaseTables._

  def updateSpline(s: GlyphProp.SplineSet): DBIO[Either[SplineHash, SplineHash]] = {
    val splineHashStr = GlyphProp.splineSetHash(s).toString
    val splineHash = SplineHash(splineHashStr)

    for {
      sh <- splineHashes.findByHash(splineHashStr)
      sp <- sh match {
        case Some(hash) =>
          DBIOX.successful(Left[SplineHash, SplineHash](hash))
        case None =>
          (splineHashes.ccQuery += splineHash).map{ z =>
            Right[SplineHash, SplineHash](z)
          }
      }
    } yield sp

    // splineHashes.ccQuery
    //   .insertOrUpdate(splineHash)
    //   .flatMap({ resHash => resHash match {
    //     case Some(insHash) =>
    //       DBIOX.successful(Right[SplineHash, SplineHash](insHash))
    //     case None =>
    //       DBIOX.successful(Left[SplineHash, SplineHash](resHash.get))
    //   }})
  }

  def addFontDir(fontDir: SplineFont.Dir): Unit = {
    val insSplines = for {
      glyph <- fontDir.glyphs
      splines <- glyph.get[GlyphProp.SplineSet]
    } yield {
      updateSpline(splines)
    }

    val eitherExistOrNot = Await.result(
      db.run(DBIOX.sequence(insSplines)),
      Duration.Inf
    )

    val existing = eitherExistOrNot.collect({
      case Left(spline) => spline
    })

    val newlyIns = eitherExistOrNot.collect({
      case Right(spline) => spline
    })

    if (existing.length>0) {
      // If any of the splines in the dir clash w/existing, then this entire font dir should be merged with
      //   the pre-existing font

      val linkedFontQ = DBIOX.sequence(
        existing.map({ e => (for {
          f <- fonts
          f2h <-  fontToHash
          h <- splineHashes
          if h.id === e.id
        } yield f).result })
      )

      val linkedFonts = DBIOX.runAndAwait(db, linkedFontQ).flatten.toSet

      linkedFonts.foreach { font =>
        println(s"""found existing linked font: ${font} """)
      }

      // merge all linked fonts:
      ///  TODO
      // val font = linkedFonts.head

      //
      def relinkQ(font: Font) = for {
        _ <- DBIOX.seqs{
          existing.map(fontToHash.rmEdgesRhs(_))
        }
        _ <- DBIOX.seqs{
          existing.map(fontToHash.addEdge(font, _) )
        }
        _ <- DBIOX.seqs{
          newlyIns.map(fontToHash.addEdge(font, _) )
        }
      } yield ()

      val relinkAllQ = DBIOX.seqs{
        linkedFonts.toList.map({font => relinkQ(font) })
      }

      DBIOX.runAndAwait(db, relinkAllQ)


    } else {
      //... otherwise, (no clashes), create a new font entry
      val linkQ = for {
        f <- fonts.ccQuery += Font()
        _ <- DBIOX.seqs{
          newlyIns.map({hash => fontToHash.addEdge(f, hash) })
        }
      } yield ()

      DBIOX.runAndAwait(db, linkQ)


    }







    // val qs = fontDir.props.map { p => p match  {
    //   // case FontProp.SplineFontDB(v: String)    =>
    //   // case FontProp.FontName(v: String)        => names += Name(None, v)
    //   // case FontProp.FullName(v: String)        => names += Name(None, v)
    //   // case FontProp.FamilyName(v: String)      => families += Family(None, v)
    //   // case FontProp.Weight(v: String)          =>
    //   // case FontProp.Copyright(v: String)       =>
    //   // case FontProp.Version(v: String)         =>
    //   // case FontProp.ItalicAngle(v: Double)     =>
    //   // case FontProp.UnderlinePosition(v: Int)  =>
    //   // case FontProp.UnderlineWidth(v: Int)     =>
    //   // case FontProp.Ascent(v: Int)             =>
    //   // case FontProp.Descent(v: Int)            =>

    //   case _                              => DBIOX.noop
    // }}

    // val ps = fontDir.glyphs.flatMap { gl => gl.props.map{ pr => pr match {
    //   case s: GlyphProp.SplineSet         => splineHashes += SplineHash(None, GlyphProp.splineSetHash(s).toString)
    //   // case GlyphProp.StartChar(glyphName) =>
    //   // case GlyphProp.Encoding(other)      =>
    //   // case GlyphProp.Width(other)         =>
    //   // case GlyphProp.Flags(other)         =>
    //   // case GlyphProp.HStem(other)         =>
    //   // case GlyphProp.VStem(other)         =>
    //   // case GlyphProp.LayerCount(other)    =>
    //   case _                              => DBIOX.noop
    // }}}
  }

  def reportAll() = {

    Await.result(
      db.run((for {
        _ <- fonts.to[List].result.map(x => println(s"fonts: ${x}"))
        // _ <- names.to[List].result.map(x => println(s"names: ${x}"))
        // _ <- families.to[List].result.map(x => println(s"families: ${x}"))
        _ <- splineHashes.to[List].result.map(x => println(s"splineHash: ${x}"))
        _ <- fontToHash.to[List].result.map({a => println(s"font->hash: ${a}")})
      } yield ())),
      Duration.Inf
    )
  }


}
