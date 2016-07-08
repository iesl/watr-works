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

    val existingGlyphHashes = eitherExistOrNot.collect({
      case Left(spline) => spline
    })

    val newGlyphHashes = eitherExistOrNot.collect({
      case Right(spline) => spline
    })

    if (existingGlyphHashes.isEmpty) {
      // None of the glyphs have been seen before, according to their hash values, so create a new font entry
      val linkQ = for {
        font <- fonts.ccQuery += Font()
        _ <- DBIOX.seqs{ newGlyphHashes.map { glhash =>
          for {
            glyph <- glyphs.ccQuery += Glyph()
            _ <- glyphToHash.addEdge(glyph, glhash)
            _ <- fontToGlyph.addEdge(font, glyph)
          } yield()
        }}
      } yield ()

      DBIOX.runAndAwait(db, linkQ)


    } else {
      // If any of the glyphs in the dir clash w/existing hashed glyphs, then this entire
      // font dir should be merged with the pre-existing font


      val linkedFontQ = DBIOX.sequence(
        existingGlyphHashes.map { glhash =>
          val q = for {
            h <- splineHashes if h.id === glhash.id
            g2h <- glyphToHash if h.id === g2h.dstId
            glyph <- glyphs if glyph.id === g2h.srcId
            f2g <- fontToGlyph if f2g.dstId === glyph.id
            font <- fonts if font.id == f2g.srcId
          } yield font

          q.result
        }
      )

      val linkedFonts = DBIOX.runAndAwait(db, linkedFontQ).flatten.toSet

      linkedFonts.foreach { font =>
        println(s"""found existing linked font: ${font} """)
      }


      def relinkQ(font: Font) = for {
        _ <- DBIOX.seqs {
          existingGlyphHashes.map(glhash => for {
            // glyph <- glyphTohash.selectAdjacentToDst(glhash)
            // _ <- fontToGlyph.rmEdgesToDst(glyph)
            // _ <- fontToGlyph.addEdge(font, glyph)
            _ <- fontToGlyph.rmEdgesToDst(glhash)
            _ <- fontToGlyph.addEdge(font, glhash)
          } yield ())
        }
      } yield ()

      linkedFonts.toList.map({font0 =>

      })

      // // merge all linked fonts:
      // ///  TODO
      // // val font = linkedFonts.head


      // val relinkAllQ = DBIOX.seqs{
      //   linkedFonts.toList.map({font => relinkQ(font) })
      // }

      // DBIOX.runAndAwait(db, relinkAllQ)


    }
  }

  // def selectFontHashEdges(): Seq[(Font, SplineHash)] = {
  //   val fontHashPairs = for {
  //     font <- fonts
  //     f2h <- fontToHash
  //     h <- splineHashes
  //     if font.id === f2h.srcId && f2h.dstId === h.id
  //   } yield (font, h)

  //   Await.result(db.run(fontHashPairs.result), Duration.Inf)
  // }

  def showFontTree(font: Font): Unit = {
    val qq = for {
      fontGlyphs <- fonts.selectGlyphs(font)
      res <-  DBIOX.sequence {
        fontGlyphs.map{ g => for {
          hash <- glyphs.selectHash(g)
          sfds <- glyphs.selectSfdUrls(g)
        } yield (hash, sfds) }
      }
    } yield fontGlyphs.zip(res).map {
      case (a, bc) => (a, bc._1, bc._2)
    }

    val res = Await.result(db.run(qq), Duration.Inf)

    val rstr = res.map { case (g:Glyph, oh:Option[SplineHash], urls: Seq[CorpusUrl]) =>
      s""" ${g}: ${oh}: [${urls.mkString(", ")}]"""
    }
    println(rstr.mkString("\n"))
  }

  def showFontTrees(): Unit = {

    val fontList = Await.result(db.run(fonts.to[List].result), Duration.Inf)

    fontList.map(showFontTree(_))

  }

  def reportAll() = {

    Await.result(
      db.run((for {
        _ <- fonts.to[List].result.map(x => println(s"fonts: ${x}"))
        // _ <- names.to[List].result.map(x => println(s"names: ${x}"))
        // _ <- families.to[List].result.map(x => println(s"families: ${x}"))
        _ <- splineHashes.to[List].result.map(x => println(s"splineHash: ${x}"))
        _ <- fontToGlyph.to[List].result.map({a => println(s"font->glyph: ${a}")})
        _ <- glyphToHash.to[List].result.map({a => println(s"glyph->hash: ${a}")})
      } yield ())),
      Duration.Inf
    )
  }


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
