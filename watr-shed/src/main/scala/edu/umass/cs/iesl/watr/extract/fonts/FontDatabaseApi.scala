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


  val D = FontDatabaseTables
  import ExecutionContext.Implicits.global


  def updateSpline(s: GlyphProp.SplineSet): DBIO[Either[D.GlyphHash, D.GlyphHash]] = {
    val glyphHashStr = GlyphProp.splineSetHash(s).toString
    D.GlyphHashes.upsert(glyphHashStr)
  }

  // def addGlyphHashes(glyphs: Seq[SplineFont.Glyph]): DBIO[Seq[Either[D.GlyphHash, D.GlyphHash]]] = {
  def addGlyphHashes(glyphs: Seq[SplineFont.Glyph]): DBIO[Seq[D.GlyphHash]] = {
    DBIOX.sequence{
      for {
        glyph   <- glyphs
        splineSet <- glyph.get[GlyphProp.SplineSet]
      } yield for {
        sps <- updateSpline(splineSet).map{ _ match {
          case Left(e) => e
          case Right(e) => e
        }}
      } yield sps
    }
  }

  def addFontSubsetDir(fontDir: SplineFont.Dir): DBIO[D.FontSubset] = {
    for {
      fontSubset     <- D.FontSubsets.ccQuery += D.FontSubset()
      fontSubsetPath <- D.CorpusUrls.ccQuery += D.CorpusUrl(fontDir.path.toString())
      _              <- D.FontSubsetToPath.addEdge(fontSubset, fontSubsetPath)
      glyphHashes    <- addGlyphHashes(fontDir.glyphs)
      _              <- DBIOX.seqs{
        for {
          (fontGlyph, glhash) <- (fontDir.glyphs zip glyphHashes)
        } yield for {
          glyph     <- D.Glyphs.ccQuery += D.Glyph()
          glyphPath <- D.CorpusUrls.ccQuery += D.CorpusUrl(fontGlyph.path.toString())
          _         <- D.GlyphToGlyphPath.addEdge(glyph, glyphPath)
          _         <- D.GlyphHashToGlyph.addEdge(glhash, glyph)
          _         <- D.FontSubsetToGlyph.addEdge(fontSubset, glyph)
        } yield()
      }
    } yield fontSubset
  }


  def addFontDir(fontDir: SplineFont.Dir): Unit = {

    DBIOX.runAndAwait(db, {
      for {
        font <- D.Fonts.ccQuery += D.Font()
        fontSubset <- addFontSubsetDir(fontDir)
        _ <- D.FontToFontSubset.addEdge(font, fontSubset)
      } yield ()
    })



    // if (existingGlyphHashes.isEmpty) {
    //   // None of the glyphs have been seen before, according to their hash values, so create a new font entry

    // } else {
    //   println(s"merging fonts")
    //   // If any of the glyphs in the dir clash w/existing hashed glyphs, then this entire
    //   // font dir should be merged with the pre-existing font

    //   val linkedFontQ = DBIOX.sequence(
    //     existingGlyphHashes.map { glhash =>
    //       val q = for {
    //         h     <- D.GlyphHashes if h.id === glhash.id
    //         g2h   <- D.GlyphHashToGlyph if h.id === g2h.dstId
    //         glyph <- D.Glyphs if glyph.id === g2h.srcId
    //         f2g   <- D.FontSubsetToGlyph if f2g.dstId === glyph.id
    //         font  <- D.Fonts if font.id == f2g.srcId
    //       } yield font

    //       q.result
    //     }
    //   )

    //   val linkedFonts = DBIOX.runAndAwait(db, linkedFontQ).flatten.toSet

    //   linkedFonts.foreach { font =>
    //     println(s"""found existing linked font: ${font} """)
    //   }


    //   def relinkQ(font: D.Font) = for {
    //     _ <- DBIOX.seqs {
    //       existingGlyphHashes.map(glhash => for {
    //         _ <- D.FontSubsetToGlyph.rmEdgesToDst(glhash)
    //         _ <- D.FontSubsetToGlyph.addEdge(font, glhash)
    //       } yield ())
    //     }
    //   } yield ()
    // }

  }

  // def selectFontHashEdges(): Seq[(Font, GlyphHash)] = {
  //   val fontHashPairs = for {
  //     font <- fonts
  //     f2h <- fontToHash
  //     h <- GlyphHashes
  //     if font.id === f2h.srcId && f2h.dstId === h.id
  //   } yield (font, h)

  //   Await.result(db.run(fontHashPairs.result), Duration.Inf)
  // }

  def showFontTree(font: D.Font): Unit = {
    val qq = for {
      fontSubsets <- D.Fonts.selectFontSubsets(font)
      fontSubsetGlyphs <- DBIOX.sequence{ fontSubsets.map{ fontSubset =>
        for {
          glyphs <- D.FontSubsets.selectGlyphs(fontSubset)
          glHashPaths <- DBIOX.sequence{
            glyphs.map{ glyph => for {
              hash <- D.Glyphs.selectHash(glyph)
              path <- D.Glyphs.selectPath(glyph)
            } yield (glyph, hash, path) }
          }
        } yield (fontSubset, glHashPaths)
      }}
    } yield fontSubsetGlyphs

    val res = Await.result(db.run(qq), Duration.Inf)

    println(s"Font ${font}")

    for {
      (fontSubset, glyphInfos) <- res
      _ = println(s"  FontSubset ${fontSubset}")
      (glyph, hash, path) <- glyphInfos
    } {
      println(s"""      glyph> ${glyph} ${hash} ${path}""")

    }
  }

  def showFontTrees(): Unit = {

    val fontList = Await.result(db.run(D.Fonts.to[List].result), Duration.Inf)

    fontList.map(showFontTree(_))

  }

  def reportAll() = {

    Await.result(
      db.run((for {
        _ <- D.Fonts.to[List].result.map(x => println(s"fonts: ${x}"))
        // _ <- D.FontSubsets.to[List].result.map(x => println(s"font subsets: ${x}"))
        _ <- D.FontToFontSubset.to[List].result.map({a => println(s"font -> fsubset: ${a}")})
        _ <- D.FontSubsetToGlyph.to[List].result.map({a => println(s"fsubset -> glyph: ${a}")})
        _ <- D.GlyphHashToGlyph.to[List].result.map({a => println(s"glyph hash -> glyph: ${a}")})
        _ <- D.GlyphHashes.to[List].result.map(x => println(s"glyphHash: ${x}"))
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
    //   case s: GlyphProp.SplineSet         => glyphHashes += GlyphHash(None, GlyphProp.splineSetHash(s).toString)
    //   // case GlyphProp.StartChar(glyphName) =>
    //   // case GlyphProp.Encoding(other)      =>
    //   // case GlyphProp.Width(other)         =>
    //   // case GlyphProp.Flags(other)         =>
    //   // case GlyphProp.HStem(other)         =>
    //   // case GlyphProp.VStem(other)         =>
    //   // case GlyphProp.LayerCount(other)    =>
    //   case _                              => DBIOX.noop
    // }}}
