package edu.umass.cs.iesl.watr
package extract
package fonts //;import acyclic.file

// import ammonite.ops._

// import slick.driver.H2Driver.api._

// import scala.concurrent._
// import scala.concurrent.duration._
// import db._


// class FontDatabaseApi(dir: Path) extends AbstractDatabase(dir)  {
//   // val log = LoggerFactory.getLogger(this.getClass)

//   import DBIOX._
//   val D = FontDatabaseTables
//   import ExecutionContext.Implicits.global


//   def updateSpline(s: GlyphProp.SplineSet): DBIO[Either[D.GlyphHash, D.GlyphHash]] = {
//     val glyphHashStr = GlyphProp.splineSetHash(s).toString
//     D.GlyphHashes.upsert(glyphHashStr)
//   }

//   // def addGlyphHashes(glyphs: Seq[SplineFont.Glyph]): DBIO[Seq[Either[D.GlyphHash, D.GlyphHash]]] = {
//   def addGlyphHashes(glyphs: Seq[SplineFont.Glyph]): DBIO[Seq[D.GlyphHash]] = {
//     DBIOX.sequence{
//       for {
//         glyph   <- glyphs
//         splineSet <- glyph.get[GlyphProp.SplineSet]
//       } yield for {
//         sps <- updateSpline(splineSet).map{ _ match {
//           case Left(e) => e
//           case Right(e) => e
//         }}
//       } yield sps
//     }
//   }

//   def addFontSubsetDir(fontDir: SplineFont.Dir): DBIO[(D.Font, D.FontSubset)] = {
//     for {
//       fontSubset     <- D.FontSubsets.ccQuery += D.FontSubset()
//       font           <- D.Fonts.ccQuery += D.Font()
//       _              <- D.FontToFontSubset.addEdge(font, fontSubset)
//       fontSubsetPath <- D.CorpusUrls.ccQuery += D.CorpusUrl(fontDir.path.toString())
//       _              <- D.FontSubsetToPath.addEdge(fontSubset, fontSubsetPath)
//       glyphHashes    <- addGlyphHashes(fontDir.glyphs)
//       glyphs         <- sequence{
//         for {
//           (fontGlyph, glhash) <- (fontDir.glyphs.filter(_.get[GlyphProp.SplineSet].isDefined) zip glyphHashes)
//         } yield for {
//           glyph     <- D.Glyphs.ccQuery += D.Glyph()

//           _ <- seq({
//             fontGlyph.path.map ({ p =>
//               for {
//                 glyphPath <- D.CorpusUrls.ccQuery += D.CorpusUrl(p.toString())
//                 _         <- D.GlyphToGlyphPath.addEdge(glyph, glyphPath)
//               } yield ()
//             }).getOrElse (
//               successful(())
//             )
//           })

//           _         <- D.GlyphHashToGlyph.addEdge(glhash, glyph)
//           _         <- D.FontSubsetToGlyph.addEdge(fontSubset, glyph)
//         } yield glyph
//       }
//       _                  = println(s"""    Added font subset dir ${fontSubset} -> ${font} with ${glyphs.length} glyphs""")
//     } yield (font, fontSubset)
//   }

//   def mergeFonts(fonts: Seq[D.Font]): DBIO[Unit] = {
//     for {
//       allFontSubsets   <- sequence{ fonts.map{ D.Fonts.selectFontSubsets(_) } }
//       uniqFontSubsets   = allFontSubsets.flatten.toSet.toList
//       sortedFontSubsets = uniqFontSubsets.sortBy(_.id)
//       uniqFonts         = fonts.toSet.toList
//       sortedFonts       = uniqFonts.sortBy(_.id)
//       _ <- {
//         sortedFonts.headOption.map{ canonicalFont =>
//           for {
//             _                 <- sequence{ sortedFonts.tail.map(D.Fonts.deleteFont(_)) }
//             _                 <- sequence{ sortedFontSubsets.map(D.FontToFontSubset.addEdge(canonicalFont, _)) }
//           } yield ()
//         } getOrElse successful(())
//       }
//     } yield ()
//   }

//   def addFontDir(fontDir: SplineFont.Dir): Unit = {

//     DBIOX.runAndAwait(database, {
//       for {
//         (newFont, newFontSubset)       <- addFontSubsetDir(fontDir)

//         // Select and merge any font subsets that have overlapping glyphs
//         glyphs            <- D.FontSubsets.selectGlyphs(newFontSubset)
//         glhashes          <- sequence { glyphs.map(D.Glyphs.selectHash(_)) }
//         allGlyphs         <- sequence { glhashes.map(D.Glyphs.forHash(_)) }
//         allFontSubsets    <- sequence { allGlyphs.flatten.map(D.Glyphs.selectFontSubset(_)) }
//         uniqueFontSubsets  = (newFontSubset +: allFontSubsets).toSet.toList
//         allFonts          <- sequence{ uniqueFontSubsets.map{ D.FontSubsets.selectFont(_) } }

//         _                 <- mergeFonts(allFonts)
//       } yield{
//         // if (sortedUniqFonts.size>1) {
//         //   println(s"""    Merged fonts ${sortedUniqFonts.mkString("\n      ", "\n      ", "\n")}""")
//         // }
//         // println(s"""   Created ${newFontSubset} -> ${canonicalFont} with ${glyphs.length} glyphs """)
//         // showFontTree(canonicalFont)
//       }
//     })

//   }

//   def buildGlyphTranslationTable(): Unit = {

//   }

//   def showHashedGlyphs(): Unit = {
//     val glyphHashes = Await.result(database.run(D.GlyphHashes.to[List].result), Duration.Inf)

//     val qq = for {
//       glyphHash <- glyphHashes
//     } yield for {
//       glyphsForHash <- D.Glyphs.forHash(glyphHash)
//       pathsAndSubsets <- sequence { glyphsForHash.map { glyph =>
//         for {
//           glyphPath <- D.Glyphs.selectPath(glyph)
//           fontSubset <- D.Glyphs.selectFontSubset(glyph)
//           fontSubsetPath <- D.FontSubsets.selectPath(fontSubset)
//         } yield (glyphPath, fontSubset, fontSubsetPath)
//       }}
//       _ = {
//         if (glyphsForHash.length>1) {

//           val groupedGlyphs = pathsAndSubsets.groupBy(_._3.url)

//           val loadedFontDirs = groupedGlyphs.map { case group =>
//             group._2.headOption.map{
//               case (glyphPath, fontSubset, fontSubsetPath) =>

//                 val dirProps = SplineFonts.addGlyph(
//                   SplineFonts.loadProps(Path(fontSubsetPath.url)),
//                   Path(glyphPath.url))


//                 group._2.tail.foldLeft(dirProps){
//                   case (acc, (glyphPath, fontSubset, fontSubsetPath)) =>
//                     SplineFonts.addGlyph(acc, (Path(glyphPath.url)))

//                 }
//             } getOrElse {
//               sys.error(s"no glyph.props found for glyph w/hash = ${glyphHash}")
//             }
//           }


//           val glyphProps = for {
//             fontDir <- loadedFontDirs
//             glyph <- fontDir.glyphs
//           } yield {
//             val startChar = glyph.prop[GlyphProp.StartChar].v
//             val width = glyph.prop[GlyphProp.Width].v
//             val encoding = glyph.prop[GlyphProp.Encoding].v
//             // val italic = glyph.prop[GlyphProp.].v
//             // val Array[String](e1, e2, e2) = encoding.trim.split(" ")
//             val enc = encoding.trim.split(" ")
//             val encH = enc(1).toInt.toHexString
//             (startChar.value, width, encH)
//           }
//           val uniqChars = glyphProps.map(_._1).toSet
//           val uniqWidths = glyphProps.map(_._2).toSet
//           val uniqEnc = glyphProps.map(_._3).toSet

//           val weights = loadedFontDirs.map(_.prop[FontProp.Weight].v).toSet

//           val names = loadedFontDirs.map({ d =>
//             val name = d.prop[FontProp.FontName].v
//             if (name.contains('+')) name.dropWhile(_ != '+').drop(1).mkString
//             else name
//           }).toSet

//           val fullNames = loadedFontDirs.map({ d =>
//             val name = d.prop[FontProp.FullName].v
//             if (name.contains('+')) name.dropWhile(_ != '+').drop(1).mkString
//             else name
//           }).toSet

//           val familyNames = loadedFontDirs.map({ d =>
//             val name = d.prop[FontProp.FamilyName].v
//             if (name.contains('+')) name.dropWhile(_ != '+').drop(1).mkString
//             else name
//           }).toSet


//           val ucs = uniqChars.mkString(", ")
//           val ues = uniqEnc.mkString(", ")
//           val uws = uniqWidths.mkString(", ")

//           val rec = s"""${glyphHash.hash} || ${ucs} || ${ues} || ${uws}"""

//           // val info =
//           // s"""|GlyphHash ${glyphHash} (${glyphsForHash.length} glyphs)
//           //     |   chars     : ${uniqChars.mkString(", ")}
//           //     |   enc       : ${uniqEnc.mkString(", ")}
//           //     |   widths    : ${uniqWidths.mkString(", ")}
//           //     |   names     : ${names.toSet.toList.mkString(", ")}
//           //     |   fullnames : ${fullNames.toSet.toList.mkString(", ")}
//           //     |   family    : ${familyNames.toSet.toList.mkString(", ")}
//           //     |   weights   : ${weights.toSet.toList.mkString(", ")}
//           //     |""".stripMargin

//           println(rec)
//         }
//       }
//     } yield {
//       ()
//     }

//     Await.result(database.run(
//       sequence(qq)
//     ), Duration.Inf)

//   }

//   def showFontTree(font: D.Font): Unit = {
//     val qq = for {
//       fontSubsets <- D.Fonts.selectFontSubsets(font)
//       fontSubsetGlyphs <- DBIOX.sequence{ fontSubsets.map{ fontSubset =>
//         for {
//           glyphs <- D.FontSubsets.selectGlyphs(fontSubset)
//           glHashPaths <- DBIOX.sequence{
//             glyphs.map{ glyph => for {
//               hash <- D.Glyphs.selectHash(glyph)
//               path <- D.Glyphs.selectPath(glyph)
//             } yield (glyph, hash, path) }
//           }
//         } yield (fontSubset, glHashPaths)
//       }}
//     } yield fontSubsetGlyphs

//     val res = Await.result(database.run(qq), Duration.Inf)

//     println(s"Font ${font}")

//     for {
//       (fontSubset, glyphInfos) <- res
//     } {
//       println(s"  FontSubset ${fontSubset}")
//       for {
//         (glyph, hash, path) <- glyphInfos
//       } {
//         println(s"""      glyph> f:${fontSubset.id} ${glyph} ${hash} ${path}""")
//       }
//     }
//   }

//   def showFontTrees(): Unit = {

//     val fontList = Await.result(database.run(D.Fonts.to[List].result), Duration.Inf)

//     fontList.map(showFontTree(_))

//   }

//   def reportAll() = {

//     Await.result(
//       database.run((for {
//         _ <- D.Fonts.to[List].result.map(x => println(s"fonts: ${x}"))
//         // _ <- D.FontSubsets.to[List].result.map(x => println(s"font subsets: ${x}"))
//         _ <- D.FontToFontSubset.to[List].result.map({a => println(s"font -> fsubset: ${a}")})
//         _ <- D.FontSubsetToGlyph.to[List].result.map({a => println(s"fsubset -> glyph: ${a}")})
//         _ <- D.GlyphHashToGlyph.to[List].result.map({a => println(s"glyph hash -> glyph: ${a}")})
//         _ <- D.GlyphHashes.to[List].result.map(x => println(s"glyphHash: ${x}"))
//       } yield ())),
//       Duration.Inf
//     )
//   }


// }


//     // val qs = fontDir.props.map { p => p match  {
//     //   // case FontProp.SplineFontDB(v: String)    =>
//     //   // case FontProp.FontName(v: String)        => names += Name(None, v)
//     //   // case FontProp.FullName(v: String)        => names += Name(None, v)
//     //   // case FontProp.FamilyName(v: String)      => families += Family(None, v)
//     //   // case FontProp.Weight(v: String)          =>
//     //   // case FontProp.Copyright(v: String)       =>
//     //   // case FontProp.Version(v: String)         =>
//     //   // case FontProp.ItalicAngle(v: Double)     =>
//     //   // case FontProp.UnderlinePosition(v: Int)  =>
//     //   // case FontProp.UnderlineWidth(v: Int)     =>
//     //   // case FontProp.Ascent(v: Int)             =>
//     //   // case FontProp.Descent(v: Int)            =>

//     //   case _                              => DBIOX.noop
//     // }}

//     // val ps = fontDir.glyphs.flatMap { gl => gl.props.map{ pr => pr match {
//     //   case s: GlyphProp.SplineSet         => glyphHashes += GlyphHash(None, GlyphProp.splineSetHash(s).toString)
//     //   // case GlyphProp.StartChar(glyphName) =>
//     //   // case GlyphProp.Encoding(other)      =>
//     //   // case GlyphProp.Width(other)         =>
//     //   // case GlyphProp.Flags(other)         =>
//     //   // case GlyphProp.HStem(other)         =>
//     //   // case GlyphProp.VStem(other)         =>
//     //   // case GlyphProp.LayerCount(other)    =>
//     //   case _                              => DBIOX.noop
//     // }}}
