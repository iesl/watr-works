package edu.umass.cs.iesl.watr
package extract
package fonts

import org.slf4j.LoggerFactory
import slick.driver.H2Driver.api._

import scala.concurrent._
import ExecutionContext.Implicits.global


object FontDatabaseTables extends EdgeTables {
  val log = LoggerFactory.getLogger(this.getClass)

  import DBIOX._
  import utils.StringCaseUtils._

  def tableName(s: String) = s.toUnderscoreCase.toUpperCase


  /*
   * Schema:
   *    Font
   *      -1-*-> FontSubset
   *               -1-1-> FontSubsetPath::Url (dir)
   *               -1-*-> Glyph
   *                        -1-1-> GlyphPath::Url (file)
   *
   *
   *    GlyphHash
   *      -1-*-> Glyph
   *
   *
   * =========================
   * Operations
   *   Merging FontSubsets under Font
   *   Load a FontSubset from sfd dir.
   *
   */

  final case class Font(id: Int=0) extends Identified

  class Fonts(tag: Tag) extends Table[Font](tag, tableName("Fonts")) {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def * = id <> (Font.apply, Font.unapply)
  }

  object Fonts extends TableQuery(new Fonts(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption


    def selectFontSubsets(font: Font): DBIO[Seq[FontSubset]] = {
      for {
        ids <- FontToFontSubset.selectAdjacentToSrc(font)
        adjs <- sequence{ ids.map{ id =>
          FontSubsets.findById(id)
        }}
      } yield adjs.flatten
    }

  }


  final case class FontSubset(id: Int=0) extends Identified

  class FontSubsets(tag: Tag) extends Table[FontSubset](tag, tableName("FontSubsets")) {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def * = id <> (FontSubset.apply, FontSubset.unapply)
  }

  object FontSubsets extends TableQuery(new FontSubsets(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption

    def selectGlyphs(fontSubset: FontSubset): DBIO[Seq[Glyph]] = {
      for {
        ids <- FontSubsetToGlyph.selectAdjacentToSrc(fontSubset)
        adjs <- sequence{ ids.map{ id =>
          Glyphs.findById(id)
        }}
      } yield adjs.flatten
    }
  }



  final case class CorpusUrl(url: String, id: Int=0) extends Identified
  class CorpusUrls(tag: Tag) extends Table[CorpusUrl](tag, tableName("CorpusUrls")) {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def url = column[String]("url")  //
    def * = (url, id) <> (CorpusUrl.tupled, CorpusUrl.unapply)

    def i0 = index("idx_url", url, unique = true)
  }
  object CorpusUrls extends TableQuery(new CorpusUrls(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  }



  final case class GlyphHash(hash: String, id: Int=0) extends Identified {
    override def toString = s"#${hash.take(3).mkString}"
  }
  class GlyphHashes(tag: Tag) extends Table[GlyphHash](tag, tableName("GlyphHashes")) {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def hash = column[String]("hash")
    def * = (hash, id) <> (GlyphHash.tupled, GlyphHash.unapply)

    def hashIdx = index("idx_hash", hash, unique = true)
  }
  object GlyphHashes extends TableQuery(new GlyphHashes(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
    def findByHash(h: String) = this.findBy(_.hash).apply(h).result.headOption
    def find(id: Int) = this.findBy(_.id).apply(id)

    // returns Left[GlyphHash] if exising entry found, Right[GlyphHash] if newly inserted
    def upsert(glyphHashStr: String): DBIO[Either[GlyphHash, GlyphHash]] = {
      val glyphHash = GlyphHash(glyphHashStr)
      for {
        sh <- GlyphHashes.findByHash(glyphHashStr)
        sp <- sh match {
          case Some(hash) =>
            DBIOX.successful(Left[GlyphHash, GlyphHash](hash))
          case None =>
            (GlyphHashes.ccQuery += glyphHash).map{ z =>
              Right[GlyphHash, GlyphHash](z)
            }
        }
      } yield sp
    }
  }



  final case class Glyph(occurrenceCount: Int=1, id: Int=0) extends Identified

  class Glyphs(tag: Tag) extends Table[Glyph](tag, tableName("Glyphs")) {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def occurrenceCount = column[Int]("occurence_count")
    def * = (occurrenceCount, id) <> (Glyph.tupled, Glyph.unapply)
  }

  object Glyphs extends TableQuery(new Glyphs(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption


    def selectPath(g: Glyph): DBIO[CorpusUrl] = {
      (for {
        ids <- GlyphToGlyphPath.selectAdjacentToSrc(g)
        adjs <- sequence{ ids.map { id =>
          CorpusUrls.findById(id)
        }}
      } yield adjs.flatten
        .headOption
        .getOrElse(sys.error(s"no path found for glyph ${g}")))
    }


    def selectHash(g: Glyph): DBIO[GlyphHash] = {
      (for {
        ids <- GlyphHashToGlyph.selectAdjacentToDst(g)
        adjs <- sequence{ ids.map { id =>
          GlyphHashes.findById(id)
        }}
      } yield adjs
        .flatten
        .headOption
        .getOrElse(sys.error(s"no hash found for glyph ${g}"))
      )
    }
  }


  val FontToFontSubset  = oneToMany[Font, FontSubset]
  val FontSubsetToPath = oneToOne[FontSubset, CorpusUrl]
  val FontSubsetToGlyph = oneToMany[FontSubset, Glyph]
  val GlyphToGlyphPath  = oneToOne[Glyph, CorpusUrl]
  val GlyphHashToGlyph  = oneToMany[GlyphHash, Glyph]


  def schemas = (
    FontToFontSubset.schema ++
      FontSubsetToPath.schema ++
      FontSubsetToGlyph.schema ++
      GlyphToGlyphPath.schema ++
      GlyphHashToGlyph.schema ++
      Fonts.schema ++
      FontSubsets.schema ++
      CorpusUrls.schema ++
      GlyphHashes.schema ++
      Glyphs.schema
  )

}
















  // final case class CharTranslation(trans: String, id: Int=0) extends Identified

  // class CharTranslations(tag: Tag) extends Table[CharTranslation](tag, tableName("CharTranslations")) {
  //   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  //   def trans = column[String]("trans")
  //   def * = (trans, id) <> (CharTranslation.tupled, CharTranslation.unapply)

  //   def i0 = index("idx_trans", trans, unique = false)
  // }

  // object CharTranslations extends TableQuery(new CharTranslations(_)) {
  //   val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
  //   def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  //   def find(trans: String) = this.findBy(_.trans).apply(trans).result.headOption


  //   def upsert(trans: String): DBIO[CharTranslation] = for {
  //     ct <- find(trans).flatMap({
  //       case z@Some(t) => successful(z)
  //       case None => for {
  //         _  <- ccQuery += CharTranslation(trans)
  //         t <- find(trans)
  //       } yield t
  //     })
  //   } yield ct.get
  // }


  // final case class Family(family: String, id: Int=0)
  // class Families(tag: Tag) extends Table[Family](tag, "FAMILIES") {
  //   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  //   def family = column[String]("family")
  //   def * = (family, id) <> (Family.tupled, Family.unapply)
  //   def familyIdx = index("idx_family", family, unique = true)
  // }
  // object families extends TableQuery(new Families(_)) {
  //   val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
  //   def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  // }

  // final case class Name(name: String, id: Int=0)
  // class Names(tag: Tag) extends Table[Name](tag, "NAME") {
  //   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  //   def name = column[String]("name")
  //   def * = (name, id) <> (Name.tupled, Name.unapply)
  // }
  // object names extends TableQuery(new Names(_)) {
  //   val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
  //   def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  // }


// def selectCharTranslations(g: Glyph): DBIO[Seq[CharTranslation]] = {
//   (for {
//     ids <- glyphToChar.selectAdjacentToSrc(g)
//     adjs <- sequence{ ids.map { id =>
//       CharTranslations.findById(id)
//     }}
//   } yield adjs.flatten)
// }
