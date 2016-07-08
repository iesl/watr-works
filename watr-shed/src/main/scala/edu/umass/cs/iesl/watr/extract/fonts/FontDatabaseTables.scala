package edu.umass.cs.iesl.watr
package extract
package fonts

import org.slf4j.LoggerFactory
import slick.driver.H2Driver.api._

import scala.concurrent._
// import scala.concurrent.duration._
import ExecutionContext.Implicits.global


object FontDatabaseTables extends EdgeTables {
  val log = LoggerFactory.getLogger(this.getClass)
  import DBIOX._

  //   def filename = foreignKey("filename_fk", file, fileChecks)(_.filename, onDelete = ForeignKeyAction.Cascade)

  final case class Font(id: Int=0) extends Identified

  class Fonts(tag: Tag) extends Table[Font](tag, "FONTS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def * = id <> (Font.apply, Font.unapply)
  }

  object fonts extends TableQuery(new Fonts(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption


    def selectGlyphs(font: Font): DBIO[Seq[Glyph]] = {
      for {
        ids <- fontToGlyph.selectAdjacentToSrc(font)
        adjs <- sequence{ ids.map{ id =>
          glyphs.findById(id)
        }}
      } yield adjs.flatten
    }

  }


  final case class CorpusUrl(url: String, id: Int=0) extends Identified

  class CorpusUrls(tag: Tag) extends Table[CorpusUrl](tag, "CORPUS_URLS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def url = column[String]("family")  // unique key
    def * = (url, id) <> (CorpusUrl.tupled, CorpusUrl.unapply)

    def i0 = index("idx_url", url, unique = true)
  }

  object corpusUrls extends TableQuery(new CorpusUrls(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  }








  final case class CorpusUrl(url: String, id: Int=0) extends Identified

  class CorpusUrls(tag: Tag) extends Table[CorpusUrl](tag, "CORPUS_URLS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def url = column[String]("family")  // unique key
    def * = (url, id) <> (CorpusUrl.tupled, CorpusUrl.unapply)

    def i0 = index("idx_url", url, unique = true)
  }

  object corpusUrls extends TableQuery(new CorpusUrls(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  }



  final case class Family(family: String, id: Int=0)

  class Families(tag: Tag) extends Table[Family](tag, "FAMILIES") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def family = column[String]("family")  // unique key
    def * = (family, id) <> (Family.tupled, Family.unapply)

    def familyIdx = index("idx_family", family, unique = true)
  }

  object families extends TableQuery(new Families(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  }


  final case class Name(name: String, id: Int=0)

  class Names(tag: Tag) extends Table[Name](tag, "NAME") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")  // unique key
    def * = (name, id) <> (Name.tupled, Name.unapply)
  }

  object names extends TableQuery(new Names(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  }


  final case class SplineHash(hash: String, id: Int=0) extends Identified {
    override def toString = s"#${hash.take(3).mkString}"
  }

  class SplineHashes(tag: Tag) extends Table[SplineHash](tag, "SPLINEHASHES") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def hash = column[String]("hash")
    def * = (hash, id) <> (SplineHash.tupled, SplineHash.unapply)

    def hashIdx = index("idx_hash", hash, unique = true)
  }

  object splineHashes extends TableQuery(new SplineHashes(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
    def findByHash(h: String) = this.findBy(_.hash).apply(h).result.headOption
    def find(id: Int) = this.findBy(_.id).apply(id)
  }

  final case class Glyph(
    occurrenceCount: Int=1,
    id: Int=0
  ) extends Identified

  class Glyphs(tag: Tag) extends Table[Glyph](tag, "GLYPHS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def occurrenceCount = column[Int]("occurence_count")
    def * = (occurrenceCount, id) <> (Glyph.tupled, Glyph.unapply)

  }

  object glyphs extends TableQuery(new Glyphs(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption

    def selectSfdUrls(g: Glyph): DBIO[Seq[CorpusUrl]] = {
      (for {
        ids <- glyphToSfd.selectAdjacentToSrc(g)
        adjs <- sequence{ ids.map { id =>
          corpusUrls.findById(id)
        }}
      } yield adjs.flatten)
    }


    def selectHash(g: Glyph): DBIO[Option[SplineHash]] = {
      (for {
        ids <- glyphToHash.selectAdjacentToSrc(g)
        adjs <- sequence{ ids.map { id =>
          splineHashes.findById(id)
        }}
      } yield adjs.headOption.flatten)
    }
  }


  val fontToGlyph = oneToMany[Font, Glyph]
  val glyphToHash = oneToOne[Glyph, SplineHash]
  val glyphToSfd = oneToMany[Glyph, CorpusUrl]


  def schemas = (
    names.schema ++
      splineHashes.schema ++
      families.schema ++
      fonts.schema ++
      glyphs.schema ++
      fontToGlyph.schema ++
      glyphToHash.schema ++
      glyphToSfd.schema
  )

}
