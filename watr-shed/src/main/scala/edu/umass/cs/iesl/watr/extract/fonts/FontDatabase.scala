package edu.umass.cs.iesl.watr
package extract
package fonts

import org.slf4j.LoggerFactory
import scala.concurrent._
import ammonite.ops._


import com.zaxxer.hikari.HikariDataSource
import slick.driver.H2Driver.api._
// import slick.driver.H2Driver.DDL

import scala.concurrent._
import scala.concurrent.duration._



object FontDatabase extends EdgeTables {
  val log = LoggerFactory.getLogger(this.getClass)

  //   def filename = foreignKey("filename_fk", file, fileChecks)(_.filename, onDelete = ForeignKeyAction.Cascade)

  final case class Font(id: Int=0) extends Identified

  class Fonts(tag: Tag) extends Table[Font](tag, "FONTS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def * = id <> (Font.apply, Font.unapply)
  }

  object fonts extends TableQuery(new Fonts(_)) {
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
  }

  val fontToHash = oneToMany[Font, SplineHash]

  def schemas = (names.schema ++
    splineHashes.schema ++
    families.schema ++
    fonts.schema ++
    fontToHash.schema
  )

}


class FontDatabase(dir: Path)  {
  import FontDatabase._


  lazy val (datasource, db) = {

    // MVCC plus connection pooling speeds up the tests ~10%
    val backend = "jdbc:h2:file:"
    val url = backend + dir / "db;MVCC=TRUE"
    val driver = "org.h2.Driver"

    val ds = new HikariDataSource()
    ds.setDriverClassName(driver)
    ds.setJdbcUrl(url)
    ds.setMaximumPoolSize(10)
    val threads = ds.getMaximumPoolSize
    val executor = AsyncExecutor("Slick", numThreads = threads, queueSize = -1)
    (ds, Database.forDataSource(ds, executor = executor))
  }

  def commit(): Future[Unit] = Future.successful(())

  // only used for chaining Futures, not for any IO
  import ExecutionContext.Implicits.global

  def shutdown(): Future[Unit] = {
    for {
      // call directly - using slick withSession barfs as it runs a how many rows were updated
      // after shutdown is executed.
      _ <- db.run(sqlu"shutdown")
      _ <- db.shutdown
      _ = datasource.close()
    } yield ()
  }

  def rmDBDir(): Unit = {
    if (exists(dir)) {
      rm(dir)
    }
  }

  def createDBDir(): Unit = {
    if (!exists(dir)) {
      log.info("creating the search database...")
      mkdir(dir)

      Await.result(
        db.run(schemas.create),
        Duration.Inf
      )
      log.info("... created the search database")
    }
  }

  def updateSpline(s: GlyphProp.SplineSet): DBIO[Either[SplineHash, SplineHash]] = {
    val splineHash = SplineHash(GlyphProp.splineSetHash(s).toString)

    splineHashes.ccQuery
      .insertOrUpdate(splineHash)
      .flatMap({ resHash => resHash match {
        case Some(insHash) =>
          DBIOX.successful(Right[SplineHash, SplineHash](insHash))
        case None =>
          DBIOX.successful(Left[SplineHash, SplineHash](resHash.get))
      }})
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




    } else {
      //... otherwise, (no clashes), create a new font entry
      val qq = for {
        f <- fonts.ccQuery += Font()
        _ <- DBIOX.seqs{
          newlyIns.map({hash => fontToHash.addEdge(f, hash) })
        }
      } yield ()

      DBIOX.runAndAwait(db, qq)


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
        _ <- names.to[List].result.map(x => println(s"names: ${x}"))
        _ <- families.to[List].result.map(x => println(s"families: ${x}"))
        _ <- splineHashes.to[List].result.map(x => println(s"splineHash: ${x}"))
        _ <- fontToHash.to[List].result.map({a => println(s"font->hash: ${a}")})
      } yield ())),
      Duration.Inf
    )
  }



  def dropAndRecreateDatabase(): Unit = {
    rmDBDir()
    createDBDir()
  }

}
