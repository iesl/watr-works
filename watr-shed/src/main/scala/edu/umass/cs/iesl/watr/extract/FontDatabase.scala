package edu.umass.cs.iesl.watr
package extract

import org.slf4j.LoggerFactory
import scala.concurrent._
import ammonite.ops._


import com.zaxxer.hikari.HikariDataSource
import slick.driver.H2Driver.api._

import scala.concurrent._
import scala.concurrent.duration._

object FontDatabase {
  val log = LoggerFactory.getLogger(this.getClass)

  final case class FqnSymbol(
      id: Option[Int],
      file: String,
      source: Option[String],
      line: Option[Int],
      offset: Option[Int] = None
  )


  private class FqnSymbols(tag: Tag) extends Table[FqnSymbol](tag, "FQN_SYMBOLS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def file = column[String]("file")
    def source = column[Option[String]]("source handle")
    def line = column[Option[Int]]("line in source")
    def offset = column[Option[Int]]("offset in source")
    def * = (id.?, file, source, line, offset) <> (FqnSymbol.tupled, FqnSymbol.unapply)

    def fileIdx = index("idx_file", file, unique = false) // FASTER DELETES
    // def filename = foreignKey("filename_fk", file, fileChecks)(_.filename, onDelete = ForeignKeyAction.Cascade)
  }

  private val fqnSymbols = TableQuery[FqnSymbols]
  private val fqnSymbolsCompiled = Compiled { TableQuery[FqnSymbols] }

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

  if (!exists(dir)) {
    log.info("creating the search database...")
    mkdir(dir)
    Await.result(
      db.run(
        (fqnSymbols.schema).create
      ),
      Duration.Inf
    )
    log.info("... created the search database")
  }

  // file with last modified time
  // def knownFiles(): Future[Seq[FileCheck]] = db.run(fileChecks.result)

  // def removeFiles(files: List[FileObject]): Future[Int] =
  //   db.run {
  //     val restrict = files.map(_.getName.getURI)
  //     // Deletion from fqnSymbols relies on fk cascade delete action
  //     fileChecks.filter(_.filename inSetBind restrict).delete
  //   }

  // private val timestampsQuery = Compiled {
  //   filename: Rep[String] => fileChecks.filter(_.filename === filename).take(1)
  // }

  // def outOfDate(f: FileObject)(implicit vfs: EnsimeVFS): Future[Boolean] = {
  //   val uri = f.getName.getURI
  //   val modified = f.getContent.getLastModifiedTime

  //   db.run(
  //     for {
  //       check <- timestampsQuery(uri).result.headOption
  //     } yield check.map(_.changed).getOrElse(true)
  //   )
  // }

  // def persist(check: FileCheck, symbols: Seq[FqnSymbol]): Future[Int] =
  //   if (symbols.isEmpty) Future.successful(0)
  //   else {
  //     val batches = symbols.grouped(10000)
  //     db.run(
  //       (fileChecksCompiled += check)
  //     ) flatMap { _ =>
  //         val foo = batches.map { batch => db.run(fqnSymbolsCompiled ++= batch) }
  //         Future.sequence(foo).map { inserts => inserts.flatten.sum }
  //       }
  //   }

  // private val findCompiled = Compiled {
  //   fqn: Rep[String] => fqnSymbols.filter(_.fqn === fqn).take(1)
  // }

  // def find(fqn: String): Future[Option[FqnSymbol]] = db.run(
  //   findCompiled(fqn).result.headOption
  // )

  // import org.ensime.indexer.IndexService._
  // def find(fqns: List[FqnIndex]): Future[List[FqnSymbol]] = {
  //   val restrict = fqns.map(_.fqn)
  //   db.run(
  //     fqnSymbols.filter(_.fqn inSet restrict).result
  //   ).map { results =>
  //     val grouped = results.groupBy(_.fqn)
  //     restrict.flatMap(grouped.get(_).map(_.head))
  //   }
  // }
}

object DatabaseService {


  // private class FileChecks(tag: Tag) extends Table[FileCheck](tag, "FILECHECKS") {
  //   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  //   def filename = column[String]("filename")
  //   def timestamp = column[Timestamp]("timestamp")
  //   def * = (id.?, filename, timestamp) <> (FileCheck.tupled, FileCheck.unapply)
  //   def idx = index("idx_filename", filename, unique = true)
  // }
  // private val fileChecks = TableQuery[FileChecks]
  // private val fileChecksCompiled = Compiled(TableQuery[FileChecks])

  // final case class FqnSymbol(
  //     id: Option[Int],
  //     file: String, // the underlying file
  //     path: String, // the VFS handle (e.g. classes in jars)
  //     fqn: String,
  //     internal: Option[String], // for fields
  //     source: Option[String], // VFS
  //     line: Option[Int],
  //     offset: Option[Int] = None // future features:
  // ) {
  //   // this is just as a helper until we can use more sensible
  //   // domain objects with slick
  //   def sourceFileObject(implicit vfs: EnsimeVFS) = source.map(vfs.vfile)

  //   // legacy: note that we can't distinguish class/trait
  //   def declAs: DeclaredAs =
  //     if (fqn.contains("(")) DeclaredAs.Method
  //     else if (internal.isDefined) DeclaredAs.Field
  //     else DeclaredAs.Class
  // }
  // private class FqnSymbols(tag: Tag) extends Table[FqnSymbol](tag, "FQN_SYMBOLS") {
  //   def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  //   def file = column[String]("file")
  //   def path = column[String]("path")
  //   def fqn = column[String]("fqn")
  //   def descriptor = column[Option[String]]("descriptor")
  //   def internal = column[Option[String]]("internal")
  //   def source = column[Option[String]]("source handle")
  //   def line = column[Option[Int]]("line in source")
  //   def offset = column[Option[Int]]("offset in source")
  //   def * = (id.?, file, path, fqn, internal, source, line, offset) <> (FqnSymbol.tupled, FqnSymbol.unapply)
  //   // our FQNs have descriptors, making them unique. but when scala
  //   // aliases use the same namespace we get collisions
  //   def fqnIdx = index("idx_fqn", fqn, unique = false)

  //   def fileIdx = index("idx_file", file, unique = false) // FASTER DELETES
  //   def filename = foreignKey("filename_fk", file, fileChecks)(_.filename, onDelete = ForeignKeyAction.Cascade)
  // }
  // private val fqnSymbols = TableQuery[FqnSymbols]
  // private val fqnSymbolsCompiled = Compiled { TableQuery[FqnSymbols] }
}
