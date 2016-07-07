package edu.umass.cs.iesl.watr
package extract
package fonts

import scala.concurrent._
import ammonite.ops._


import com.zaxxer.hikari.HikariDataSource
import slick.driver.H2Driver.api._

import scala.concurrent._
import scala.concurrent.duration._


abstract class AbstractDatabase(dir: Path)  {
  import FontDatabaseTables._
  import ExecutionContext.Implicits.global

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



  def dropAndRecreateDatabase(): Unit = {
    rmDBDir()
    createDBDir()
  }

}
